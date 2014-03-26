%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Our connection to AMQP and how we handle what payloads we want to
%%% receive, and what module/functions should handle those payloads
%%% when received.
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ss7map_if_sms).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
         ,terminate/2, code_change/3]).

-include("ss7map_if.hrl").

-record(state, {}).


-define(QUEUE_NAME, <<"hlr_sms_d_x">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ok = amqp_util:new_exchange(<<"hlr_sms_d_x">>, <<"topic">>),
    Q = amqp_util:new_queue(<<>>, [{exclusive, true}]),
    ok = amqp_util:bind_q_to_exchange(Q, <<"kazoo.*">>, <<"hlr_sms_d_x">>),
    ok = amqp_util:basic_consume(Q),
    {ok, #state{}}.


%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
handle_info({#'basic.deliver'{exchange = <<"hlr_sms_d_x">>, 
                              routing_key =  <<"kazoo.", IMSI/binary>>,
                              delivery_tag = Tag},
             #amqp_msg{props = #'P_basic'{reply_to = ReplyQ,
                                          correlation_id = CorrelId},
                                          payload = Payload}},
             State) ->

    lager:debug("~p:~p  #'basic.deliver' reply_to:~p correlation_id:~p payload:~p~n", [?MODULE, self(), ReplyQ, CorrelId, Payload]),
    case  lookup_contact(IMSI, <<"sip.kagesys.com">>) of
        {ok, Contact} ->
            {From, Body} =  decode(Payload),
            Header = [
               {"profile", "sip_clients"}
              ,{"content-length", wh_util:to_list(size(Body))}
              ,{"content-type", "text/plain"}
              ,{"to", <<"sip:", IMSI/binary, "@sip.kagesys.com">>}
              ,{"from", <<"sip:", From/binary, "@sip.kagesys.com">>}
              ,{"contact", Contact}
              ,{"body", Body}
            ],
            Resp = freeswitch:sendevent('freeswitch@kazoo.kagesys.com', 'SEND_MESSAGE', Header),
            lager:debug("sent SIP/SIMPLE Msg to '~s': ~p", [IMSI, Resp]);
        _Else ->
            lager:debug("drop SIP/SIMPLE Msg '~s' not registered", [IMSI])
    end, 
    amqp_util:basic_publish(<<>>, ReplyQ, <<"ok">>, <<"application/json">>,[{correlation_id, CorrelId}]),
    {noreply, State};

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).


%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
lookup_contact(User, Realm) ->
    Req = [{<<"Username">>, User}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Contact">>]}
           | wh_api:default_headers(<<"ss7map_if">>, <<"1.0">>)
          ],
    ReqResp = whapps_util:amqp_pool_collect(Req
                                            ,fun wapi_registration:publish_query_req/1
                                            ,'ecallmgr'
                                            ,2000
                                           ),
    case  ReqResp of
        {'ok', JObjs} ->
            case [Contact
                  || JObj <- JObjs
                         ,wapi_registration:query_resp_v(JObj)
                         ,(Contact = wh_json:get_value([<<"Fields">>, 1, <<"Contact">>]
                                                       ,JObj)) =/= 'undefined'
                 ]
            of
                [Contact|_] ->
                    lager:info("fetched user ~s@~s contact ~s", [User, Realm, Contact]),
                    {'ok', Contact};
                _Else ->
                    lager:info("contact query for user ~s@~s returned an empty result", [User, Realm]),
                    {'error', 'not_found'}
            end;
        _Else ->
            lager:info("contact query for user ~s@~s failed: ~p", [User, Realm, _Else]),
            {'error', 'not_found'}
    end.

decode(Payload) ->
    <<TpRp:1, TpUdhi:1, TpSri:1, _:2, TpMms:1, TpMti:2, TpOA_L0:8, Rest1/binary>> = Payload,
    TpOA_L =  ((TpOA_L0 div 2) + (TpOA_L0 rem 2) + 1) * 8,
    <<TpOA:TpOA_L, TpPid:8, TpDcs:8, TpScTs:(7*8), TpUserData_L:8, TpUserData/binary>> = Rest1,
    {list_to_binary(from_bcd(<<TpOA:TpOA_L>>)), list_to_binary(sms_encoding:from_7bit(TpUserData))}.

from_bcd(<<16#91,V/binary>>) ->                 %% Convert 9144... to +44....
    from_bcd(V, [$+]);

from_bcd(V) when is_binary(V) ->
    from_bcd(V, []).

from_bcd(<<>>, Acc) ->
    Acc;

from_bcd(V, Acc) ->
    <<A:4, B:4, Rest/binary>> = V,
    case A of
        16#0F ->
            from_bcd(Rest, Acc ++ [B+$0]);
        _ ->
            from_bcd(Rest, Acc ++ [B+$0, A+$0])
    end.

