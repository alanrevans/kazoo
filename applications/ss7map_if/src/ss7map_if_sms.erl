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
    {From, Body} =  decode(Payload),
    Header = [
           {"profile", "sip_clients"}
          ,{"content-length", wh_util:to_list(length(Body))}
          ,{"content-type", "text/plain"}
          ,{"to", <<IMSI/binary, "@sip.kagesys.com">>}
          ,{"from", From ++ "@sip.kagesys.com"}
          ,{"body", Body}
         ],
    Resp = freeswitch:sendevent('freeswitch@kazoo.kagesys.com', 'SEND_MESSAGE', Header),
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
decode(Payload) ->
    <<TpRp:1, TpUdhi:1, TpSri:1, _:2, TpMms:1, TpMti:2, TpOA_L0:8, Rest1/binary>> = Payload,
    TpOA_L =  ((TpOA_L0 div 2) + (TpOA_L0 rem 2) + 1) * 8,
    <<TpOA:TpOA_L, TpPid:8, TpDcs:8, TpScTs:(7*8), TpUserData_L:8, TpUserData/binary>> = Rest1,
    {from_bcd(<<TpOA:TpOA_L>>), sms_encoding:from_7bit(TpUserData)}.

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

