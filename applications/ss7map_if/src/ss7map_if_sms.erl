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
    ok = amqp_util:bind_q_to_exchange(Q, <<"kazoo">>, <<"hlr_sms_d_x">>),
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
