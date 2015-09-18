%%%-------------------------------------------------------------------
%%% @copyright (C) 2012 VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ss7map_if).

-export([start/0, start_link/0, stop/0]).

-export([submit_sms/3]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    _ = start_deps(),
    ss7map_if_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    application:start(ss7map_if).

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(ss7map_if).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    [wh_util:ensure_started(App) || App <- [sasl, crypto, whistle_amqp]].


submit_sms(Imsi, SmsMsc, SmsMsg) ->
    Props = [{<<"imsi">>, Imsi},{<<"smsmsc">>, SmsMsc}, {<<"smsmsg">>, SmsMsg}],
    case whapps_util:amqp_pool_request(Props
                              ,fun wapi_hlr:publish_submit_sms_req/1
                              ,fun wapi_hlr:submit_sms_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} ->
            case  wh_json:get_value([<<"value">>,<<"result">>], RespJObj) of
                <<"accept">> -> ok;
                Else -> {error, Else}
            end;
        {error, _} = Err -> Err
    end.

