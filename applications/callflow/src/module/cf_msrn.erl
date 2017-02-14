%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_msrn).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case bridge_to_endpoints(Data, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the device"),
            cf_exe:stop(Call);
        {'fail', _}=Reason -> maybe_handle_bridge_failure(Reason, Call);
        {'error', _R} ->
            lager:info("error bridging to device: ~s"
                       ,[wh_json:get_value(<<"Error-Message">>, _R)]
                      ),
            cf_exe:continue(Call)
    end.

-spec maybe_handle_bridge_failure(any(), whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure(Reason, Call) ->
    case cf_util:handle_bridge_failure(Reason, Call) of
        'not_found' -> cf_exe:continue(Call);
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to bridge to the endpoints created to reach this device
%% @end
%%--------------------------------------------------------------------
-spec bridge_to_endpoints(wh_json:object(), whapps_call:call()) ->
                                 cf_api_bridge_return().
bridge_to_endpoints(Data, Call) ->
    Callee_number = whapps_call:callee_id_number(Call),
    {ok, Info} = ss7c_if:get_subscriber_info(<<"msrn">>, wnm_util:to_e164(Callee_number), rest),
    Imsi = proplists:get_value(<<"imsi">>, Info),
    Msisdn = proplists:get_value(<<"msisdn">>, Info),
    Endpoints = [{[{<<"Invite-Format">>,<<"username">>},
                 {<<"To-User">>, Imsi},
                 {<<"To-Username">>, Imsi},
                 {<<"To-Realm">>, <<"sip.altilink.ganx">>},
%%                 {<<"To-DID">>,<<"+4470000001">>},
                 {<<"SIP-Interface">>,<<"sofia/umainterface_1/">>},
%%                 {<<"Callee-ID-Name">>,<<"Jean-Francois Gault2">>},
                 {<<"Callee-ID-Number">>, Msisdn},
%%                 {<<"Outbound-Callee-ID-Name">>,<<"Jean-Francois Gault2">>},
                 {<<"Outbound-Callee-ID-Number">>, Msisdn},
                 {<<"Ignore-Early-Media">>,<<"true">>},
                 {<<"Endpoint-Timeout">>,<<"30">>},
%%                 {<<"Endpoint-ID">>,<<"994d5941ecbf56dda7fe6cc252b530e8">>},
                 {<<"Codecs">>,[<<"AMR">>,<<"PCMA">>]}
%%                 {<<"Presence-ID">>,<<"208220000000002@sip.altilink.ganx">>},
%%                 {<<"Custom-Channel-Vars">>,
%%                 {[{<<"SIP-Invite-Domain">>,<<"sip.altilink.ganx">>},
%%                   {<<"Media-Encryption-Enforce-Security">>,false},
%%                   {<<"Account-ID">>,<<"6ff4f0ab8bb5abd7c18d17cc65001312">>},
%%                   {<<"Owner-ID">>,<<"6c4dcfca439ea4b624739571a4633c6c">>},
%%                   {<<"Authorizing-ID">>,<<"994d5941ecbf56dda7fe6cc252b530e8">>}]}},
%%                   {<<"Ignore-Completed-Elsewhere">>,true}
                  ]}
                ],
    %% Release the MSRN : Don't care too much if it fails
    spawn(ss7c_if, update_vlr, [<<"imsi">>, Imsi, <<"msrn">>, <<"undefined">>, rest]),
    
    Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
    IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
    whapps_call_command:b_bridge(Endpoints, Timeout, <<"simultaneous">>, IgnoreEarlyMedia, Call).
