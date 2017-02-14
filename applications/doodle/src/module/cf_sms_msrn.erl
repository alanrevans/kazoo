%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_sms_msrn).

-include("../doodle.hrl").

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
handle(Data, Call1) ->
    case build_endpoint(Data, Call1) of
        {'error', 'do_not_disturb'} = Reason ->
            maybe_handle_bridge_failure(Reason, Call1);
        {'error', Reason} ->
            doodle_exe:continue(doodle_util:set_flow_error(<<"error">>, wh_util:to_binary(Reason), Call1));
        {Endpoints, Call} ->
            case whapps_sms_command:b_send_sms(Endpoints, Call) of
                {'ok', JObj} -> handle_result(JObj, Call);
                {'error', _} = Reason -> maybe_handle_bridge_failure(Reason, Call)
            end
    end.

-spec handle_result(wh_json:object(), whapps_call:call()) -> 'ok'.
handle_result(JObj, Call) ->
    Status = doodle_util:sms_status(JObj),
    Call1 = doodle_util:set_flow_status(Status, Call),
    handle_result_status(Call1, Status).

-spec handle_result_status(whapps_call:call(), ne_binary()) -> 'ok'.
handle_result_status(Call, <<"pending">>) ->
    doodle_util:maybe_reschedule_sms(Call);
handle_result_status(Call, _Status) ->
    lager:info("completed successful message to the device"),
    doodle_exe:continue(Call).

-spec maybe_handle_bridge_failure({'error', any()}, whapps_call:call()) -> 'ok'.
maybe_handle_bridge_failure({_ , R}=Reason, Call) ->
    case doodle_util:handle_bridge_failure(Reason, Call) of
        'not_found' ->
            doodle_util:maybe_reschedule_sms(
              doodle_util:set_flow_status(<<"pending">>, wh_util:to_binary(R), Call));
        'ok' -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to build the endpoints to reach this device
%% @end
%%--------------------------------------------------------------------
build_endpoint(_Data, Call) ->
    Callee_number = whapps_call:callee_id_number(Call),
    {ok, Info} = ss7c_if:get_subscriber_info(<<"msrn">>, wnm_util:to_e164(Callee_number), rest),
    Imsi = proplists:get_value(<<"imsi">>, Info),
    Msisdn = proplists:get_value(<<"msisdn">>, Info),
    Endpoints = [{[{<<"Invite-Format">>,<<"username">>},
                 {<<"To-User">>, Imsi},
                 {<<"To-Username">>, Imsi},
                 {<<"To-Realm">>, <<"1a-ganx.altilink.ganx">>},
                 {<<"Endpoint-Type">>, <<"sip">>},
%%                 {<<"To-DID">>,<<"+4470000001">>},
                 {<<"SIP-Interface">>,<<"sofia/umainterface_1/">>},
%%                 {<<"Callee-ID-Name">>,<<"Jean-Francois Gault2">>},
                 {<<"Callee-ID-Number">>, Msisdn},
%%                 {<<"Outbound-Callee-ID-Name">>,<<"Jean-Francois Gault2">>},
                 {<<"Outbound-Callee-ID-Number">>, Msisdn},
%%                 {<<"Ignore-Early-Media">>,<<"true">>},
                 {<<"Endpoint-Timeout">>,<<"30">>}
%%                 {<<"Endpoint-ID">>,<<"994d5941ecbf56dda7fe6cc252b530e8">>},
%%                 {<<"Codecs">>,[<<"AMR">>,<<"PCMA">>]}
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
    {Endpoints, Call}.
