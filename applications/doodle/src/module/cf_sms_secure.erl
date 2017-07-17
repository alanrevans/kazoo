%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_sms_secure).

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
        {'error', 'absent_subscriber'} = Reason ->
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
            lager:info("message not delivered to the device ~p, maybe_reschedule_sms", [Reason]),
            doodle_util:maybe_reschedule_sms(
                doodle_util:set_flow_status(<<"pending">>, wh_util:to_binary(R), Call));
        'ok' -> 
            lager:warning("message not delivered to the device ~p, not resheduling", [Reason]),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to build the endpoints to reach this device
%% @end
%%--------------------------------------------------------------------
build_endpoint(_Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    Msisdn = <<$+, CaptureGroup/binary>>,
    case  catch ss7c_if:get_subscriber_info(<<"msisdn">>, Msisdn, rest) of
        {ok, Info} ->
            Imsi = proplists:get_value(<<"imsi">>, Info),
            Realm = proplists:get_value(<<"vlr">>, Info),
            Endpoints = [{[{<<"Invite-Format">>,<<"username">>},
                 {<<"To-User">>, Imsi},
                 {<<"To-Username">>, Imsi},
                 {<<"imsi">>, Imsi},
                 {<<"msisdn">>, Msisdn},
                 {<<"To-Realm">>, Realm},
                 {<<"Endpoint-Type">>, <<"sip">>},
                 {<<"SIP-Interface">>,<<"sofia/umainterface_1/">>},
                 {<<"Callee-ID-Number">>, Msisdn},
                 {<<"Outbound-Callee-ID-Number">>, Msisdn},
                 {<<"Endpoint-Timeout">>,<<"30">>}
                  ]}
                ],
            Call2 = whapps_call:kvs_store(<<"target_device_id">>, Imsi, Call),
            {Endpoints, Call2};
        {error,<<"absent_subscriber">>} -> {error, absent_subscriber};
        Else -> Else
    end.

