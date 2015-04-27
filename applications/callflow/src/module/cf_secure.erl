%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_secure).

-include("../callflow.hrl").

-export([handle/2]).

-define(DEFAULT_EVENT_WAIT, 10000).

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
    case  wh_json:get_ne_value(<<"mode">>, Data) of
        <<"uma">> -> handle_uma(Data, Call);
        <<"gsm">> -> handle_gsm(Data, Call);
        <<"sms">> -> handle_sms(Data, Call);
        <<"sms_presence">> -> handle_sms_presence(Data, Call);
        _ -> cf_exe:stop(Call)
    end.

handle_uma(Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    AccountDb = whapps_call:account_db(Call),
    ViewOptions = [{<<"key">>, <<$+, CaptureGroup/binary>>}],
    case couch_mgr:get_results(AccountDb, <<"devices/listing_by_msisdn">>, ViewOptions) of
        {ok, [RespJObj]} ->
            maybe_bridge_endpoints(wh_json:merge_jobjs(Data, RespJObj), Call);
        _Else ->
            lager:info("failed to secure number ~s", [CaptureGroup]),
            _ = whapps_call_command:play(<<"fault-can_not_be_completed_as_dialed">>, Call),
            cf_exe:stop(Call)
    end.

handle_gsm(_Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    NewCall = whapps_call:set_request(<<$+, CaptureGroup/binary, "@sip.kagesys.com">>, Call),
    NewFlow = wh_json:from_list([{<<"data">>,<<>>},{<<"module">>,<<"resources">>},{<<"children">>,<<>>}]),
    lager:info("call diverting to ~s", [CaptureGroup]),
    branch(NewFlow, NewCall).

handle_sms(Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    To = <<$+, CaptureGroup/binary>>,
    From = whapps_call:from(Call),
    Name = whapps_call:caller_id_name(Call),
    Body = <<Name/binary, " is trying to reach you via the satcom1 secure UMA service">>,
    Node = node(),
    Props = [{<<"from">>, From}
             ,{<<"to">>, To} 
             ,{<<"body">>, Body}],
    ecallmgr_ss7map_if:submit_sms(Props, Node),
    cf_exe:stop(Call).

handle_sms_presence(Data, Call) ->
    {error, not_yet}.

maybe_bridge_endpoints(Data, Call) ->
    User = wh_json:get_ne_value([<<"value">>,<<"imsi">>], Data),
    Realm =  whapps_call:request_realm(Call),
    Req = [{<<"Username">>, User}
           ,{<<"Realm">>, Realm}
           ,{<<"Fields">>, [<<"Contact">>]}
           | wh_api:default_headers(<<"cf_secure">>, <<"1.0">>)
          ],
    ReqResp = whapps_util:amqp_pool_collect(Req
                                            ,fun wapi_registration:publish_query_req/1
                                            ,'ecallmgr'
                                            ,2000
                                           ),
    case  ReqResp of
        {'ok', JObjs} ->
            case [Contact
                  || J <- JObjs
                         ,wapi_registration:query_resp_v(J)
                         ,(Contact = wh_json:get_value([<<"Fields">>, 1, <<"Contact">>]
                                                       ,J)) =/= 'undefined'
                 ]
            of
                [Contact|_] ->
                    lager:info("fetched user ~s@~s contact ~s", [User, Realm, Contact]),
                    bridge_endpoints(Data, Call);
                _Else ->
                    lager:info("user ~s@~s not currently registered", [User, Realm]),
                    cf_exe:continue(Call)
            end;
        _Else ->
            lager:info("contact query for user ~s@~s failed: ~p", [User, Realm, _Else]),
            _ = whapps_call_command:play(<<"fault-can_not_be_completed_at_this_time">>, Call),
            {'error', 'not_found'}
    end.

bridge_endpoints(Data, Call) ->
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

-spec maybe_handle_bridge_failure(_, whapps_call:call()) -> 'ok'.
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
    EndpointId = wh_json:get_value(<<"id">>, Data),
    Params = wh_json:set_value(<<"source">>, ?MODULE, Data),
    {CID, _} = get_caller_id(Data, Call),
    C2 = set_caller_id(CID, Call),
    case cf_endpoint:build(EndpointId, Params, C2) of
        {'error', _}=E -> E;
        {'ok', Endpoints} ->
            Timeout = wh_json:get_integer_value(<<"timeout">>, Data, ?DEFAULT_TIMEOUT_S),
            IgnoreEarlyMedia = cf_util:ignore_early_media(Endpoints),
            whapps_call_command:b_bridge(Endpoints, Timeout, <<"simultaneous">>, IgnoreEarlyMedia, C2)
    end.

branch(Data, Call) ->
    'ok' = wapi_offnet_resource:publish_req(build_offnet_request(Data, Call)),
    case wait_for_stepswitch(Call) of
        {<<"SUCCESS">>, _} ->
            lager:info("completed successful offnet request"),
            cf_exe:stop(Call);
        {Cause, Code} -> handle_bridge_failure(Cause, Code, Call)
    end.

-spec handle_bridge_failure(api_binary(), api_binary(), whapps_call:call()) -> 'ok'.
handle_bridge_failure(Cause, Code, Call) ->
    lager:info("offnet request error, attempting to find failure branch for ~s:~s", [Code, Cause]),
    case cf_util:handle_bridge_failure(Cause, Code, Call) of
        'ok' -> lager:debug("found bridge failure child");
        'not_found' ->
            cf_util:send_default_response(Cause, Call),
            cf_exe:stop(Call)
    end.

-spec build_offnet_request(wh_json:object(), whapps_call:call()) -> 'ok'.
build_offnet_request(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = get_caller_id(Data, Call),
    props:filter_undefined([{<<"Resource-Type">>, <<"audio">>}
                            ,{<<"Application-Name">>, <<"bridge">>}
                            ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
                            ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
                            ,{<<"Outbound-Caller-ID-Name">>, CIDName}
                            ,{<<"Outbound-Caller-ID-Number">>, CIDNumber}
                            ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
                            ,{<<"Call-ID">>, cf_exe:callid(Call)}
                            ,{<<"Control-Queue">>, cf_exe:control_queue(Call)}
                            ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
                            ,{<<"Account-ID">>, whapps_call:account_id(Call)}
                            ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
                            ,{<<"Media">>, wh_json:get_value(<<"Media">>, Data)}
                            ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
                            ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
                            ,{<<"Format-From-URI">>, wh_json:is_true(<<"format_from_uri">>, Data)}
                            ,{<<"Hunt-Account-ID">>, get_hunt_account_id(Data, Call)}
                            ,{<<"Flags">>, get_flags(Data, Call)}
                            ,{<<"Ignore-Early-Media">>, get_ignore_early_media(Data)}
                            ,{<<"Force-Fax">>, get_force_fax(Call)}
                            ,{<<"SIP-Headers">>,get_sip_headers(Data, Call)}
                            ,{<<"To-DID">>, get_to_did(Data, Call)}
                            ,{<<"From-URI-Realm">>, get_from_uri_realm(Data, Call)}
                            ,{<<"Bypass-E164">>, get_bypass_e164(Data)}
                            | wh_api:default_headers(cf_exe:queue_name(Call), ?APP_NAME, ?APP_VERSION)
                           ]).

-spec get_bypass_e164(wh_json:object()) -> boolean().
get_bypass_e164(Data) ->
    wh_json:is_true(<<"do_not_normalize">>, Data)
        orelse wh_json:is_true(<<"bypass_e164">>, Data).

-spec get_from_uri_realm(wh_json:object(), whapps_call:call()) -> api_binary().
get_from_uri_realm(Data, Call) ->
    case wh_json:get_ne_value(<<"from_uri_realm">>, Data) of
        'undefined' -> maybe_get_call_from_realm(Call);
        Realm -> Realm
    end.

-spec maybe_get_call_from_realm(whapps_call:call()) -> api_binary().
maybe_get_call_from_realm(Call) ->
    case whapps_call:from_realm(Call) of
        'undefined' -> get_account_realm(Call);
        Realm -> Realm
    end.

-spec get_account_realm(whapps_call:call()) -> api_binary().
get_account_realm(Call) ->
    AccountId = whapps_call:account_id(Call),
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} -> wh_json:get_value(<<"realm">>, JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_caller_id(wh_json:object(), whapps_call:call()) -> api_binary().
get_caller_id(Data, Call) ->
    Type = wh_json:get_value(<<"caller_id_type">>, Data, <<"external">>),
    cf_attributes:caller_id(Type, Call).

set_caller_id(<<$+, CID/binary>>, Call) ->
    whapps_call:set_caller_id_number(<<$*, CID/binary, $*>>, Call).

-spec get_hunt_account_id(wh_json:object(), whapps_call:call()) -> api_binary().
get_hunt_account_id(Data, Call) ->
    case wh_json:is_true(<<"use_local_resources">>, Data, 'true') of
        'false' -> 'undefined';
        'true' ->
            AccountId = whapps_call:account_id(Call),
            wh_json:get_value(<<"hunt_account_id">>, Data, AccountId)
    end.

-spec get_to_did(wh_json:object(), whapps_call:call()) -> ne_binary().
get_to_did(Data, Call) ->
    case wh_json:is_true(<<"do_not_normalize">>, Data) of
        'false' -> whapps_call:request_user(Call);
        'true' ->
            Request = whapps_call:request(Call),
            [RequestUser, _] = binary:split(Request, <<"@">>),
            RequestUser
    end.

-spec get_sip_headers(wh_json:object(), whapps_call:call()) -> api_object().
get_sip_headers(Data, Call) ->
    Routines = [fun(J) ->
                        case wh_json:is_true(<<"emit_account_id">>, Data) of
                            'false' -> J;
                            'true' ->
                                wh_json:set_value(<<"X-Account-ID">>, whapps_call:account_id(Call), J)
                        end
                end
               ],
    CustomHeaders = wh_json:get_value(<<"custom_sip_headers">>, Data, wh_json:new()),
    JObj = lists:foldl(fun(F, J) -> F(J) end, CustomHeaders, Routines),
    case wh_util:is_empty(JObj) of
        'true' -> 'undefined';
        'false' -> JObj
    end.

-spec get_ignore_early_media(wh_json:object()) -> api_binary().
get_ignore_early_media(Data) ->
    wh_util:to_binary(wh_json:is_true(<<"ignore_early_media">>, Data, <<"false">>)).

-spec get_force_fax(whapps_call:call()) -> 'undefined' | boolean().
get_force_fax(Call) ->
    case cf_endpoint:get(Call) of
        {'ok', JObj} -> wh_json:is_true([<<"media">>, <<"fax_option">>], JObj);
        {'error', _} -> 'undefined'
    end.

-spec get_flags(wh_json:object(), whapps_call:call()) -> 'undefined' | ne_binaries().
get_flags(Data, Call) ->
    Routines = [fun get_endpoint_flags/3
                ,fun get_flow_flags/3
                ,fun get_flow_dynamic_flags/3
                ,fun get_endpoint_dynamic_flags/3
                ,fun get_account_dynamic_flags/3
               ],
    lists:foldl(fun(F, A) -> F(Data, Call, A) end, [], Routines).

-spec get_endpoint_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_endpoint_flags(_, Call, Flags) ->
    case cf_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', JObj} ->
            case wh_json:get_value(<<"outbound_flags">>, JObj) of
                'undefined' -> Flags;
                 EndpointFlags -> EndpointFlags ++ Flags
            end
    end.

-spec get_flow_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_flow_flags(Data, _, Flags) ->
    case wh_json:get_value(<<"outbound_flags">>, Data) of
        'undefined' -> Flags;
        FlowFlags -> FlowFlags ++ Flags
    end.

-spec get_flow_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_flow_dynamic_flags(Data, Call, Flags) ->
    case wh_json:get_value(<<"dynamic_flags">>, Data) of
        'undefined' -> Flags;
        DynamicFlags -> process_dynamic_flags(DynamicFlags, Flags, Call)
    end.

-spec get_endpoint_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_endpoint_dynamic_flags(_, Call, Flags) ->
    case cf_endpoint:get(Call) of
        {'error', _} -> Flags;
        {'ok', JObj} ->
            case wh_json:get_value(<<"dynamic_flags">>, JObj) of
                'undefined' -> Flags;
                 DynamicFlags ->
                    process_dynamic_flags(DynamicFlags, Flags, Call)
            end
    end.

-spec get_account_dynamic_flags(wh_json:object(), whapps_call:call(), ne_binaries()) -> ne_binaries().
get_account_dynamic_flags(_, Call, Flags) ->
    DynamicFlags = whapps_account_config:get(whapps_call:account_id(Call)
                                             ,<<"callflow">>
                                             ,<<"dynamic_flags">>
                                             ,[]
                                            ),
    process_dynamic_flags(DynamicFlags, Flags, Call).

-spec process_dynamic_flags(ne_binaries(), ne_binaries(), whapps_call:call()) -> ne_binaries().
process_dynamic_flags([], Flags, _) -> Flags;
process_dynamic_flags([DynamicFlag|DynamicFlags], Flags, Call) ->
    case is_flag_exported(DynamicFlag) of
        'false' -> process_dynamic_flags(DynamicFlags, Flags, Call);
        'true' ->
            Fun = wh_util:to_atom(DynamicFlag),
            process_dynamic_flags(DynamicFlags, [whapps_call:Fun(Call)|Flags], Call)
    end.

-spec is_flag_exported(ne_binary()) -> boolean().
is_flag_exported(Flag) ->
    is_flag_exported(Flag, whapps_call:module_info('exports')).

is_flag_exported(_, []) -> 'false';
is_flag_exported(Flag, [{F, 1}|Funs]) ->
    case wh_util:to_binary(F) =:= Flag of
        'true' -> 'true';
        'false' -> is_flag_exported(Flag, Funs)
    end;
is_flag_exported(Flag, [_|Funs]) -> is_flag_exported(Flag, Funs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on offnet response
%% @end
%%--------------------------------------------------------------------
-spec wait_for_stepswitch(whapps_call:call()) -> {ne_binary(), api_binary()}.
wait_for_stepswitch(Call) ->
    case whapps_call_command:receive_event(?DEFAULT_EVENT_WAIT, 'true') of
        {'ok', JObj} ->
            case wh_util:get_event_type(JObj) of
                {<<"resource">>, <<"offnet_resp">>} ->
                    {wh_json:get_value(<<"Response-Message">>, JObj)
                     ,wh_json:get_value(<<"Response-Code">>, JObj)
                    };
                {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
                    lager:info("recv channel destroy"),
                    {wh_json:get_value(<<"Hangup-Cause">>, JObj)
                     ,wh_json:get_value(<<"Hangup-Code">>, JObj)
                    };
                _ -> wait_for_stepswitch(Call)
            end;
        _ -> wait_for_stepswitch(Call)
    end.

