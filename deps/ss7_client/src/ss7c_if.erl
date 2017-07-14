-module(ss7c_if).

-export([auth_info/1, 
         auth_info/2,
         auth_info/3,
         update_location/2, 
         update_location/3,
         imsi_detach/1,
         imsi_detach/2,
         get_subscriber_info/2, 
         get_subscriber_info/3,
         get_cf_info/2, 
         update_vlr/4,
         update_vlr/5,
         get_msisdn/1, 
         get_msisdn/2,
         submit_sms/3, 
         submit_sms/4,
         deliver_sms/3
%%         deliver_sms/4
        ]).

-define(APP_NAME, <<"ss7c">>).
-define(APP_VERSION, <<"0.0.1">>).

auth_info(Imsi) ->
    auth_info(Imsi, 1).

auth_info(Imsi, Count) ->
    auth_info(Imsi, Count, amqp).
    
auth_info(Imsi, Count, Proto) ->
    Props = props:filter_undefined(
         [ 
            {<<"imsi">>, Imsi}
           ,{<<"count">>, list_to_binary(integer_to_list(Count))}
           ,{<<"cmd">>, <<"auth_info">>}
          | wh_api:default_headers(<<"HLR">>, <<"auth_info">>, ?APP_NAME, ?APP_VERSION)
         ]),
    auth_info(Imsi, Count, Proto, Props).

auth_info(Imsi, _Count, amqp, Props) ->
    case  wh_amqp_worker:call(Props
                              ,fun wapi_HLR:publish_auth_info_req/1
                              ,fun wapi_HLR:auth_info_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} ->
            ok = check_response(RespJObj),
            extract_vectors(RespJObj);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end;
auth_info(Imsi, _Count, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(hlr, url),
    Url_b = << Url_prefix/binary, $/, Imsi/binary, <<"/auth_info">>/binary>>,
    case rest_req(Url_b, Props, get) of
    {ok, RespJObj} ->
            ok = check_response(RespJObj),
            extract_vectors(RespJObj);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.

update_location(Imsi, VlrId) ->
    update_location(Imsi, VlrId, amqp).
    
update_location(Imsi, VlrId, Proto) ->
    Props = props:filter_undefined(
                    [ 
                       {<<"imsi">>, Imsi}
                      ,{<<"vlrid">>, list_to_binary(pid_to_list(VlrId))}
                      ,{<<"cmd">>, <<"update_location">>}
                      ,{<<"hostname">>, list_to_binary(net_adm:localhost())}
                     | wh_api:default_headers(<<"HLR">>, <<"update_location">>, ?APP_NAME, ?APP_VERSION)
                    ]),
    update_location(Imsi, VlrId, Proto, Props).

update_location(Imsi, VlrId, amqp, Props) ->
    case wh_amqp_worker:call(Props
                              ,fun wapi_HLR:publish_update_location_req/1
                              ,fun wapi_HLR:update_location_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} -> 
            ok = check_response(RespJObj);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end;
update_location(Imsi, _VlrId, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(hlr, url),
    Url_b = << Url_prefix/binary, $/, Imsi/binary, <<"/update_location">>/binary>>,
    case rest_req(Url_b, Props, put) of
    {ok, RespJObj} ->
            ok = check_response(RespJObj),
            publish_reg_success(Imsi, Props);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.

get_cf_info(Imsi, Cause) ->
    Props = [{<<"imsi">>, Imsi}, {<<"cause">>, Cause}],
    case wh_amqp_worker:call(Props
                              ,fun wapi_HLR:publish_get_cf_info_req/1
                              ,fun wapi_HLR:get_cf_info_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} ->
            case wh_json:get_ne_value(<<"ftn">>, RespJObj) of
                undefined -> {error, wh_json:get_value([<<"value">>,<<"cause">>], RespJObj)};
                Ftn -> {ok, Ftn}
            end;
        {error, _}  = Err -> Err
    end.

imsi_detach(Imsi) ->
    imsi_detach(Imsi, amqp).
    
imsi_detach(Imsi, Proto) ->
    Props = [{<<"imsi">>, Imsi}],
    imsi_detach(Imsi, Proto, Props).

imsi_detach(Imsi, amqp, Props) ->
    case wh_amqp_worker:call(Props
                              ,fun wapi_HLR:publish_imsi_detach/1
                              ,fun wapi_HLR:imsi_detach_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} ->
            case wh_json:get_ne_value([<<"value">>,<<"result">>], RespJObj) of
                 <<"imsi_detached">> -> ok;
                 _ -> error
            end;
        {error, Err} -> Err
    end;
imsi_detach(Imsi, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(vlr, url),
    Url_b = << Url_prefix/binary, $/, Imsi/binary, <<"/imsi_detach">>/binary>>,
    case rest_req(Url_b, Props, put) of
    {ok, RespJObj} ->
            ok = check_response(RespJObj);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.

get_msisdn(Imsi) ->
    get_msisdn(Imsi, amqp).

get_msisdn(Imsi, Proto) ->
    Props = props:filter_undefined(
         [
            {<<"imsi">>, Imsi}
           ,{<<"cmd">>, <<"get_msisdn">>}
          | wh_api:default_headers(<<"VLR">>, <<"get_msisdn">>, ?APP_NAME, ?APP_VERSION)
         ]),
    get_msisdn(Imsi, Proto, Props).

get_msisdn(_Imsi, amqp, Props) ->
    case wh_amqp_worker:call_collect(Props
                              ,fun wapi_VLR:publish_get_msisdn_req/1
                              ,fun get_msisdn_is_success/1
                              ,2000
                             ) of
        {ok, RespJObjs} ->
            [Msisdn] = [ wh_json:get_ne_value(<<"msisdn">>, RespJObj) || RespJObj <- 
                                   RespJObjs, 
                                   wh_json:get_ne_value(<<"result">>, RespJObj) == <<"success">> ],
            {ok, Msisdn};
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end;
get_msisdn(Imsi, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(vlr, url),
    Url_b = << Url_prefix/binary, $/, Imsi/binary, <<"/get_msisdn">>/binary>>,
    case rest_req(Url_b, Props, get) of
    {ok, RespJObj} ->
            ok = check_response(RespJObj),
            Msisdn = wh_json:get_ne_value(<<"msisdn">>, RespJObj),
            {ok, Msisdn};
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.
    
get_subscriber_info(Id_type, Id) ->
    get_subscriber_info(Id_type, Id, amqp).
    
get_subscriber_info(Id_type, Id, Proto) ->
    Props = props:filter_undefined(
         [
            {<<"id_type">>, Id_type}
           ,{<<"id">>, Id}
           ,{<<"cmd">>, <<"get_subscriber_info">>}
          | wh_api:default_headers(<<"VLR">>, <<"get_subscriber_info">>, ?APP_NAME, ?APP_VERSION)
         ]),
    get_subscriber_info(Id_type, Id, Proto, Props).

get_subscriber_info(Id_type, Id, amqp, Props) ->
    case wh_amqp_worker:call_collect(Props
                              ,fun wapi_VLR:publish_get_subscriber_info_req/1
                              ,fun get_subscriber_is_success/1
                              ,2000
                             ) of
        {ok, RespJObjs} ->
            [Info] = [[ {<<"msisdn">>, wh_json:get_ne_value(<<"msisdn">>, RespJObj)},
                        {<<"imsi">>,   wh_json:get_ne_value(<<"imsi">>, RespJObj)},
                        {<<"isd">>,    wh_json:get_ne_value(<<"subscriber_info">>, RespJObj)}]
                                || RespJObj <-
                                   RespJObjs,
                                   wh_json:get_ne_value(<<"result">>, RespJObj) == <<"success">> ],
            {ok, Info};
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end;
get_subscriber_info(Id_type, Id, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(vlr, url),
    Url_b = << Url_prefix/binary, $/, Id_type/binary, $/, Id/binary, <<"/get_subscriber_info">>/binary>>,
    case rest_req(Url_b, Props, get) of
        {ok, RespJObj} ->
            ok = check_response(RespJObj),
            Info = [{<<"msisdn">>, wh_json:get_ne_value(<<"msisdn">>, RespJObj)},
                    {<<"imsi">>,   wh_json:get_ne_value(<<"imsi">>, RespJObj)},
                    {<<"isd">>,    wh_json:get_ne_value(<<"subscriber_info">>, RespJObj)}],
            {ok, Info};
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.

update_vlr(Id_type, Id, Attribute, Value) ->
    update_vlr(Id_type, Id, Attribute, Value, amqp).
    
update_vlr(Id_type, Id, Attribute, Value, Proto) ->
    Props = props:filter_undefined(
         [
            {<<"id_type">>, Id_type}
           ,{<<"id">>, Id}
           ,{<<"attribute">>, Attribute}
           ,{<<"value">>, Value}
           ,{<<"cmd">>, <<"update_vlr">>}
          | wh_api:default_headers(<<"VLR">>, <<"update_vlr">>, ?APP_NAME, ?APP_VERSION)
         ]),
    update_vlr(Id_type, Id, Attribute, Value, Proto, Props).
    
update_vlr(Id_type, Id, Attribute, Value, amqp, Props) ->
    case wh_amqp_worker:call_collect(Props
                              ,fun wapi_VLR:publish_update_vlr_req/1
                              ,fun update_vlr_is_success/1
                              ,2000
                             ) of
        {ok, _ } -> ok;
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end;
update_vlr(Id_type, Id, Attribute, Value, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(vlr, url),
    Url_b = << Url_prefix/binary, $/, Id_type/binary, $/, Id/binary, <<"/update_vlr">>/binary>>,
    case rest_req(Url_b, Props, put) of
        {ok, _ } -> ok;
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.

submit_sms(Imsi, SmsMsc, SmsMsg) ->
    submit_sms(Imsi, SmsMsc, SmsMsg, amqp).
    
submit_sms(Imsi, SmsMsc, SmsMsg, Proto) ->
    Props = [{<<"imsi">>, Imsi},{<<"smsmsc">>, SmsMsc}, {<<"smsmsg">>, SmsMsg}],
    submit_sms(Imsi, SmsMsc, SmsMsg, Proto, Props).

submit_sms(Imsi, SmsMsc, SmsMsg, amqp, Props) ->
    case wh_amqp_worker:call(Props
                              ,fun wapi_HLR:publish_submit_sms_req/1
                              ,fun wapi_HLR:submit_sms_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} ->
            check_success(RespJObj);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end;
submit_sms(Imsi, SmsMsc, SmsMsg, rest, Props0) ->
    Props = Props0 ++ [{<<"Msg-ID">>,<<"hellodaddy">>}],
    Url_prefix = whapps_config:get(hlr, url),
    Url_b = << Url_prefix/binary, $/, Imsi/binary, <<"/submit_sms">>/binary>>,
    case rest_req(Url_b, Props, put) of
        {ok, RespJObj} ->
            check_success(RespJObj);
        {error, _} = Err -> Err;
        {timeout, _} -> {error, timeout}
    end.
  
deliver_sms(Msisdn, SmsMsc, SmsMsg) ->
    Props = [{<<"msisdn">>, Msisdn},{<<"smsmsc">>, SmsMsc}, {<<"smsmsg">>, SmsMsg}],
    case wh_amqp_worker:call(Props
                              ,fun wapi_HLR:publish_deliver_sms_req/1
                              ,fun wapi_HLR:deliver_sms_resp_v/1
                              ,5000
                             ) of
        {ok, RespJObj} ->
            case  wh_json:get_value([<<"value">>,<<"result">>], RespJObj) of
                <<"accept">> -> ok;
                Else -> {error, Else}
            end;
        {error, _} = Err -> Err
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_msisdn_is_success([JObj|_]) -> get_msisdn_is_success(JObj);
get_msisdn_is_success(JObj) ->
    wapi_VLR:get_msisdn_resp_v(JObj) andalso check_success(JObj).

get_subscriber_is_success([JObj|_]) -> get_subscriber_is_success(JObj);
get_subscriber_is_success(JObj) ->
        wapi_VLR:get_subscriber_info_resp_v(JObj) andalso check_success(JObj).

update_vlr_is_success([JObj|_]) -> update_vlr_is_success(JObj);
update_vlr_is_success(JObj) ->
        wapi_VLR:update_vlr_resp_v(JObj) andalso check_success(JObj).
        
check_success(JObj) ->
    check_success(wh_json:get_ne_value(<<"result">>, JObj), JObj).
check_success(<<"fail">>, _JObj) ->
    false;
check_success(<<"success">>, _JObj) ->
    true.

check_response(JObj) ->
    check_response(wh_json:get_ne_value(<<"result">>, JObj), JObj).
check_response(<<"fail">>, JObj) ->
    throw({error, wh_json:get_ne_value(<<"cause">>, JObj)});
check_response(<<"success">>, _JObj) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_vectors(RespJObj) ->
    case try_triplets(RespJObj) of
        undefined -> try_quintuplets(RespJObj);
        Else -> Else
    end.

try_triplets(RespJObj) ->
    case wh_json:get_ne_value(<<"triplets">>, RespJObj) of
        undefined -> undefined;
        Triplets -> {ok, Triplets}
    end.

try_quintuplets(RespJObj) ->
    case wh_json:get_ne_value(<<"quintuplets">>, RespJObj) of
        undefined -> undefined;
        Else -> {ok, Else}
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_reg_success(Imsi, Props0) ->
   Host_name = props:get_value(<<"hostname">>, Props0),
   Realm = whapps_config:get(<<"hlr">>, <<"realm">>),
   Fs_path =  whapps_config:get(<<"hlr">>, <<"fs_path">>, <<"sip:10.0.101.20:5060">>),
   Props = [
              {<<"First-Registration">>, <<"false">>}
             ,{<<"Expires">>,<<"3600">>}
             ,{<<"Contact">>, <<"<sip:", Imsi/binary, "@", Host_name/binary, ":5060;transport=udp;fs_path=<", Fs_path, ";lr;received='sip:", Host_name/binary, ":5060;transport=udp'>>">>}
             ,{<<"Realm">>, Realm}
             ,{<<"Username">>, Imsi}
             ,{<<"From-User">>, Imsi}
             ,{<<"From-Host">>, Realm}
             ,{<<"To-User">>, Imsi}
             ,{<<"To-Host">>, Realm}
             ,{<<"User-Agent">>, <<"ganx_proxy">>}
           ],
   Req = lists:foldl(fun(K, Acc) ->
                             case props:get_first_defined([wh_util:to_lower_binary(K), K], Props) of
                                 'undefined' -> Acc;
                                 V -> [{K, V} | Acc]
                             end
                     end
                     ,[{<<"Event-Timestamp">>, round(wh_util:current_tstamp())}
                       | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ]
                     ,wapi_registration:success_keys()),
   wh_amqp_worker:cast(Req, fun wapi_registration:publish_success/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rest_req(Url, Props, put) ->
    Body = wh_json:encode(wh_json:from_list(Props)),
    rest_req(Url, Props, put, Body);
rest_req(Url, Props, Method) ->
    rest_req(Url, Props, Method, []).
    
rest_req(Url, Props0, Method, Body) when is_binary(Url) ->
    Props = [{binary_to_list(X), binary_to_list(Y)} || {X, Y} <- Props0],
    rest_req(binary_to_list(Url), Props, Method, Body);

rest_req(Url, Props, Method, Body) ->
    case ibrowse:send_req(Url, Props, Method, Body, [], 3000) of
        {ok, "200", _, Resp} ->
            JObj = wh_json:decode(Resp),
            {ok, JObj};
        {error, _} = Err -> Err
    end. 
