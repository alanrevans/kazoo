-module(wapi_hlr).

-export([update_location_req/1, publish_update_location_req/1]).
-export([update_location_req_v/1, update_location_resp_v/1]).

-export([get_msrn_req/1, publish_get_msrn_req/1]).
-export([get_msrn_req_v/1, get_msrn_resp_v/1]).

-export([get_cf_info_req/1, publish_get_cf_info_req/1]).
-export([get_cf_info_req_v/1, get_cf_info_resp_v/1]).

-export([auth_info_req/1, publish_auth_info_req/1]).
-export([auth_info_req_v/1, auth_info_resp_v/1]).

-export([get_msisdn_req/1, publish_get_msisdn_req/1]).
-export([get_msisdn_req_v/1, get_msisdn_resp_v/1]).

-export([sip_info_req/1, publish_sip_info_req/1]).
-export([sip_info_req_v/1, sip_info_resp_v/1]).

-export([submit_sms_req/1, publish_submit_sms_req/1]).
-export([submit_sms_req_v/1, submit_sms_resp_v/1]).

-export([deliver_sms_req/1, publish_deliver_sms_req/1]).
-export([deliver_sms_req_v/1, deliver_sms_resp_v/1]).

-export([publish_update_vlr/1, update_vlr/1]).

-export([publish_imsi_detach/1, imsi_detach/1]).
-export([imsi_detach_v/1, imsi_detach_resp_v/1]).

%% Should rename this to update_location_req
update_location_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"update_location">>}
            ,{<<"cmd">>, <<"update_location">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"vlrid">>,<<"imsi">>], []).

update_location_req_v(_JOBj) ->
    true.

publish_update_location_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:update_location_req/1),
    publish(Payload).

update_location_resp_v(_Resp) ->
    true.

get_msrn_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Msisdn = props:get_value(<<"MSISDN">>, Props0),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"get_msrn">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"get_msrn">>}
            ,{<<"msisdn">>, Msisdn}
            ],
    wh_api:build_message(Props, [<<"cmd">>,<<"msisdn">>], []).

get_msrn_req_v(_JOBj) ->
    true.

publish_get_msrn_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:get_msrn_req/1),
    publish(Payload).

get_msrn_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_get_cf_info_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:get_cf_info_req/1),
    publish(Payload).

get_cf_info_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"get_cf_info">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"get_cf_info">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"cause">>], []).

get_cf_info_req_v(_JOBj) ->
    true.

get_cf_info_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_get_msisdn_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:get_msisdn_req/1),
    publish(Payload).

get_msisdn_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"get_msisdn">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"get_msisdn">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>], []).

get_msisdn_req_v(_JOBj) ->
    true.

get_msisdn_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_sip_info_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:sip_info_req/1),
    publish(Payload).

sip_info_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"sip_info">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"sip_info">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>], []).

sip_info_req_v(_JOBj) ->
    true.

sip_info_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%


publish_auth_info_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:auth_info_req/1),
    publish(Payload).

auth_info_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"auth_info">>},{<<"cmd">>, <<"auth_info">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"count">>], []).

auth_info_req_v(_JOBj) ->
    true.

auth_info_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%

publish_update_vlr(Props) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:update_vlr/1),
    publish(Payload).

update_vlr(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"update_vlr">>}
            ,{<<"cmd">>, <<"update_vlr">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"vlrid">>], []).


%%%%%%%%%%%%%%%%%%%%

publish_imsi_detach(Props) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:imsi_detach/1),
    publish(Payload).

imsi_detach(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"imsi_detach">>}
            ,{<<"cmd">>, <<"imsi_detach">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>], []).

imsi_detach_v(_JOBj) ->
    true.

imsi_detach_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%


publish_submit_sms_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:submit_sms_req/1),
    publish(Payload).

submit_sms_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"submit_sms">>},{<<"cmd">>, <<"submit_sms">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"smsmsc">>,<<"smsmsg">>], []).

submit_sms_req_v(_JOBj) ->
    true.

submit_sms_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%

publish_deliver_sms_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:deliver_sms_req/1),
    publish(Payload).

deliver_sms_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"deliver_sms">>},{<<"cmd">>, <<"deliver_sms">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"msisdn">>,<<"smsmsc">>,<<"smsmsg">>], []).

deliver_sms_req_v(_JOBj) ->
    true.

deliver_sms_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%

publish (Payload)->
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload).
