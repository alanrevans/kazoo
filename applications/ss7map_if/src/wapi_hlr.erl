-module(wapi_hlr).

-export([request/1, request_v/1, publish_request/1]).
-export([response_v/1]).

-export([get_msrn_req/1, publish_get_msrn_req/1]).
-export([get_msrn_req_v/1, get_msrn_resp_v/1]).

-export([get_cf_info_req/1, publish_get_cf_info_req/1]).
-export([get_cf_info_req_v/1, get_cf_info_resp_v/1]).

%% Should rename this to update_location_req
request(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    IMSI = props:get_value(<<"Username">>, Props0),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"update_location">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"update_location">>}
            ,{<<"vlrid">>, <<"kazoo.", IMSI/binary>>}
            ,{<<"imsi">>, IMSI}
            ],
    wh_api:build_message(Props, [<<"cmd">>,<<"vlrid">>,<<"imsi">>], []).

request_v(_JOBj) ->
    true.

publish_request(Props) ->
%%    Q  = amqp_util:new_queue(<<>>, [{auto_delete, true},{return_value, queue}]),
%%    ok = amqp_util:basic_consume(Q, [{no_ack, true}]),
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:request/1),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload).

response_v(_Resp) ->
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
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload).

get_msrn_resp_v(_Resp) ->
    true.

publish_get_cf_info_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:get_cf_info_req/1),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload).

get_cf_info_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    Imsi = wh_json:get_value(<<"imsi">>, props:get_value(<<"value">>, Props0)),
    Cause = props:get_value(<<"cause">>, Props0),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"get_cf_info">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"get_cf_info">>}
            ,{<<"imsi">>, Imsi}
            ,{<<"cause">>, Cause}
            ],
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"cause">>], []).

get_cf_info_req_v(_JOBj) ->
    true.

get_cf_info_resp_v(_Resp) ->
    true.
