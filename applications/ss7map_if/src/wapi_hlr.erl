-module(wapi_hlr).

-export([request/1, request_v/1, publish_request/1]).
-export([response_v/1]).

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

request_v(JOBj) ->
    true.

publish_request(Props) ->
%%    Q  = amqp_util:new_queue(<<>>, [{auto_delete, true},{return_value, queue}]),
%%    ok = amqp_util:basic_consume(Q, [{no_ack, true}]),
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:request/1),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload).

response_v(Resp) ->
    true.
