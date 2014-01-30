-module(wapi_hlr).

-export([request/1, request_v/1, publish_request/1]).
-export([response_v/1]).

request(Props0) ->
    Defaults = wh_api:default_headers(<<"ss7map_if">>, wh_util:to_binary(?MODULE)),
    IMSI = proplists:get_value(<<"Username">>, Props0),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"update_location">>}
            ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
            ,{<<"cmd">>, <<"update_location">>}
            ,{<<"vlrid">>, <<"kazoo">>}
            ,{<<"imsi">>, IMSI}
            ],
    {ok, Req} = wh_api:build_message(Props, [], []),
    wh_json:encode(Req).

request_v(JOBj) ->
    true.

publish_request(Props) ->
%%    Q  = amqp_util:new_queue(<<>>, [{auto_delete, true},{return_value, queue}]),
%%    ok = amqp_util:basic_consume(Q, [{no_ack, true}]),
    Req = wh_api:prepare_api_payload(Props, [], fun ?MODULE:request/1),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Req),
    ok.

response_v(Resp) ->
    true.
