-module(wapi_VLR).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-define(EXCHANGE, <<"vlr_rpc_x">>).
-define(EXCHANGE_TYPE, <<"fanout">>).

-define(EVENT_CATEGORY, <<"VLR">>).

                               
%% GET_MSISDN Requests
-define(GET_MSISDN_REQ_HEADERS, [<<"cmd">>,<<"imsi">>]).
-define(OPTIONAL_GET_MSISDN_REQ_HEADERS, []).
-define(GET_MSISDN_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, <<"get_msisdn">>}
                              ]).
-define(GET_MSISDN_REQ_TYPES, [{<<"cmd">>, fun is_binary/1}
                             ,{<<"imsi">>, fun is_binary/1}
                             ]).

%% GET_MSISDN Responses
-define(GET_MSISDN_RESP_HEADERS, [<<"result">>]).
-define(OPTIONAL_GET_MSISDN_RESP_HEADERS, [<<"msisdn">>, <<"cause">>]).
-define(GET_MSISDN_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                               ,{<<"Event-Name">>, <<"get_msisdn">>}
                               ,{<<"result">>, [<<"success">>,<<"fail">>]}
                               ]).
-define(GET_MSISDN_RESP_TYPES, []).

%% GET_SUBSCRIBER_INFO Requests
-define(GET_SUBSCRIBER_INFO_REQ_HEADERS, [<<"cmd">>,<<"id_type">>,<<"id">>]).
-define(OPTIONAL_GET_SUBSCRIBER_INFO_REQ_HEADERS, []).
-define(GET_SUBSCRIBER_INFO_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, <<"get_subscriber_info">>}
                              ,{<<"id_type">>, [<<"msrn">>,<<"imsi">>]}
                              ]).
-define(GET_SUBSCRIBER_INFO_REQ_TYPES, [{<<"cmd">>, fun is_binary/1}
                             ,{<<"id_type">>, fun is_binary/1}
                             ,{<<"id">>, fun is_binary/1}
                             ]).

%% GET_SUBSCRIBER_INFO Responses
-define(GET_SUBSCRIBER_INFO_RESP_HEADERS, [<<"result">>]).
-define(OPTIONAL_GET_SUBSCRIBER_INFO_RESP_HEADERS, [<<"msisdn">>,<<"imsi">>,<<"vlr">>,<<"account_id">>,<<"subscriber_info">>,<<"isd">>,<<"cause">>]).
-define(GET_SUBSCRIBER_INFO_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                               ,{<<"Event-Name">>, <<"get_subscriber_info">>}
                               ,{<<"result">>, [<<"success">>,<<"fail">>]}
                               ]).
-define(GET_SUBSCRIBER_INFO_RESP_TYPES, []).



%% UPDATE VLR Requests
-define(UPDATE_VLR_REQ_HEADERS, [<<"cmd">>,<<"id_type">>,<<"id">>,<<"attribute">>,<<"value">>]).
-define(OPTIONAL_UPDATE_VLR_REQ_HEADERS, []).
-define(UPDATE_VLR_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, <<"update_vlr">>}
                              ,{<<"id_type">>, [<<"imsi">>]}
                              ,{<<"attribute">>, [<<"msrn">>,<<"account_id">>]}
                              ]).
-define(UPDATE_VLR_REQ_TYPES, [{<<"cmd">>, fun is_binary/1}
                             ,{<<"id_type">>, fun is_binary/1}
                             ,{<<"id">>, fun is_binary/1}
                             ,{<<"attribute">>, fun is_binary/1}
                             ,{<<"value">>, fun is_binary/1}
                             ]).

%% UPDATE VLR Responses
-define(UPDATE_VLR_RESP_HEADERS, [<<"result">>]).
-define(OPTIONAL_UPDATE_VLR_RESP_HEADERS, [<<"cause">>]).
-define(UPDATE_VLR_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                               ,{<<"Event-Name">>, <<"update_vlr">>}
                               ,{<<"result">>, [<<"success">>,<<"fail">>]}
                               ]).
-define(UPDATE_VLR_RESP_TYPES, []).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([get_msrn_req/1, publish_get_msrn_req/1]).
-export([get_msrn_req_v/1, get_msrn_resp_v/1]).

-export([get_cf_info_req/1, publish_get_cf_info_req/1]).
-export([get_cf_info_req_v/1, get_cf_info_resp_v/1]).

-export([get_msisdn_req/1, publish_get_msisdn_req/1]).
-export([get_msisdn_resp/1, publish_get_msisdn_resp/2]).
-export([get_msisdn_req_v/1, get_msisdn_resp_v/1]).

-export([get_subscriber_info_req/1, publish_get_subscriber_info_req/1]).
-export([get_subscriber_info_resp/1, publish_get_subscriber_info_resp/2]).
-export([get_subscriber_info_req_v/1, get_subscriber_info_resp_v/1]).

-export([update_vlr_req/1, publish_update_vlr_req/1]).
-export([update_vlr_resp/1, publish_update_vlr_resp/2]).
-export([update_vlr_req_v/1, update_vlr_resp_v/1]).


bind_q(Queue, Props) ->
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    bind_q(Queue, Events, Props).

bind_q(Q, [Event|T], Props) ->
    _ = amqp_util:bind_q_to_exchange(Q, Event, ?EXCHANGE),
    bind_q(Q, T, Props);
bind_q(_Q, [], _Props) -> 'ok'.

unbind_q(Queue, Props) ->
    Events = props:get_value('restrict_to', Props, [<<"*">>]),
    unbind_q(Queue, Events, Props).

unbind_q(Q, [Event|T], Props) ->
    _ = amqp_util:unbind_q_from_exchange(?EXCHANGE, Q, Event),
    unbind_q(Q, T, Props);
unbind_q(_Q, [], _Props) -> 'ok'.

%%-------------     -------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
declare_exchanges() ->
    amqp_util:new_exchange(?EXCHANGE, ?EXCHANGE_TYPE).


get_msrn_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
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
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"get_cf_info">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"get_cf_info">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"cause">>], []).

get_cf_info_req_v(_JOBj) ->
    true.

get_cf_info_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_get_msisdn_req(JObj) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?GET_MSISDN_REQ_VALUES, fun ?MODULE:get_msisdn_req/1),
    publish(Payload).

get_msisdn_req(Prop) when is_list(Prop) ->
    case get_msisdn_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?GET_MSISDN_REQ_HEADERS, ?OPTIONAL_GET_MSISDN_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for get_msisdn_req"}
    end;
get_msisdn_req(JObj) ->
    get_msisdn_req(wh_json:to_proplist(JObj)).
    
get_msisdn_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?GET_MSISDN_REQ_HEADERS, ?GET_MSISDN_REQ_VALUES, ?GET_MSISDN_REQ_TYPES);
get_msisdn_req_v(JObj) ->
    get_msisdn_req_v(wh_json:to_proplist(JObj)).

%%%%%%%%%%%

publish_get_msisdn_resp(Queue, JObj) ->
    publish_get_msisdn_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_get_msisdn_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?GET_MSISDN_RESP_VALUES, fun ?MODULE:get_msisdn_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

get_msisdn_resp(Prop) when is_list(Prop) ->
    case get_msisdn_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?GET_MSISDN_RESP_HEADERS, ?OPTIONAL_GET_MSISDN_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for get_msisdn_resp"}
    end;
get_msisdn_resp(JObj) ->
    get_msisdn_resp(wh_json:to_proplist(JObj)).

get_msisdn_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?GET_MSISDN_RESP_HEADERS, ?GET_MSISDN_RESP_VALUES, ?GET_MSISDN_RESP_TYPES);
get_msisdn_resp_v(JObj) ->
    get_msisdn_resp_v(wh_json:to_proplist(JObj)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_get_subscriber_info_req(JObj) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?GET_SUBSCRIBER_INFO_REQ_VALUES, fun ?MODULE:get_subscriber_info_req/1),
    publish(Payload).

get_subscriber_info_req(Prop) when is_list(Prop) ->
    case get_subscriber_info_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?GET_SUBSCRIBER_INFO_REQ_HEADERS, ?OPTIONAL_GET_SUBSCRIBER_INFO_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for get_subscriber_info_req"}
    end;
get_subscriber_info_req(JObj) ->
    get_subscriber_info_req(wh_json:to_proplist(JObj)).
    
get_subscriber_info_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?GET_SUBSCRIBER_INFO_REQ_HEADERS, ?GET_SUBSCRIBER_INFO_REQ_VALUES, ?GET_SUBSCRIBER_INFO_REQ_TYPES);
get_subscriber_info_req_v(JObj) ->
    get_subscriber_info_req_v(wh_json:to_proplist(JObj)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_get_subscriber_info_resp(Queue, JObj) ->
    publish_get_subscriber_info_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_get_subscriber_info_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?GET_SUBSCRIBER_INFO_RESP_VALUES, fun ?MODULE:get_subscriber_info_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

get_subscriber_info_resp(Prop) when is_list(Prop) ->
    case get_subscriber_info_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?GET_SUBSCRIBER_INFO_RESP_HEADERS, ?OPTIONAL_GET_SUBSCRIBER_INFO_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for get_subscriber_info_resp"}
    end;
get_subscriber_info_resp(JObj) ->
    get_subscriber_info_resp(wh_json:to_proplist(JObj)).

get_subscriber_info_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?GET_SUBSCRIBER_INFO_RESP_HEADERS, ?GET_SUBSCRIBER_INFO_RESP_VALUES, ?GET_SUBSCRIBER_INFO_RESP_TYPES);
get_subscriber_info_resp_v(JObj) ->
    get_subscriber_info_resp_v(wh_json:to_proplist(JObj)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_update_vlr_req(JObj) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?UPDATE_VLR_REQ_VALUES, fun ?MODULE:update_vlr_req/1),
    publish(Payload).

update_vlr_req(Prop) when is_list(Prop) ->
    case update_vlr_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_VLR_REQ_HEADERS, ?OPTIONAL_UPDATE_VLR_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for update_vlr_req"}
    end;
update_vlr_req(JObj) ->
    update_vlr_req(wh_json:to_proplist(JObj)).
    
update_vlr_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_VLR_REQ_HEADERS, ?UPDATE_VLR_REQ_VALUES, ?UPDATE_VLR_REQ_TYPES);
update_vlr_req_v(JObj) ->
    update_vlr_req_v(wh_json:to_proplist(JObj)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_update_vlr_resp(Queue, JObj) ->
    publish_update_vlr_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_update_vlr_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?UPDATE_VLR_RESP_VALUES, fun ?MODULE:update_vlr_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

update_vlr_resp(Prop) when is_list(Prop) ->
    case update_vlr_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_VLR_RESP_HEADERS, ?OPTIONAL_UPDATE_VLR_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for update_vlr_resp"}
    end;
update_vlr_resp(JObj) ->
    update_vlr_resp(wh_json:to_proplist(JObj)).

update_vlr_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_VLR_RESP_HEADERS, ?UPDATE_VLR_RESP_VALUES, ?UPDATE_VLR_RESP_TYPES);
update_vlr_resp_v(JObj) ->
    update_vlr_resp_v(wh_json:to_proplist(JObj)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish(Payload) ->
    publish(Payload, ?DEFAULT_CONTENT_TYPE).
    
publish (Payload, ContentType)->
    amqp_util:basic_publish(?EXCHANGE,<<"vlr_rpc_q">>,Payload,ContentType).
