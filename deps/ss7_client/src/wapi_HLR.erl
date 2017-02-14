-module(wapi_HLR).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-define(EXCHANGE, <<"hlr_rpc_x">>).
-define(EXCHANGE_TYPE, <<"direct">>).

-define(EVENT_CATEGORY, <<"HLR">>).

                               
%% Auth Info Requests
-define(AUTH_INFO_REQ_HEADERS, [<<"cmd">>,<<"imsi">>,<<"count">>]).
-define(OPTIONAL_AUTH_INFO_REQ_HEADERS, []).
-define(AUTH_INFO_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, <<"auth_info">>}
                              ]).
-define(AUTH_INFO_REQ_TYPES, [{<<"cmd">>, fun is_binary/1}
                             ,{<<"imsi">>, fun is_binary/1}
                             ,{<<"count">>, fun is_integer/1}
                             ]).

%% Auth Info Responses
-define(AUTH_INFO_RESP_HEADERS, [<<"result">>]).
-define(OPTIONAL_AUTH_INFO_RESP_HEADERS, [<<"cause">>,<<"triplets">>,<<"quintuplets">>]).
-define(AUTH_INFO_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                               ,{<<"Event-Name">>, <<"auth_info">>}
                               ,{<<"result">>, [<<"success">>,<<"fail">>]}
                               ]).
-define(AUTH_INFO_RESP_TYPES, []).

%% Update Location Requests
-define(UPDATE_LOCATION_REQ_HEADERS, [<<"cmd">>,<<"imsi">>]).
-define(OPTIONAL_UPDATE_LOCATION_REQ_HEADERS, [<<"hostname">>]).
-define(UPDATE_LOCATION_REQ_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                              ,{<<"Event-Name">>, <<"update_location">>}
                              ]).
-define(UPDATE_LOCATION_REQ_TYPES, [{<<"cmd">>, fun is_binary/1}
                             ,{<<"imsi">>, fun is_binary/1}
                             ]).

%% Update Location Responses
-define(UPDATE_LOCATION_RESP_HEADERS, [<<"result">>]).
-define(OPTIONAL_UPDATE_LOCATION_RESP_HEADERS, [<<"cause">>]).
-define(UPDATE_LOCATION_RESP_VALUES, [{<<"Event-Category">>, ?EVENT_CATEGORY}
                               ,{<<"Event-Name">>, <<"update_location">>}
                               ,{<<"result">>, [<<"success">>,<<"fail">>]}
                               ]).
-define(UPDATE_LOCATION_RESP_TYPES, []).


-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).


-export([update_location_req/1, publish_update_location_req/1]).
-export([update_location_resp/1, publish_update_location_resp/2]).
-export([update_location_req_v/1, update_location_resp_v/1]).

-export([auth_info_req/1, publish_auth_info_req/1]).
-export([auth_info_resp/1, publish_auth_info_resp/2]).
-export([auth_info_req_v/1, auth_info_resp_v/1]).

-export([sip_info_req/1, publish_sip_info_req/1]).
-export([sip_info_req_v/1, sip_info_resp_v/1]).

-export([submit_sms_req/1, publish_submit_sms_req/1]).
-export([submit_sms_req_v/1, submit_sms_resp_v/1]).

-export([deliver_sms_req/1, publish_deliver_sms_req/1]).
-export([deliver_sms_req_v/1, deliver_sms_resp_v/1]).

-export([publish_update_vlr/1, update_vlr/1]).

-export([publish_imsi_detach/1, imsi_detach/1]).
-export([imsi_detach_v/1, imsi_detach_resp_v/1]).


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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_update_location_req(JObj) ->
    publish_update_location_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_update_location_req(JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?UPDATE_LOCATION_REQ_VALUES, fun ?MODULE:update_location_req/1),
    publish(Payload, ContentType).

update_location_req(Prop) when is_list(Prop) ->
    case update_location_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_LOCATION_REQ_HEADERS, ?OPTIONAL_UPDATE_LOCATION_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for update_location_req"}
    end;
update_location_req(JObj) ->
    update_location_req(wh_json:to_proplist(JObj)).

update_location_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_LOCATION_REQ_HEADERS, ?UPDATE_LOCATION_REQ_VALUES, ?UPDATE_LOCATION_REQ_TYPES);
update_location_req_v(JObj) ->
    update_location_req_v(wh_json:to_proplist(JObj)).

%%%

publish_update_location_resp(Queue, JObj) ->
    publish_update_location_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_update_location_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?UPDATE_LOCATION_RESP_VALUES, fun ?MODULE:update_location_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

update_location_resp(Prop) when is_list(Prop) ->
    case update_location_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?UPDATE_LOCATION_RESP_HEADERS, ?OPTIONAL_UPDATE_LOCATION_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for update_location_resp"}
    end;
update_location_resp(JObj) ->
    update_location_resp(wh_json:to_proplist(JObj)).

update_location_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?UPDATE_LOCATION_RESP_HEADERS, ?UPDATE_LOCATION_RESP_VALUES, ?UPDATE_LOCATION_RESP_TYPES);
update_location_resp_v(JObj) ->
    update_location_resp_v(wh_json:to_proplist(JObj)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_sip_info_req(Props) ->
    {ok, Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:sip_info_req/1),
    publish(Payload).

sip_info_req(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"sip_info">>}
            ,{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Props0)}
            ,{<<"cmd">>, <<"sip_info">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>], []).

sip_info_req_v(_JOBj) ->
    true.

sip_info_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_auth_info_req(JObj) ->
    publish_auth_info_req(JObj, ?DEFAULT_CONTENT_TYPE).
publish_auth_info_req(JObj, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(JObj, ?AUTH_INFO_REQ_VALUES, fun ?MODULE:auth_info_req/1),
    publish(Payload, ContentType).

auth_info_req(Prop) when is_list(Prop) ->
    case auth_info_req_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTH_INFO_REQ_HEADERS, ?OPTIONAL_AUTH_INFO_REQ_HEADERS);
        'false' -> {'error', "Proplist failed validation for auth_info_req"}
    end;
auth_info_req(JObj) ->
    auth_info_req(wh_json:to_proplist(JObj)).

auth_info_req_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTH_INFO_REQ_HEADERS, ?AUTH_INFO_REQ_VALUES, ?AUTH_INFO_REQ_TYPES);
auth_info_req_v(JObj) ->
    auth_info_req_v(wh_json:to_proplist(JObj)).

%%%

publish_auth_info_resp(Queue, JObj) ->
    publish_auth_info_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).
publish_auth_info_resp(Queue, Resp, ContentType) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Resp, ?AUTH_INFO_RESP_VALUES, fun ?MODULE:auth_info_resp/1),
    amqp_util:targeted_publish(Queue, Payload, ContentType).

auth_info_resp(Prop) when is_list(Prop) ->
    case auth_info_resp_v(Prop) of
        'true' -> wh_api:build_message(Prop, ?AUTH_INFO_RESP_HEADERS, ?OPTIONAL_AUTH_INFO_RESP_HEADERS);
        'false' -> {'error', "Proplist failed validation for auth_info_resp"}
    end;
auth_info_resp(JObj) ->
    auth_info_resp(wh_json:to_proplist(JObj)).

auth_info_resp_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?AUTH_INFO_RESP_HEADERS, ?AUTH_INFO_RESP_VALUES, ?AUTH_INFO_RESP_TYPES);
auth_info_resp_v(JObj) ->
    auth_info_resp_v(wh_json:to_proplist(JObj)).

%%%%%%%%%%%%%%%%%%%%%%%%

publish_update_vlr(Props) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:update_vlr/1),
    publish(Payload).

update_vlr(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"update_vlr">>}
            ,{<<"cmd">>, <<"update_vlr">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"imsi">>,<<"vlrid">>], []).


%%%%%%%%%%%%%%%%%%%%

publish_imsi_detach(Props) ->
    {'ok', Payload} = wh_api:prepare_api_payload(Props, [], fun ?MODULE:imsi_detach/1),
    publish(Payload).

imsi_detach(Props0) ->
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
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
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
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
    Defaults = wh_api:default_headers(props:get_value(<<"Server-ID">>, Props0), <<"ss7client_if">>, wh_util:to_binary(?MODULE)),
    Props = Defaults ++ [{<<"Event-Category">>, <<"HLR">>},{<<"Event-Name">>, <<"deliver_sms">>},{<<"cmd">>, <<"deliver_sms">>}] ++ Props0,
    wh_api:build_message(Props, [<<"cmd">>,<<"msisdn">>,<<"smsmsc">>,<<"smsmsg">>], []).

deliver_sms_req_v(_JOBj) ->
    true.

deliver_sms_resp_v(_Resp) ->
    true.

%%%%%%%%%%%%%%%%%%%
publish(Payload) ->
    publish(Payload, ?DEFAULT_CONTENT_TYPE).
    
publish (Payload, ContentType)->
    amqp_util:basic_publish(?EXCHANGE,<<"hlr_rpc_q">>,Payload,ContentType).
