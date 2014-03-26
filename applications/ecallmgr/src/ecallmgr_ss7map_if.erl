%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_ss7map_if).

-include("ecallmgr_ss7map_if.hrl").

-export([submit_sms/2, submit_password/2
        ]).

-define(WAIT_FOR_RESP_TIMEOUT, 10000).

submit_data_sms(Props) ->
    lager:debug("submit_data_sms received"),
    Imsi =  props:get_value(<<"from">>, Props),
    To =  props:get_value(<<"to">>, Props),
    TpUserData =  props:get_value(<<"body">>, Props),
    SmsSubmit = build_data_sms_submit(<<$+, To/binary>>, TpUserData),                %% AlanE: FIXME need to handle national/international numbers
    Id = couch_mgr:get_uuid(),
    Q  = amqp_util:new_queue(<<>>, [{auto_delete, true},{return_value, queue}]),
    ok = amqp_util:basic_consume(Q, [{no_ack, true}]),
    Payload =  wh_json:encode({[{<<"cmd">>, <<"submit_sms">>},{<<"imsi">>, Imsi},{<<"smsmsc">>, "+33644402010"}, {<<"smsmsg">>, binary_to_list(SmsSubmit)}]}),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload,<<"application/json">>,[{correlation_id, Id},{reply_to, Q}]),
    wait_for_response(Q, Id).

submit_password(Msisdn, Imsi) ->
    lager:debug("submit_pasword received"),
    Id = couch_mgr:get_uuid(),
    Q  = amqp_util:new_queue(<<>>, [{auto_delete, true},{return_value, queue}]),
    ok = amqp_util:basic_consume(Q, [{no_ack, true}]),
    Payload =  wh_json:encode({[{<<"cmd">>, <<"sip_info">>},{<<"imsi">>, Imsi}]}),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload,<<"application/json">>,[{correlation_id, Id},{reply_to, Q}]),
    {ok, [JObj]} = wait_for_response(Q, Id),
    Password =  wh_json:get_ne_value([<<"value">>,<<"password">>],JObj),
    Props = [{<<"from">>,<<"208220000000015">>},
             {<<"to">>,Msisdn},
             {<<"body">>,Password}],
    submit_data_sms(Props).

submit_sms(Props, _Node) ->
    lager:debug("submit_sms received MO SMS"),
    From =  props:get_value(<<"from">>, Props),
    To =  props:get_value(<<"to">>, Props),
    [Imsi, _Realm] = binary:split(From, <<"@">>), 
    TpUserData =  props:get_value(<<"body">>, Props),
    SmsSubmit = build_sms_submit(<<$+, To/binary>>, TpUserData),		%% AlanE: FIXME need to handle national/international numbers
    Id = couch_mgr:get_uuid(),
    Q  = amqp_util:new_queue(<<>>, [{auto_delete, true},{return_value, queue}]),
    ok = amqp_util:basic_consume(Q, [{no_ack, true}]),
    Payload =  wh_json:encode({[{<<"cmd">>, <<"submit_sms">>},{<<"imsi">>, Imsi},{<<"smsmsc">>, "+33644402010"}, {<<"smsmsg">>, binary_to_list(SmsSubmit)}]}),
    amqp_util:basic_publish(<<>>,<<"hlr_rpc_q">>,Payload,<<"application/json">>,[{correlation_id, Id},{reply_to, Q}]),
    wait_for_response(Q, Id).

wait_for_response(Q, Id) ->
    receive
        #'basic.consume_ok'{} -> wait_for_response(Q, Id);
        {#'basic.deliver'{routing_key = Q}, 
         #amqp_msg{props = #'P_basic'{correlation_id = Id}, payload=Payload}} ->
%% AlanE: Should check the response
            JObj = wh_json:decode(Payload),
	    lager:debug("Submit SMS Response: ~p", [JObj]),
            {ok, JObj}
    after ?WAIT_FOR_RESP_TIMEOUT ->
            lager:debug("Timed out(~b) waiting for submit_sms response", [?WAIT_FOR_RESP_TIMEOUT]),
            timeout 
    end.


-record (sms_submit, {tp_rp, tp_udhi, tp_srr, tp_vpf, tp_rd, tp_mti, tp_mr, tp_dest_addr, tp_pid, tp_dcs, tp_usr_data}).

build_sms_submit(To, TpUserData) ->
    RpUser = #sms_submit{tp_rp = 0,
                 tp_udhi = 0,
                 tp_srr = 0,
                 tp_vpf = 2,
                 tp_rd = 0,
                 tp_mti = 1,
                 tp_mr = 16#55,
                 tp_dest_addr = to_bcd(binary_to_list(To)),
                 tp_pid = 0,
                 tp_dcs = 0,
                 tp_usr_data = sms_encoding:to_7bit(TpUserData)},
    enc_rp_user(RpUser). 

build_data_sms_submit(To, TpUserData) ->
    RpUser = #sms_submit{tp_rp = 0,
                 tp_udhi = 1,
                 tp_srr = 0,
                 tp_vpf = 2,
                 tp_rd = 0,
                 tp_mti = 1,
                 tp_mr = 16#55,
                 tp_dest_addr = to_bcd(binary_to_list(To)),
                 tp_pid = 0,
                 tp_dcs = 4,
                 tp_usr_data = <<6,5,4,13013:16,13013:16,TpUserData/binary>>},
    enc_rp_user(RpUser).

enc_rp_user(SmsSubmit) when is_record(SmsSubmit, sms_submit) ->
     #sms_submit{tp_rp = TpRp,
                 tp_udhi = TpUdhi,
                 tp_srr = TpSrr,
                 tp_vpf = TpVpf,
                 tp_rd = TpRd,
                 tp_mti = TpMti,
                 tp_mr = TpMr,
                 tp_dest_addr = TpDA,
                 tp_pid = TpPid,
                 tp_dcs = TpDcs,
                 tp_usr_data = TpUsrData} = SmsSubmit,
    TpDA_L = (byte_size(TpDA) - 1) * 2,
    TpUsrData_L = byte_size(TpUsrData),
    <<TpRp:1, TpUdhi:1, TpSrr:1, TpVpf:2, TpRd:1, TpMti:2, TpMr:8, TpDA_L:8, TpDA/binary, TpPid:8, TpDcs:8, 16#AD, TpUsrData_L:8, TpUsrData/binary>>.

to_bcd([$+|T]) ->             %% Handle "+4478.....
    A = to_bcd(T),
    <<16#91, A/binary>>;

to_bcd(V0) when is_list(V0) ->
    V = case length(V0) rem 2 of
       1 ->
          V0 ++ [255];
       0 ->
          V0
    end,
    to_bcd(list_to_binary(V), <<>>).

to_bcd(<<>>, Acc) ->
    Acc;

to_bcd(V, Acc) ->
    <<A:8, B:8, Rest/binary>> = V,
    to_bcd(Rest, <<Acc/binary, (B-$0):4, (A-$0):4>>).

