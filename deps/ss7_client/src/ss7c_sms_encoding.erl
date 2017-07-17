-module(ss7c_sms_encoding).

-author("dawid.figiel@gmail.com").
-export([is_utf16/1, to_7bit/1, from_7bit/1, to_utf16/1, from_utf16/1]).

%% build_sms_submit/3 : used by applications to build an SMS message and submit it to the ss7gw
-export([build_sms_submit/3]).

%% 7-bit encoding
%% ---------------------------------------------------------------------
%% Function for converting string to 7-bit encoding according to:
%% GSM 03.38 Version 5.3.0
%% ---------------------------------------------------------------------
%% Initial Function call: to_7bit(String).
%% ---------------------------------------------------------------------
%% Input:  String containing only ASCII characters
%% ---------------------------------------------------------------------
%% Output: Binary encoded String
%% ---------------------------------------------------------------------
is_utf16(Binary) when is_binary(Binary) ->
    is_utf16(binary_to_list(Binary));
is_utf16(String) when is_list(String) ->
    lists:any(fun(X) -> X > 127 end, String).

to_utf16(String) when is_list(String) ->
    to_utf16(list_to_binary(String));
to_utf16(<<>>) -> <<>>;
to_utf16(String) when is_binary(String) ->
    unicode:characters_to_binary(String, utf8, utf16).

from_utf16(List) when is_list(List) ->
    from_utf16(list_to_binary(List));
from_utf16(<<>>) -> [];
from_utf16(Bin) ->
    unicode:characters_to_binary(Bin, utf16, utf8).

to_7bit([]) -> <<>>;
to_7bit(String) when is_list(String) ->
    to_7bit(list_to_binary(String),<<>>);
to_7bit(String) when is_binary(String) ->
    to_7bit(String,<<>>).

to_7bit(String, <<>>) ->
    to_7bit(ss7c_gsm0338:from_utf8(String), <<>>, 1).

to_7bit(<<Char1:8>>,<<>>,_Cntr) -> <<<<Char1>>/binary>>;
to_7bit(<<_Char1:8>>,Out,8) -> Out;
to_7bit(<< Char1:8>>,Out,7) ->
%%    << Out/binary,<<((Char1 bsr 6) bor 26)>>/binary >>;
    << Out/binary,<<((Char1 bsr 6) bor 64)>>/binary >>;
to_7bit(<<Char1:8>>,Out,Cntr) ->
    << Out/binary,<<(Char1 bsr (Cntr - 1))>>/binary >>;
to_7bit(<<_Char:8,In/binary>>,Out,8)->
    to_7bit(In,Out,1);
to_7bit(<<Char1:8,Char2:8,In/binary>>,Out,Cntr)->
    SRChar1 = Char1 bsr (Cntr - 1),
    NewChar1  = <<Char2:Cntr,SRChar1:(8-Cntr)>>,
    to_7bit(<<<<Char2>>/binary,In/binary>>,<<Out/binary,NewChar1/binary>>,Cntr+1).

%% 7-bit decoding
%% ---------------------------------------------------------------------
%% Function for converting 7-bit encoding to String according to:
%% GSM 03.38 Version 5.3.0
%% ---------------------------------------------------------------------
%% Initial Function call: from_7bit(SevenBitEncodedBinary).
%% ---------------------------------------------------------------------
%% Input: Binary encoded String
%% ---------------------------------------------------------------------
%% Output:  String containing only ASCII characters
%% ---------------------------------------------------------------------
from_7bit(List) when is_list(List) -> from_7bit(list_to_binary(List));
from_7bit(<<>>) -> [];
from_7bit(Bin) ->
    binary_to_list(ss7c_gsm0338:to_utf8(from_7bit(Bin,<<>>,<<>>,1))).

from_7bit(<<>>,<<32>>,Out,8) ->
    Out;
from_7bit(<<>>,<<CharN>>,Out,8) ->
    <<Out/binary,CharN:8>>;
from_7bit(<<>>,<<0>>,Out,_Cntr) ->
    Out;
from_7bit(<<>>,<<CharN>>,Out,Cntr) ->
    <<Out/binary,0:(9-Cntr),CharN:(Cntr-1)>>;
from_7bit(<<CharN:1,CharO:7,In/binary>>,<<CharI>>,Out,8) ->
    from_7bit(In,<<CharN>>,<<Out/binary,0:1,CharI:7,0:1,CharO:7>>,2);
from_7bit(<<Byte:8,In/binary>>,<<>>,<<>>,1) ->
    CharN = Byte bsr 7,
    from_7bit(In,<<CharN>>,<<(Byte band 127)>>,2);
from_7bit(<<Byte:8,In/binary>>,<<CharI>>,Out,Cntr) ->
    Char = (Byte bsl (Cntr - 1)) bor CharI,
    CharN = Byte bsr (8 - Cntr),
    from_7bit(In,<<CharN>>,<<Out/binary,0:1,Char:7>>,Cntr+1).
    
    
-record (sms_submit, {tp_rp, tp_udhi, tp_srr, tp_vpf, tp_rd, tp_mti, tp_mr, tp_dest_addr, tp_pid, tp_dcs, tp_vp, tp_usr_data}).

build_sms_submit(To, TpUserData, text)  ->
    RpUser = #sms_submit{
                    tp_rp = 0,      % no reply path
                    tp_udhi = 0,    % no UD Header
                    tp_srr = 0,     % status report not requested
                    tp_vpf = 2,     % relative format
                    tp_rd = 0,      % do not reject duplicates
                    tp_mti = 1,     % message type indicator 1 = SMS_SUBMIT (MS -> SC)
                    tp_mr = random:uniform(255), % message reference
                    tp_dest_addr = ss7c_util:e164_to_bcd(unknown, To),
                    tp_pid = 0,     % protocol identifier
                    tp_dcs = enc_tp_dcs(TpUserData),
                    tp_vp = 173,        % 7 days
                    tp_usr_data = enc_tp_usr_data(TpUserData)},
    enc_rp_user(RpUser);
build_sms_submit(To, TpUserData, binary) when is_binary(TpUserData) ->
    RpUser = #sms_submit{
                    tp_rp = 0,
                    tp_udhi = 1,
                    tp_srr = 0,
                    tp_vpf = 2,
                    tp_rd = 0,
                    tp_mti = 1,
                    tp_mr = random:uniform(255),
                    tp_dest_addr = ss7c_util:e164_to_bcd(binary_to_list(To)),
                    tp_pid = 0,
                    tp_dcs = 16#F5,
                    tp_vp = 173,
                    tp_usr_data = TpUserData},
    enc_rp_user(RpUser).

enc_tp_usr_data(TpUserData) ->
    case is_utf16(TpUserData) of
        true -> to_utf16(TpUserData);
        false -> to_7bit(TpUserData)
    end.
    
enc_tp_dcs(TpUserData) ->
    case is_utf16(TpUserData) of
        true -> 16#08;
        false -> 16#00
    end.
    
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
             tp_vp = TpVp,
             tp_usr_data = TpUsrData} = SmsSubmit,
    TpDA_L = (byte_size(TpDA) - 1) * 2,
    TpUsrData_L = byte_size(TpUsrData),
    <<TpRp:1, TpUdhi:1, TpSrr:1, TpVpf:2, TpRd:1, TpMti:2, TpMr:8, TpDA_L:8, TpDA/binary, TpPid:8, TpDcs:8, TpVp:8, TpUsrData_L:8, TpUsrData/binary>>.

