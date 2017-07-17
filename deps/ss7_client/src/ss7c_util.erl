%% SS7 Client Helper functions

-module(ss7c_util).

-record(tripletList, {
        count,
        authTriplet = []
        }).

-record(authTriplet, {
        rand,
        sres,
        kc
        }).

-record(authQuintuplet, {
        rand,
        xres,
        ck,
        ik,
        autn
        }).

-record(rand, {
        rand_length,
        rand_value
        }).

-record(sres, {
        sres_length,
        sres_value
        }).

-record(xres, {
        xres_length,
        xres_value
        }).

-record(kc, {
        kc_length,
        kc_value
        }).

-record(ck, {
        ck_length,
        ck_value
        }).

-record(ik, {
        ik_length,
        ik_value
        }).

-record(autn, {
        autn_length,
        autn_value
        }).

-export([to_bit_string_format/1]).
-export([to_bcd/1, e164_to_bcd/1, e164_to_bcd/2, imsi_to_bcd/1, from_bcd/1, display_id/1]).

-export([get_triplet_list/2, get_triplet_list/3]).

to_bit_string_format(Id) when is_binary(Id) ->
    list_to_binary(display_id(Id));

to_bit_string_format(Id) when is_pid(Id) ->
    list_to_binary(pid_to_list(Id)).

display_id(Id) when is_list(Id) ->
   display_id(from_bcd(Id));

display_id(Id) when is_binary(Id) ->
    IdL = binary_to_list(binary_nibble_swap(Id)),
    [_H|T] = lists:flatten(lists:map(fun(A) -> io_lib:format("~2.16.0B", [A]) end, IdL)),       %% [H|T] to stip off leading nibble (type/oddeven)
    T.

binary_nibble_swap(Binary) when is_binary(Binary) ->
    nibble_swap(binary_to_list(Binary)).

nibble_swap(List) ->
    SwapList =
    lists:map(fun(Num) ->
        <<MSNibble:4,LSNibble:4>> = <<Num>>,
        [<<LSNibble:4, MSNibble:4>>]
        end,
        List),
    list_to_binary(lists:flatten(SwapList)).


imsi_to_bcd(Imsi)  when is_binary(Imsi) ->
    to_bcd(binary_to_list(Imsi));
imsi_to_bcd(Imsi) when is_list(Imsi) ->
    to_bcd(Imsi).
    
e164_to_bcd(unknown, <<"+", Number>>)  ->
    e164_to_bcd(Number);
e164_to_bcd(unknown, Number) ->
    <<16#81, (to_bcd(Number))/binary>>;
e164_to_bcd(international, <<"+", Number>>)  ->
    e164_to_bcd(Number);
e164_to_bcd(international, Number) when is_binary(Number) ->
    e164_to_bcd(Number).
    
e164_to_bcd(E164) ->
    <<16#91, (to_bcd(E164))/binary>>.
    
to_bcd(B) when is_binary(B) ->
    to_bcd(binary_to_list(B));
to_bcd( [$+ | T] ) ->           %% Handle "+4478.....
    to_bcd(T);
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
    
    
from_bcd([16#91|V]) ->                  %% Convert 9144... to +44....
    from_bcd(list_to_binary(V), [$+]);

from_bcd([16#81|V]) ->                  %% Handle unknown number ....
    from_bcd(list_to_binary(V), []);

from_bcd(V) when is_list(V) ->
    from_bcd(list_to_binary(V));

from_bcd(<<16#91,V/binary>>) ->                 %% Convert 9144... to +44....
    from_bcd(V, [$+]);
from_bcd(<<16#81,V/binary>>) ->                 %% Handle unknown number ....
    from_bcd(V, []);

from_bcd(V) when is_binary(V) ->
    from_bcd(V, []).

from_bcd(<<>>, Acc) ->
    Acc;

from_bcd(V, Acc) ->
    <<A:4, B:4, Rest/binary>> = V,
    case A of
        16#0F ->
            from_bcd(Rest, Acc ++ [bcd(B)]);
        _ ->
            from_bcd(Rest, Acc ++ [bcd(B), bcd(A)])
    end.

bcd(X) when X >= 0, X =< 9 ->
    X + $0;
bcd(X) when X == 10 ->
    $*;
bcd(X) when X == 11 ->
    $#;
bcd(X) when X == 12 ->
    $A;
bcd(X) when X == 13 ->
    $B;
bcd(X) when X == 14 ->
    $C.
    
get_triplet_list(Imsi, Count) ->
    get_triplet_list(Imsi, Count, amqp).

get_triplet_list(Imsi, Count, Proto) ->
            case catch ss7c_if:auth_info(Imsi, Count, Proto) of
                {ok, Resp} ->
                        format_triplet_list(Resp);
                {error, Error} ->
                        {error, Error};

                {'EXIT', {timeout, Details}} ->
                        {timeout, Details};
                Else ->
                        Else
           end.

format_triplet_list([H|_] = Resp) ->
    TL = case wh_json:get_ne_value(<<"sres">>, H) of
        undefined -> triplets_from_quins(Resp);
        _ ->
            [#authTriplet{
                rand= #rand{rand_length = 16,
                        rand_value  = erlang:list_to_integer(binary_to_list(wh_json:get_ne_value(<<"rand">>, X)),16)},
                sres= #sres{sres_length= 4,
                        sres_value=  erlang:list_to_integer(binary_to_list(wh_json:get_ne_value(<<"sres">>, X)),16)},
                kc= #kc{kc_length= 8,
                    kc_value=  erlang:list_to_integer(binary_to_list(wh_json:get_ne_value(<<"kc">>, X)),16)}
                } || X <- Resp]
    end,
    {tripletList, length(TL), TL}.

triplets_from_quins(Resp) ->
   [#authTriplet{
            rand= #rand{rand_length = 16,
                      rand_value  = erlang:list_to_integer(binary_to_list(wh_json:get_ne_value(<<"rand">>, X)),16)},
            sres= #sres{sres_length= 4,
                      sres_value=  sres_from_xres(wh_json:get_ne_value(<<"xres">>, X))},
            kc= #kc{kc_length= 8,
                  kc_value=  kc_from_ck_ik(wh_json:get_ne_value(<<"ck">>, X), wh_json:get_ne_value(<<"ik">>, X))}
    } || X <- Resp].

sres_from_xres(Xres) ->
    Xres_list = binary_to_list(Xres),
    Xres_star = lists:append(Xres_list, lists:duplicate(16 - length(Xres_list), $0)),
    Xres_int = erlang:list_to_integer(lists:flatten(Xres_star), 16),
    <<X1:32, X2:32, X3:32, X4:32>> = <<Xres_int:(16*8)>>,
    X1 bxor X2 bxor X3 bxor X4.

kc_from_ck_ik(Ck, Ik) ->
    Ck_int = erlang:list_to_integer(binary_to_list(Ck), 16),
    Ik_int = erlang:list_to_integer(binary_to_list(Ik), 16),
    <<Ck1:64, Ck2:64>> = <<Ck_int:128>>,
    <<Ik1:64, Ik2:64>> = <<Ik_int:128>>,
    Ck1 bxor Ck2 bxor Ik1 bxor Ik2.

