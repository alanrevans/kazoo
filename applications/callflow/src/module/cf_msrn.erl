%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_msrn).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    Msrn = wnm_util:to_e164(whapps_call:to_user(Call)),
    ViewOptions = [{'key', Msrn}],
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:get_results(AccountDb, <<"devices/listing_by_msrn">>, ViewOptions) of
        {'ok', []} ->
            lager:info("msrn ~s doesn't exist", [Msrn]);
        {'ok', [JObj]} ->
            lager:info("Msrn returned ~p", [JObj]),
            CallFlow = whapps_call:kvs_fetch('cf_flow', Call),
            ChildFlow = wh_json:get_value(<<"children">>, CallFlow),
            NewFlow =  wh_json:set_value([<<"data">>, <<"id">>], wh_json:get_ne_value(<<"id">>, JObj), ChildFlow),
            cf_exe:branch(NewFlow, whapps_call:kvs_store('cf_flow', NewFlow, Call));
        {'ok', _} ->
            lager:info("Msrn ~s is ambiguous", [Msrn]),
            cf_exe:stop(Call);
        _E ->
            lager:info("failed to find msrn ~s: ~p", [Msrn, _E]),
            cf_exe:stop(Call)
    end.
