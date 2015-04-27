-module(couch_move).

-export([move_all_dbs/2, move_db/3]).

move_all_dbs(SrcNode, DestNode) ->
    {ok, JObjs} =  couch_mgr:admin_all_docs(<<"dbs">>),
    Dbs = [ wh_json:get_value(<<"id">>, JObj) || JObj <- JObjs],
    [ move_db(Db, SrcNode, DestNode) || Db <- Dbs ].
    

move_db(DbName, SrcNode, DestNode) ->
    {ok, Doc} = couch_mgr:admin_open_doc(<<"dbs">>, DbName),
    ByNode = wh_json:get_value(<<"by_node">>, Doc),
    ByRange = wh_json:get_value(<<"by_range">>, Doc),
    Shards = wh_json:get_value(SrcNode, ByNode),

    Fun1 = [fun(JObj) ->
                        wh_json:delete_key(SrcNode, JObj)
                end
                ,fun(JObj) -> 
                        wh_json:set_value(DestNode, Shards, JObj)
                end
               ],
    NewByNode = lists:foldl(fun(F, C) -> F(C) end, ByNode, Fun1),

    Fun2 = fun(Nodes) ->
              case lists:member(SrcNode, Nodes) of
                  true ->
                      L2 = lists:delete(SrcNode, Nodes),
                      L2 ++ [DestNode];
                  _ -> Nodes
              end
          end,
   NewByRange =  wh_json:from_list([{Shard, Fun2(Nodes)} || {Shard, Nodes} <- wh_json:to_proplist(ByRange)]),

   NewDoc = wh_json:set_values([{<<"by_node">>, NewByNode},{<<"by_range">>, NewByRange}], Doc),
   couch_util:save_doc(wh_couch_connections:get_admin_server(), <<"dbs">>, NewDoc, []).
