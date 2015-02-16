-module(ememc).
-export([
         ets_test/0,

         ets_create/1,
         ets_fill/2,
         ets_delete_firsts/2,
         ets_delete/1,
         ets_delete_all_objects/1
        ]).

-define(PAUSE, 1).

test(Type) ->
    stater:report("test started"),
    timer:sleep(?PAUSE*1000),
    parallel_consumer(),
    timer:sleep(?PAUSE*1000),
    ?MODULE:(list_to_atom(Type ++ "_create"))(ttt),
    timer:sleep(?PAUSE*1000),
    ?MODULE:(list_to_atom(Type ++ "_fill"))(ttt, 6),
    timer:sleep(?PAUSE*1000),
    ?MODULE:(list_to_atom(Type ++ "_delete_firsts"))(ttt, 3),
    timer:sleep(?PAUSE*3*1000),
    m_garbage_collect(),
    timer:sleep(?PAUSE*3*1000),
    ?MODULE:(list_to_atom(Type ++ "_delete_all_objects"))(ttt),
    timer:sleep(?PAUSE*3*1000),
    ?MODULE:(list_to_atom(Type ++ "_delete"))(ttt),
    timer:sleep(?PAUSE*10*1000),
    stater:done(),
    ok.

ets_create(T) ->
    stater:report("creating ets table"),
    ets:new(T, [named_table, public, bag, {write_concurrency, true}]).

ets_fill(T, Ms) ->
    stater:report("started filling ets table with " ++ integer_to_list(Ms) ++ "m entries"),
    F = fun(0, _, _) -> done;
           (C, O, Fx) ->
                ets:insert(T, {C+O, {C, O, C*O, {C+O, C-O}, "helloworld"}}),
                Fx(C-1, O, Fx)
        end,
    vutil:pmap(fun(O) -> F(1000000, (O-1) * 1000000, F) end,
               lists:seq(1, Ms)),
    stater:report("finished filling ets table").

ets_delete_firsts(T, Ms) ->
    ets_delete_first_(T, Ms*1000000).

ets_delete_first_(T, M) ->
    stater:report("started deleting " ++ integer_to_list(M div 1000000) ++ "m entries in ets table with ets:foldl, ets:delete_object"),
    ets:foldl(fun(_, 0) -> 0;
                 (Obj, MM) ->
                      ets:delete_object(T, Obj),
                      MM - 1
              end, M, T),
    stater:report("stopped deleting with ets:foldl, ets:delete_object").

m_garbage_collect() ->
    stater:report("manual garbage collect"),
    erlang:garbage_collect(),
    stater:report("manual garbage collect done").

%% ets_delete_first_(_T, 0) -> done;
%% ets_delete_first_(T, M) ->
%%     case ets:first(T) of
%%         TT when is_integer(TT) ->
%%             ets:delete(T, TT),
%%             ets_delete_first_(T, M-1);
%%         _ ->
%%             done
%%     end.

ets_delete(T) ->
    stater:report("deleting the table with ets:delete/1"),
    ets:delete(T),
    stater:report("deleted the table with ets:delete/1").

ets_delete_all_objects(T) ->
    stater:report("deleting the table with ets:delete_all_objects/1"),
    ets:delete_all_objects(T),
    stater:report("deleted the table with ets:delete_all_objects/1").

ets_test() ->
    test("ets").

parallel_consumer() ->
    stater:report("parallel consumer started"),
    spawn(fun() ->
               ets:new(yyy, [named_table, public, bag, {write_concurrency, false}]),
               F = fun(C, O, Fx) ->
                           ets:insert(yyy, {C+O, {C, O, C*O, {C+O, C-O}, "helloworld"}}),
                           Fx(C+1, O, Fx)
                   end,
               F(0, 1, F)
          end).

