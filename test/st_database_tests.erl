-module (st_database_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

new_test() ->
  {ok, _Db, LinesProcessed} = create_test_db(),
  ?assertMatch(20, LinesProcessed).

lookup_test() ->
  ok. %TODO

count_words_test() ->
  {ok, Db, LnProcd} = create_test_db(),
  Count             = st_database:count_words(Db),
  ?assertMatch(20, LnProcd),
  ?assertMatch(20, Count).

%%%===================================================================
%%% Helpers
%%%===================================================================

create_test_db() ->
  Filename  = code:priv_dir(scrabbletool) ++ "/testdict.txt",
  TableName = st_database_tests,
  st_database:new(TableName, Filename).