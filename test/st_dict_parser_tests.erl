-module (st_dict_parser_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->

  TestFile                = code:priv_dir(scrabbletool) ++ "/testdict.txt",
  Db                      = ets:new(test_table, [public]),
  LinesInTestFile         = 20,
  {ok, LinesProcessed}    = st_dict_parser:parse(Db, TestFile),
  ?assertEqual(LinesInTestFile, LinesProcessed),

  {ok, FileDescr} = file:open(TestFile, [read, {encoding, utf8}]),
  case file:read_line(FileDescr) of 
    {ok, Line} ->
      Word = string:strip(Line, right, $\n),
      Bin  = unicode:characters_to_binary(Word),
      ?assertMatch(true, ets:member(Db, Bin));
    eof ->
      ok;
    _ ->
      ?assertMatch(true, false) % force failure!
  end.