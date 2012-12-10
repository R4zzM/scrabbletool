-module(st_dict_parser).

-export([parse/2]).

	%%%===================================================================
%%% API
%%%===================================================================
parse(Db, FileName) ->
	case populate_db(Db, FileName) of
		{ok, LinesProcessed} ->
			{ok, LinesProcessed}; % {Words in db, Words in file} TODO: Fix this. We must return some kind of valid length.
    Error ->
      Error
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

populate_db(Db, File) ->
	{ok, FileDescr} = file:open(File, [read, {encoding, utf8}]),
	populate_db(Db, FileDescr, 0).

populate_db(Db, FileDescr, LinesProcessed) ->
	case file:read_line(FileDescr) of 
		{ok, Line} ->
			Word = string:strip(Line, right, $\n),
			Bin  = unicode:characters_to_binary(Word),
			true = ets:insert(Db, {Bin, []}),
			populate_db(Db, FileDescr, LinesProcessed + 1);
		eof ->
			{ok, LinesProcessed};
		Error ->
      Error
	end.