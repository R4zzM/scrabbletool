-module(st_dict_parser).

-export([parse/2]).

%%%===================================================================
%%% API
%%%===================================================================
parse(EtsRef, FileName) ->
	case populate_db(EtsRef, FileName) of
		{ok, LinesProcessed} ->
			{ok, LinesProcessed}; % {Words in db, Words in file} TODO: Fix this. We must return some kind of valid length.
    Error ->
      Error
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

populate_db(EtsRef, File) ->
	{ok, FileDescr} = file:open(File, [read, {encoding, utf8}]),
	populate_db(EtsRef, FileDescr, 0).

populate_db(EtsRef, FileDescr, LinesProcessed) ->
	case file:read_line(FileDescr) of 
		{ok, Line} ->
			Word = string:strip(Line, right, $\n),
			Bin  = unicode:characters_to_binary(Word),
			true = ets:insert(EtsRef, {Bin, []}),
			populate_db(EtsRef, FileDescr, LinesProcessed + 1);
		eof ->
			{ok, LinesProcessed};
		Error ->
      Error
	end.