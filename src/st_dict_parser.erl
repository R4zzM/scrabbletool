-module(st_dict_parser).

-export([parse/3]).

%%%===================================================================
%%% API
%%%===================================================================
parse(Ref, FileName, Format) ->
	case Format of
		flat ->
			case populate_db_from_flat_file(Ref, FileName) of
				{ok, LinesProcessed} ->
					{ok, st_database:count_words(Ref), LinesProcessed}; % {Words in db, Words in file} TODO: Fix this. We must return some kind of valid length.
				{error, Reason} ->
					{error, Reason}
			end;
		_Error ->
			{error, invalid_format_specifier} 
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

populate_db_from_flat_file(EtsRef, File) ->
	{ok, FileDescr} = file:open(File, [read, {encoding, utf8}]),
	populate_db_row_by_row(EtsRef, FileDescr, 0).

populate_db_row_by_row(EtsRef, FileDescr, LinesProcessed) ->
	case file:read_line(FileDescr) of 
		{ok, Line} ->
			Word = string:strip(Line, right, $\n),
			WordBinary = unicode:characters_to_binary(Word),
			true = ets:insert(EtsRef, {WordBinary, []}),
			populate_db_row_by_row(EtsRef, FileDescr, LinesProcessed + 1);
		eof ->
			{ok, LinesProcessed};
		{error, Reason} ->
			{error, Reason}
	end.
	
