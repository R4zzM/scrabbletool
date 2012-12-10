-module(st_database).

-export([new/2, lookup/2, count_words/1]).

-define(CHUNK_SIZE, 500).


%%%===================================================================
%%% API
%%%===================================================================

new(TableName, File) ->
  Db = ets:new(TableName, [public]),
  case st_dict_parser:parse(Db, File) of
	  {ok, LinesProcessed} ->
	    case index_db(Db, ?CHUNK_SIZE) of
	      {ok, _} ->
		      {ok, Db, LinesProcessed};
		    _Other ->
		      {stop, indexing_error} 
	    end;    
	  Error ->
	    Error
  end.

lookup(Db, Word) ->
    ets:lookup(Db, Word).

count_words(Db) ->
    FirstKey = ets:first(Db),
    count_words(Db, FirstKey, 0).

%%%===================================================================
%%% Internal functions
%%%===================================================================

count_words(_EtsRef, '$end_of_table', CurrentCount) ->
    CurrentCount;
count_words(EtsRef, Key, CurrentCount) ->
    NextKey = ets:next(EtsRef, Key),
    count_words(EtsRef, NextKey, CurrentCount + 1).

index_db(EtsRef, ChunkSize) ->
    KeyPairs = chunk(EtsRef, ChunkSize), 
    start_indexers(EtsRef, KeyPairs).

% Returns a list containing the pairs of keys {From, To} that can be used to iterate through the database. The number of keys between From and To is ChunkSize.
chunk(EtsRef, ChunkSize) ->
    FirstKey = ets:first(EtsRef),
    chunk(EtsRef, ChunkSize, 1, FirstKey, FirstKey, []).
chunk(_EtsRef, _ChunkSize, _ChunkOffset, '$end_of_table', FromKey, ChunkKeys) ->
    Keys = [{FromKey, '$end_of_table'}, ChunkKeys],
    lists:flatten(lists:reverse(Keys)); % <-- return
chunk(EtsRef, ChunkSize, _ChunkOffset = ChunkSize, PrevKey, FromKey, ChunkKeys) ->
    ToKey = ets:next(EtsRef, PrevKey),
    Keys = [{FromKey, ToKey} | ChunkKeys], 
    chunk(EtsRef, ChunkSize, 0, ToKey, ToKey, Keys);
chunk(EtsRef, ChunkSize, ChunkOffset, PrevKey, FromKey, ChunkKeys) ->
    Key = ets:next(EtsRef, PrevKey),
    chunk(EtsRef, ChunkSize, ChunkOffset + 1, Key, FromKey, ChunkKeys).

start_indexers(EtsRef, KeyPairs) ->
    Pid = self(),
    IndexerPids = [spawn(fun() -> index(EtsRef, Pid, KeyPair, 0) end) || KeyPair <- KeyPairs],
    wait_for_indexers(IndexerPids, 0, 0).

wait_for_indexers(IndexerPids, Progress, TotalWordsProcessed) ->
    receive 
    	{indexer_done, _IndexerPid, ProcessedByIndexer} when Progress < length(IndexerPids) - 1 ->
    	    NewProgress = Progress + 1,
    	    NewTotalWordsProcessed = TotalWordsProcessed + ProcessedByIndexer,
    	    wait_for_indexers(IndexerPids, NewProgress, NewTotalWordsProcessed);
    	{indexer_done, _IndexerPid, ProcessedByIndexer} ->
    	    {ok, TotalWordsProcessed + ProcessedByIndexer};
    	_Other ->
    	    wait_for_indexers(IndexerPids, Progress, TotalWordsProcessed)
        after 30000 ->
    	    timeout
    end.
	  
index(Ref, ServerPid, {CurrentWord, ToWord}, WordsProcessed) ->
    process_word(Ref, CurrentWord),
    case ets:next(Ref, CurrentWord) of
	ToWord -> % done
	    ServerPid ! {indexer_done, self(), WordsProcessed + 1};
	AnotherWord ->
	    index(Ref, ServerPid, {AnotherWord, ToWord}, WordsProcessed + 1)
    end.

process_word(Ref, Word) ->
    {FirstParentCandidate, SecondParentCandidate} = create_parent_candidates(Word),
    _ = case ets:lookup(Ref, FirstParentCandidate) of 
	[{FirstParentCandidate, _}] ->
	    add_as_child(Ref, Word, FirstParentCandidate);
	[] ->
	    candidate_not_a_parent
	end,

    _ = case ets:lookup(Ref, SecondParentCandidate) of 
	[{SecondParentCandidate, _}] ->
	    add_as_child(Ref, Word, SecondParentCandidate); 
	[] ->
	    candidate_not_a_parent
	end,
    ok.
    
% Example: If "erasmat" is used as an arg the result will be {rasmat, erasma}.
create_parent_candidates(Word) ->
    WordAsList = unicode:characters_to_list(Word),
    [_ | FirstCandidate] = WordAsList,
    ReversedWord = lists:reverse(WordAsList),
    [_ | SecondCandidateReversed] = ReversedWord,
    SecondCandidate = lists:reverse(SecondCandidateReversed),
    {unicode:characters_to_binary(FirstCandidate), unicode:characters_to_binary(SecondCandidate)}.

% Child will be added as a child of Parent (doh!).
add_as_child(Ref, Child, Parent) ->
    [{Parent, Children}] = ets:lookup(Ref, Parent),
    ets:insert(Ref, {Parent, [Child | Children]}).