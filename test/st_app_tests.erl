-module(st_app_tests).

-behaviour(scrabble_tool_lookup_handler).

-export([manual_lookup_hook/1]).

-include_lib("eunit/include/eunit.hrl").

% For manual testing
% -export([count_test/0, simple_lookup_test/0]).

%%%===================================================================
%%% Automatic Tests
%%%===================================================================
	
start_test() ->
	start_scrabble_tool(),
	stop_scrabble_tool().

lookup_hook_test() ->
	start_scrabble_tool(),
	[{<<"zelot">>,[<<"ozelot">>]}] = lookup("zelot"),
	[{<<"talan">>,[<<"talang">>]}] = lookup("talan"),
	stop_scrabble_tool().	

%%%===================================================================
%%% Manual Tests
%%%===================================================================

% NOTE: Word is a string list, not a string binary. This is to make å, ä and ö characters work. 
manual_lookup_hook(Word) ->
	lookup(Word).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_scrabble_tool() ->
	application:start(scrabble_tool),
	timer:sleep(500).

stop_scrabble_tool() ->
	application:stop(scrabble_tool),
	timer:sleep(500).

lookup(Word) ->
	WordUtf8Binary = unicode:characters_to_binary(Word), 
	scrabble_tool:lookup_hook(WordUtf8Binary).

count_words() ->
	scrabble_tool:count_words().
	

