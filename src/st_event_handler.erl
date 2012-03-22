-module(st_event_handler).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/1, code_change/3]).

%%%===================================================================
%%% Exports
%%%===================================================================

add_handler() ->
	st_event_server:add_handler(?MODULE, []).
  
delete_handler() ->
	st_event_server:delete_handler(?MODULE, []).

%%%===================================================================
%%% Exported Callbacks
%%%===================================================================

init(Args) ->
	ok.

handle_event({lookup_hook, Word}, _State) ->
	error_logger:info_msg("lookup_hook, Word:", []),
	{ok, _State};
handle_event({lookup_hook_tree, Word}, _State) ->
	error_logger:info_msg("lookup_hook_tree, Word:", []),
	{ok, _State};
handle_event(_Event, _State) ->
	{ok, _State}.

handle_call(_Request, _State) ->
	ok.

handle_info(_Request, _State) ->
	ok.

terminate(_State) ->
	ok.

code_change(_OldVsn, _State, _Extra) ->
	ok.

%%%===================================================================
%%% Internal
%%%===================================================================