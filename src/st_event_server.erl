-module(st_event_server).

-export([start_link/0, add_handler/2, delete_handler/2]).

-export([lookup_hook/1, lookup_hook_tree/1]).

-define(SERVER, ?MODULE).

start_link() ->
	gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
	gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
	gen_event:delete_handler(?SERVER, Handler, Args).

lookup_hook(Word) ->
	gen_event:notify(?SERVER, {lookup_hook, Word}).

lookup_hook_tree(Word) ->
	gen_event:notify(?SERVER, {lookup_hook_tree, Word}).