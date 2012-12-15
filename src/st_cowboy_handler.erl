-module (st_cowboy_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export ([hook/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, hook}
  ], Req, State}.

hook(Req, State) ->
  Body  = st_app:lookup_hook_tree(<<"mo">>),
  {Body, Req, State}.

  %% Konformera resultatet av scrabbletool till mochiJson.