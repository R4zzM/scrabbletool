-module (st).

-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(scrabbletool),
  io:format("~n"),
  io:format("##########################################~n"),
  io:format("# All applications started successfully! #~n"),
  io:format("##########################################~n").