-module(com).
-export([pile/0]).


pile() ->
    lists:foreach(fun(Module) ->
        compile:file(Module)
    end, ["main", "renderer", "stateserver", "ball"]).
