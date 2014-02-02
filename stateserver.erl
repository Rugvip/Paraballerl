-module(stateserver).
-export([config/1, start/1, init/1]).
-include("paraballerl.hrl").

start(Config) ->
    spawn_link(?MODULE, init, [Config]).

config(Config) -> Config.

init(_Config) ->
    case lists:member(?STATE_SERVER, registered()) of
        true -> unregister(?STATE_SERVER);
        _ -> ok
    end,
    register(?STATE_SERVER, self()),
    loop(#state{
        balls = dict:new()
    }),
    ok.

loop(State = #state{balls = Balls}) ->
    receive
        {Pid, get} -> Pid ! {state, State}, loop(State);
        {new_ball, Ball, BallState} ->
            io:format("Got new ball: ~p: ~p~n", [Ball, BallState]),
            monitor(process, Ball),
            loop(State#state{balls = dict:store(Ball, BallState, Balls)});
        {ball, Ball, BallState} ->
            loop(State#state{balls = dict:store(Ball, BallState, Balls)});
        {'DOWN', _Ref, process, Ball, Reason} ->
            io:format("Removing ball ~p: ~p~n", [Ball, Reason]),
            loop(State#state{balls = dict:erase(Ball, Balls)})
    end.
