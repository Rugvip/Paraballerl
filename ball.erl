-module(ball).
-export([spawn/0, new/0, loop/1]).
-include("paraballerl.hrl").
-define(UPDATE_TIME, 20).

spawn() ->
    spawn(?MODULE, new, []).

new() ->
    random:seed(now()),
    timer:send_interval(?UPDATE_TIME, update),
    Angle = random:uniform() * math:pi() * 2.0,
    State = #ball_state{
        x = random:uniform() - 0.5,
        y = random:uniform() - 0.5,
        vx = math:cos(Angle) * (random:uniform() + 0.5),
        vy = math:sin(Angle) * (random:uniform() + 0.5),
        radius = (random:uniform() + 0.5) * 20
    },
    ?STATE_SERVER ! {new_ball, self(), State},
    loop(State).

next(State) ->
    ?STATE_SERVER ! {ball, self(), State},
    ?PHYSICS_SERVER ! {ball, self(), State},
    loop(State).


loop(State) ->
    receive
        upgrade -> ?MODULE:loop(State);
        update -> next(tick(State));
        {collision, NewState} -> loop(NewState)
    after 1000 ->
        io:format("DAFUQ?~n")
    end.

tick(#ball_state{x = X, y = Y, vx = Vx, vy = Vy} = State) ->
    State#ball_state{
        x = X + Vx,
        y = Y + Vy
    }.
