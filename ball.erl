-module(ball).
-export([spawn/0, new/0]).
-include("paraballerl.hrl").
-define(UPDATE_TIME, 20).

spawn() ->
    spawn(?MODULE, new, []).

new() ->
    random:seed(now()),
    timer:send_interval(?UPDATE_TIME, update),
    State = #ball_state{
        x = random:uniform() - 0.5,
        y = random:uniform() - 0.5,
        vx = (random:uniform() - 0.5) * 0.01,
        vy = (random:uniform() - 0.5) * 0.01,
        radius = (random:uniform() + 0.5) * 0.1
    },
    ?STATE_SERVER ! {new_ball, self(), State},
    loop(State),
    ok.

next(State) ->
    ?STATE_SERVER ! {ball, self(), State},
    loop(State).


loop(State) ->
    receive
        update -> next(tick(State));
        {collision, {wall, Side}} -> next(collide_wall(Side, State))
    after 1000 ->
        io:format("DAFUQ?~n")
    end.

tick(#ball_state{x = X, y = Y, vx = Vx, vy = Vy} = State) ->
    State#ball_state{
        x = X + Vx,
        y = Y + Vy
    }.

collide_wall(left, #ball_state{vx = Vx} = State) -> State#ball_state{vx = abs(Vx)};
collide_wall(right, #ball_state{vx = Vx} = State) -> State#ball_state{vx = -abs(Vx)};
collide_wall(top, #ball_state{vy = Vy} = State) -> State#ball_state{vy = -abs(Vy)};
collide_wall(bottom, #ball_state{vy = Vy} = State) -> State#ball_state{vy = abs(Vy)}.
