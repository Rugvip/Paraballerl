-module(physics).
-export([config/1, start/1, init/1]).
-include("paraballerl.hrl").

start(Config) ->
    spawn_link(?MODULE, init, [Config]).

config(Config) -> Config.

init(Config) ->
    catch register(?PHYSICS_SERVER, self()),
    loop(Config),
    ok.

loop(Config = #config{space = #space{
        left = Left, right = Right, bottom = Bottom, top = Top
    }}) ->
    receive
        {ball, Ball, State = #ball_state{x = X, y = Y, vx = Vx, vy = Vy, radius = R}} ->
            if
                X - R < Left -> Ball ! {collision, State#ball_state{vx = abs(Vx)}};
                X + R > Right -> Ball ! {collision, State#ball_state{vx = -abs(Vx)}};
                Y - R < Bottom -> Ball ! {collision, State#ball_state{vy = abs(Vy)}};
                Y + R > Top -> Ball ! {collision, State#ball_state{vy = -abs(Vy)}};
                true -> ok
            end,
            loop(Config)
    end.
