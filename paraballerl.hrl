-define(RENDERER, paraballerl_renderer).
-define(STATE_SERVER, paraballerl_state_server).
-define(PHYSICS_SERVER, paraballerl_physics_server).

-record(config, {env, canvas, frame, gl, space}).
-record(state, {balls}).
-record(ball_state, {x, y, vx, vy, radius}).
-record(space, {left, right, bottom, top}).
