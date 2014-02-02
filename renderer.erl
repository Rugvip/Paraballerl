-module(renderer).
-export([config/1, start/1, init/1]).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").
-include("paraballerl.hrl").

start(Config) ->
    spawn_link(?MODULE, init, [Config]),
    ok.

config(#config{frame = Frame} = Config) ->
    GLAttrib = [{attribList, [?WX_GL_RGBA,
                  ?WX_GL_DOUBLEBUFFER,
                  ?WX_GL_MIN_RED, 8,
                  ?WX_GL_MIN_GREEN, 8,
                  ?WX_GL_MIN_BLUE, 8,
                  ?WX_GL_DEPTH_SIZE, 0, 0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrib),
    wxGLCanvas:connect(Canvas, size),
    wxGLCanvas:connect(Canvas, paint, [callback]),
    Config#config{canvas = Canvas}.


init(#config{canvas = Canvas, env = Env}) ->
    case lists:member(?RENDERER, registered()) of
        true -> unregister(?RENDERER);
        _ -> ok
    end,
    register(?RENDERER, self()),
    wx:set_env(Env),
    wxGLCanvas:setCurrent(Canvas),
    gl:viewport(0, 0, 600, 400),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:clearColor(1.0, 0.0, 1.0, 1.0),
    loop(Canvas),
    ok.

loop(Config) ->
    ?STATE_SERVER ! {self(), get},
    receive
        {state, State} ->
            gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
            gl:color3f(1.0, 0.0, 0.0),
            dict:fold(fun(_, B, _) -> draw(B), 0 end, 0, State#state.balls),
            wxGLCanvas:swapBuffers(Config),
            timer:sleep(20);
        Other -> io:format("FAIL: ~p~n", [Other])
        after 2000 -> ok
    end,
    loop(Config).


draw(#ball_state{x = X, y = Y}) ->
    gl:'begin'(?GL_TRIANGLE_FAN),
    gl:vertex2f(-0.5 + X,  0.5 + Y),
    gl:vertex2f( 0.5 + X,  0.5 + Y),
    gl:vertex2f( 0.5 + X, -0.5 + Y),
    gl:vertex2f(-0.5 + X, -0.5 + Y),
    gl:vertex2f(-0.5 + X,  0.5 + Y),
    gl:'end'().
