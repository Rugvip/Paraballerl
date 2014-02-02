-module(renderer).
-export([config/1, start/1, init/1]).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").
-include("paraballerl.hrl").

-record(gl_state, {ball_list}).

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
    Config#config{canvas = Canvas, gl = #gl_state{}}.


init(#config{canvas = Canvas, env = Env} = Config) ->
    case lists:member(?RENDERER, registered()) of
        true -> unregister(?RENDERER);
        _ -> ok
    end,
    register(?RENDERER, self()),
    wx:set_env(Env),
    wxGLCanvas:setCurrent(Canvas),

    NewConfig = create_ball_list(Config),
    gl:viewport(0, 0, 1024, 720),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho(-512.0, 512.0, -360.0, 360.0, 0.0, 1.0),
    gl:clearColor(1.0, 1.0, 1.0, 1.0),
    loop(NewConfig),
    ok.

loop(#config{canvas = Canvas, gl = Gl} = Config) ->
    ?STATE_SERVER ! {self(), get},
    receive
        {state, State} ->
            gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
            gl:color3f(0.1, 1.0, 0.4),
            dict:fold(fun(_, B, _) -> draw(Gl, B), 0 end, 0, State#state.balls),
            wxGLCanvas:swapBuffers(Canvas),
            timer:sleep(20);
        Other -> io:format("FAIL: ~p~n", [Other])
        after 2000 -> ok
    end,
    loop(Config).

create_ball_list(Config) ->
    List = gl:genLists(1),
    ok = gl:newList(List, ?GL_COMPILE),
    gl:'begin'(?GL_TRIANGLE_FAN),
    lists:foreach(fun(A) ->
        gl:vertex2f(math:cos(A), math:sin(A))
    end, [math:pi() * X / 10.0 || X <- lists:seq(0, 20)]),
    gl:'end'(),
    gl:endList(),
    Config#config{gl = #gl_state{ball_list = List}}.


draw(#gl_state{ball_list = List}, #ball_state{x = X, y = Y, radius = Radius}) ->
    gl:pushMatrix(),
    gl:translatef(X, Y, 0),
    gl:scalef(Radius, Radius, 1.0),
    gl:callList(List),
    gl:popMatrix(),
    ok.
