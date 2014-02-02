-module(main).
-export([start/0, init/0]).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").
-include("paraballerl.hrl").

start() ->
    spawn(?MODULE, init, []).

init() ->
    Modules = [renderer, stateserver],
    Config = config(Modules),
    lists:foreach(fun(M) -> M:start(Config) end, Modules),
    wxWindow:raise(Config#config.frame),
    wxWindow:show(Config#config.frame),
    loop(Config),
    wx:destroy().

config(Modules) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "OpenGL Test", [{pos, {100, 100}}, {size, {1024, 720}}]),

    wxWindow:connect(Frame, close_window),

    Config = #config{
        env = wx:get_env(),
        frame = Frame
    },

    lists:foldl(fun(M, C) -> M:config(C) end, Config, Modules).


loop(#config{frame = Frame} = Config) ->
    receive
        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_menu_selected}} ->
            wxWindow:close(Frame, []);
        #wx{event = #wxClose{type = close_window}} ->
            wxWindow:close(Frame, []);
        Other -> io:format("Msg: ~p~n", [Other]), loop(Config)
    end.
