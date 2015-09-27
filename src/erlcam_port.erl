%%% ErlCam Port Wrapper
%%%
%%%   Wraps the bundled BitCam Program which connects with a video device
%%%   using Video4Linux2 compatible drivers. 
%%%
-module(erlcam_port).

-export([start/0,
         stop/1, ensure_stopped/0,
         get_frame/2]).

%%% =========================================================================
%%% API
%%% =========================================================================

%% @doc Turn on the webcam via the device path and the options provided.
-spec start() -> {ok, port()} | {error, any()}.
start() ->
    case catch build_port( ) of
        Port when is_port(Port) -> 
            {ok, Port};
        {'EXIT',Reason} ->
            {error,Reason}
    end.

%% @doc Make sure the port quits using the normal quit functionality.
-spec stop( port() ) -> ok.
stop( Port ) ->
    erlang:port_command( Port, "q\n", [] ),
    ok.

%% @doc Potentially mean killing of the open port.
-spec ensure_stopped() -> ok.
ensure_stopped() ->
    os:cmd("killall bitcam"), 
    ok.

%% @doc Get the current frame from the loaded device.
-spec get_frame( non_neg_integer(), port() ) -> {ok, binary()} | 
                                                {error, timeout}.
get_frame( Timeout, Port ) ->
    erlang:port_command( Port, "c\n", [] ),
    receive
        {_Port, {data, Data}} -> 
            {ok, Data}
        after Timeout -> 
            {error, timeout}
    end.

%%% =========================================================================
%%% Internal functions
%%% =========================================================================

%% @hidden
%% @doc Build the port with the device, and various configuration.
build_port() ->
    Device = application:get_env( erlcam, device, "/dev/video0" ),
    Width  = application:get_env( erlcam, width,  "780" ),
    Height = application:get_env( erlcam, height, "480" ),
    Field  = application:get_env( erlcam, field,  "none" ),
    Format = application:get_env( erlcam, format, "mjpeg" ),
    open_port( {spawn_executable, "priv/bitcam"},
                       [ {args, [Device, Width, Height, Field, Format]},
                         binary,
                         use_stdio,
                         exit_status
                        ] ).

