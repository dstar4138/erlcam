%%% ErlCam
%%%
%%%     A Video4Linux Device wrapper for Erlang. ErlCam uses a Port called
%%%     BitCam, which will capture frames from a given device and publish
%%%     to stdout. Erlang will then capture this output and broadcast it
%%%     within the Erlang VM to given Process Groups.
%%%
-module(erlcam).

-export([start/0, start/2, stop/0]).
-export([get_frame/0]).
-export([add_group/2, remove_group/1]).

%% @doc Start the ErlCam application with the default arguments.
start() -> application:start( ?MODULE ).

%% @doc Start the ErlCam application with the given arguments.
-spec start( Device :: string(), [ Option ] ) -> ok | {error, term()}
       when Option :: {width, string()}  |
                      {height, string()} |
                      {field, string()}  |
                      {format, string()}. 
start( Device, Args ) -> 
    application:load( ?MODULE ),
    load_env( [{device, Device}|Args] ),
    application:start( ?MODULE ).

%% @doc Stop the ErlCam application.
-spec stop() -> ok | {error, term()}.
stop() -> application:stop( ?MODULE ).

%% @doc Get the current/latent frame from the linked device.
-spec get_frame() -> {ok, binary()} | {error, any()}.
get_frame() -> erlcam_pubsub:get_frame().

%% @doc Add a group that ErlCam will publish to on an interval.
-spec add_group( atom(), pos_integer() ) -> ok.
add_group( GroupName, Interval ) -> 
    erlcam_pubsub:add_group( GroupName, Interval ).

%% @doc Remove a group from ErlCam's list of accounts to publish to.
-spec remove_group( atom() ) -> ok.
remove_group( GroupName ) -> erlcam_pubsub:rm_group( GroupName ).

%% @hidden
%% @doc Override the application environment with the arguments passed in. 
load_env( [] ) -> ok;
load_env( [{Par,Val} | Rest] ) ->
    application:set_env( ?MODULE, Par, Val ),
    load_env( Rest ).

