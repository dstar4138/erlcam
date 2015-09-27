%%% ErlCam OTP Supervisor
%%%
%%% Supervise the Pub/Sub Server and Port. If the Port was started up 
%%% separately, it will need to be stopped before starting the Application.
%%%
-module(erlcam_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the OTP Supervisor.
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Get the child description for the supervisor to start up.
init( Args ) ->
    {ok, {#{ strategy  => one_for_one, 
             intensity => 0, 
             period    => 10
           }, 
          [ #{ id    => erlcam_pubsub, 
               start => {erlcam_pubsub, start_link, Args} 
             }
          ]
    }}.

