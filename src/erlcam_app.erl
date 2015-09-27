%%% ErlCam OTP Application
%%%
%%%     The ErlCam application is a standard pub/sub mechanism which
%%%     will publish web-cam binaries on a consistent basis to a 
%%%     process group from a local device. ErlCam can be used without 
%%%     starting up the OTP application if the fault-tolerance is not 
%%%     neccessary. Please note however that it is up to the caller to 
%%%     clean up after themselves if this is the case.
%%%
-module(erlcam_app).
-behaviour(application).

%% Application callbacks
-export([start/2,stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
%% @doc Wrap application startup in start-type check. As long as we are
%%   starting up normally, we can continue. This application does not
%%   support distributed Erlang.
%% @end
start(normal, _StartArgs) -> erlcam_sup:start_link().

%% @private
%% @doc Upon successful stop of the application, we just want to make 
%%   sure we have closed out connection with the port we opened.
%% @end
stop( _State ) -> erlcam_port:ensure_stopped().

