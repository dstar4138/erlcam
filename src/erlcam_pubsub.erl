%%% ErlCam Pub/Sub GenServer
%%%
%%%     This simplistic Gen Server will spawn timers to trigger frame
%%%     refreshes and send them to a subscribed process group. This 
%%%     Pub/Sub Server can handle multiple groups at different 
%%%     intervals.
%%%
-module(erlcam_pubsub).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_frame/0]).
-export([add_group/2, rm_group/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { port, groups = [], timeout=100, frame }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get the current/latent frame from the linked device.
-spec get_frame() -> {ok, binary()} | {error, any()}.
get_frame() ->
    gen_server:call( ?MODULE, get_frame ).

%% @doc Add a group that ErlCam will publish to on an interval.
-spec add_group( atom(), pos_integer() ) -> ok.
add_group( PG2GroupName, TriggerTimer ) ->
    gen_server:cast( ?MODULE, {add_group, PG2GroupName, TriggerTimer} ).

%% @doc Remove a group from ErlCam's list of accounts to publish to.
-spec rm_group( atom() ) -> ok.
rm_group( PG2GroupName ) ->
    gen_server:cast( ?MODULE, {rm_group, PG2GroupName} ).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
    {ok, Port} = erlcam_port:start(),
    {ok, #state{port=Port}}.

%% @private
%% @doc Handling call messages. We only use this for ad-hoc get_frames.
handle_call(get_frame, _From, #state{timeout=T,port=P}=S) ->
    case
        catch erlcam_port:get_frame( T, P ) 
    of
        {ok, _Binary}=Reply -> {reply, Reply, S};
        {error, timeout}=Reply -> {reply, Reply, S};
        {'EXIT',Reason} -> {stop, Reason, {error, Reason}, S}
    end. 

%% @private
%% @doc Handling cast messages.
handle_cast({add_group, Name, TriggerTime }, #state{groups=Gs}=S) ->
    case 
        catch add_group( Name, TriggerTime, Gs ) 
    of
        {ok, NewGs} -> {noreply, S#state{groups=NewGs}};
        {warn, _Warn} -> {noreply, S};
        {error, Reason} -> {stop, Reason, S}
    end;
handle_cast({rm_group, G}, #state{groups=Gs}=S) ->
    case
        catch rm_group(G, Gs)
    of
        {ok, NewGs} -> {noreply, S#state{groups=NewGs}};
        {error, Reason} -> {stop, Reason, S}
    end.

%% @private
%% @doc Handling all non call/cast messages.
handle_info({Port, {data,Data}}, #state{port=Port}=S) -> 
    {noreply, S#state{frame=Data}}; 
handle_info(Info, State) -> 
    {stop, {unknownmsg, Info}, State}.

%% @private
%% @doc Terminate the open Port, and verify all groups know we terminated.
terminate( Reason, #state{groups=Gs, port=P} ) ->
    erlcam_port:stop(P),
    lists:foreach( fun(G) -> close_group(G, Reason) end, Gs).

%% @private
%% @doc Convert process state when code is changed. Unused.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @hidden
%% @doc Add a group to the list of groups being published to. This also
%%   starts up a trigger process which will keep a timer to send a 
%%   frame to a group of processes when finished.
%% @end 
add_group( GroupName, TriggerTime, GroupList ) ->
    case 
       lists:keyfind( GroupName, 1, GroupList ) 
    of
        {GroupName, _, _} -> 
            {warn, already_exists};
        false -> 
            Pid = spawn_link( fun() -> 
                                trigger_proc( GroupName, TriggerTime ) 
                              end ),
            GroupItem = {GroupName, TriggerTime, Pid},
            NewGroupList = [GroupItem|GroupList],
            {ok, NewGroupList}
    end.

%% @hidden
%% @doc Remove a group from the group list and shutdown
rm_group( GroupName, GroupList ) ->
    case 
        lists:keytake( GroupName, 1, GroupList )
    of
        {value, GroupItem, NewGroupList} -> 
            close_group( GroupItem, rm_group ),
            {ok, NewGroupList}; 
        false ->
            {error, badarg}
    end.

%% @hidden
%% @doc Shutdown the timer for the group which was linked to ErlCam.
close_group( {_GroupName, _TriggerTime, Pid}, Reason ) ->
    Pid ! {shutdown, Reason}. 

%% @hidden
%% @doc This function is used as the trigger process which will trigger 
%%   refreshes by the pub/sub server for each group.
%% @end
trigger_proc( GroupName, Timeout ) ->
    receive
        {shutdown, Reason} ->
            lists:foreach( fun(P) -> P!{erlcam, shutdown, Reason} end,
                           pg2:get_members( GroupName ) )
    after Timeout ->
%        error_logger:info_msg("trigger_prod:~p:~p:Sending new binary~n",[GroupName, Timeout]),
        process_frame( GroupName ),
        trigger_proc( GroupName, Timeout )
    end.

%% @hidden
%% @doc Get a frame from the server, and send it to all processes currently
%%   in the Process Group.
%% @end
process_frame( GroupName ) ->
    {ok, Frame} = erlcam_pubsub:get_frame(),
    lists:foreach( fun(P) -> ping( Frame, P ) end, 
                   pg2:get_members(GroupName) ).
ping( Frame, Pid ) ->
%    error_logger:info_msg("ping:sending frame to pid(~p)~n",[Pid]),
    Pid ! {erlcam, frame, Frame}.
