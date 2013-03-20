%% @author Bryan Fink
%% @doc dispatch_watcher monitors a file that is expected to contain
%%      a Webmachine dispatcher configuration.  The file will be
%%      reloaded when its mtime changes.

-module(dispatch_watcher).

-include_lib("kernel/include/file.hrl").
-behavior(e2_task).

-export([start_link/0, handle_task/1]).


-record(state, {last, dispatch, interval}).

-define(DEFAULT_DISPATCH, "priv/dispatch.conf").
-define(DEFAULT_INTERVAL, 1000).

%% External API

%% @spec start() -> Result
%% @doc Start the watcher with the default dispatch path
%%      and monitor interval.
%% gen_server callbacks

start_link() ->
    Dispatch = filename:join(["priv", "dispatch.conf"]),
    io:format("~p:~p (~p) Dispatch File ~p ~n",
					    [?FILE, ?LINE, self(), Dispatch]),
    
    State    =  #state{last		= stamp(),
		       dispatch	= Dispatch},
    e2_task:start_link(?MODULE, State, [{repeat,time_interval:get_interval({1, second}) }]).
 
handle_task(State) ->
    {_Outcome, NewState} = attempt_reload(State, false),
    {repeat, NewState}.

%% handle_call(reload_now, _From, State) ->
%%     {Outcome, NewState} = attempt_reload(State, true),
%%     {reply, Outcome, NewState, NewState#state.interval};
%% handle_call(stop, _From, State) ->
%%     {stop, shutdown, stopped, State};
%% handle_call(_Req, _From, State) ->
%%     {reply, {error, badrequest}, State, State#state.interval}.


%% Internal API

attempt_reload(State, Force) ->
    Now = stamp(),
    Changed = case file:read_file_info(State#state.dispatch) of
                  {ok, FileInfo} ->
                      FileInfo#file_info.mtime >= State#state.last
                          andalso FileInfo#file_info.mtime < Now;
                  {error, Reason} ->
                      {error, Reason}
              end,
    case {Changed, Force} of
        {true, _}      -> attempt_reload2(State#state{last=Now});
        {false, true}  -> attempt_reload2(State#state{last=Now});
%%% do not update timestamp if reload not attempted
        {false, false} -> {nothing_to_do, State};
        {Error, _}     ->
            io:format("Dispatch reload aborted: ~p~n", [Error]),
            {Error, State}
    end.

attempt_reload2(State) ->
    case file:consult(State#state.dispatch) of
        {ok, NewDispatch} ->
	    ExistingRoutes = webmachine_router:get_routes(),
	    NewRoutes = NewDispatch -- ExistingRoutes,
	    lists:foreach(fun(Route) ->
				  io:format("~p:~p (~p) Adding Route ~p ~n",
					    [?FILE, ?LINE, self(), Route]),
				  webmachine_router:add_route(Route)
			  end, NewRoutes),
	    DeadRoutes = ExistingRoutes -- NewDispatch ,
	    lists:foreach(fun(Route) ->
				  io:format("~p:~p (~p) Remove Route ~p ~n",
					    [?FILE, ?LINE, self(), Route]),
				  webmachine_router:remove_route(Route)
			  end, DeadRoutes),
            {ok, State};
        Error ->
            io:format("Dispatch reload failed: ~p~n", [Error]),
            {Error, State}
    end.

stamp() ->
    erlang:localtime().
