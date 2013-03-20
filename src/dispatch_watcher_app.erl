%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2013, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created :  6 Feb 2013 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(dispatch_watcher_app).


-behavior(e2_application).

-export([init/1, init/0, start/0]).

start() ->
    e2_application:start_with_dependencies(dispatch_watcher_app).

init([]) ->
    io:format("~p:~p (~p) init([]) ~n", [?MODULE, ?LINE, self()]),
    {ok, [dispatch_watcher, {dispatch_watcher}]}.

init() ->
    io:format("~p:~p (~p) init() ~n", [?MODULE, ?LINE, self()]),
    {ok, [dispatch_watcher]}.
