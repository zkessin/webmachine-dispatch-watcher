%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2013, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created :  6 Feb 2013 by Zachary Kessin <>
%%%-------------------------------------------------------------------
-module(dispatch_watcher_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_reader/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, dispatch_watcher, [registered]).

start_reader(_) ->
    e2_task_supervisor:start_task(?MODULE, []).
