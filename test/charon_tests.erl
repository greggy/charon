%%%-------------------------------------------------------------------
%%% @author greg
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. сен 2014 18:38
%%%-------------------------------------------------------------------
-module(charon_tests).
-author("greg").

-include_lib("eunit/include/eunit.hrl").

storages_test() ->
    {ok, _Pid} = charon_sup:start_link(),

    charon_manager:storage_pid(counter),
    lists:map(fun(X) -> charon_storage:put(counter, {test, X}) end, lists:seq(1, 10)),

    charon_manager:storage_pid(gauge),
    lists:map(fun(X) -> charon_storage:put(gauge, {test, X}) end, lists:seq(1, 10)),

    [
        ?assertEqual(charon_storage:length(counter), 10),
        ?assertEqual(erlang:length(charon_storage:dump(counter)), 10),
        ?assertEqual(charon_storage:length(counter), 0),
        
        ?assertEqual(charon_storage:length(gauge), 10),
        ?assertEqual(erlang:length(charon_storage:dump(gauge)), 10),
        ?assertEqual(charon_storage:length(gauge), 0)
    ].
