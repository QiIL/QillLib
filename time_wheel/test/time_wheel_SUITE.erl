-module(time_wheel_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(nowarn_export_all).
-compile(export_all).

init_per_suite(Config) ->
    Config.

init_per_testcase(Case, Config) ->
    [{case_name, Case} | Config].
  
end_per_testcase(_, _) -> ok.

end_per_suite(_Config) -> 
    ok.

all() -> 
    [add_test].

add_test(_Config) ->
    {ok, Pid} = time_wheel_server:start_link(50),
    ct:print("pid:~w", [Pid]),
    gen_server:call(Pid, {test_insert, 50}),
    gen_server:call(Pid, {test_insert, 500}),
    gen_server:call(Pid, {test_insert, 5000}),
    ct:print("i: ~p~n", [gen_server:call(Pid, i)]),
    timer:sleep(100),
    ct:print("i: ~p~n", [gen_server:call(Pid, i)]),
    timer:sleep(1000),
    ct:print("i: ~p~n", [gen_server:call(Pid, i)]),
    ok.
