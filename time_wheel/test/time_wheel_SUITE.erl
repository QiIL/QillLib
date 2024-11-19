-module(time_wheel_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(nowarn_export_all).
-compile(export_all).

init_per_suite(Config) ->
    io:format("lalalal~n", []),
    time_wheel_server:start_link(),
    Config.

add_test() ->
    io:format("do test insert~n", []),
    time_wheel:test_insert(50),
    time_wheel:test_insert(500),
    time_wheel:test_insert(5000),
    time_wheel:test_insert(50000),
    time_wheel:test_insert(500000),
    time_wheel:i().