%%%-----------------------------------
%%% @doc 时间轮调用进程例子
%%%-----------------------------------
-module(time_wheel_server).
-behavious(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3 
]).

-record(stat, {interval}).

start_link(Tick) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tick], []).

init([Interval]) ->
    process_flag(trap_exit, true),
    time_wheel:init_wheel(Interval),
    erlang:send_after(Interval, self(), tick),
    {ok, #stat{interval = Interval}}.

handle_call(i, _, State) ->
    Reply = time_wheel:i(),
    {reply, Reply, State};
handle_call({test_insert, After}, _, State) ->
    Reply = time_wheel:test_insert(After),
    {reply, Reply, State};
handle_call(_Request, _, State) ->
    {reply, ok, State}.


handle_cast(i, State) ->
    time_wheel:i(),
    {noreply, State};
handle_cast({test_insert, After}, State) ->
    time_wheel:test_insert(After),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(tick, #stat{interval = Interval} = State) ->
    time_wheel:tick(),
    erlang:send_after(Interval, self(), tick),
    {noreply, State}.

terminate(_, _) -> 
    ok.

code_change(_, State, _) ->
    {ok, State}.