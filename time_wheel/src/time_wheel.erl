%%% ------------------------------------------
%%% @doc 时间轮，基于进程字典
%%% 轮的大小：用二进制位数表示，默认轮的大小为[8,6,6,6,6]，即每256次tick第一个轮，才需要检测第二个轮，第三四五个轮同理
%%% 默认轮，从第二个轮开始需要检测对应轮所需的tick次数：256，16384，1048576，67108864
%%% 默认轮仅支持2^32次tick以内的延时挂载，超过则会返回超出范围错误
%%% 超时范围用默认的tick时间（50毫秒）换算结果如下：
%%% math:pow(2, 32) * 50 / 1000 = 214748364秒
%%% math:pow(2, 32) * 50 / 1000 / 60 = 3579139分钟
%%% math:pow(2, 32) * 50 / 1000 / 60 / 60 = 59652小时
%%% math:pow(2, 32) * 50 / 1000 / 60 / 60 / 24 = 2485天
%%% math:pow(2, 32) * 50 / 1000 / 60 / 60 / 24 / 365 = 6.8年
%%% ------------------------------------------
-module(time_wheel).

-export([
    i/0,
    init_wheel/0,
    init_wheel/1,
    init_wheel/2,
    tick/0,
    insert/2,
    insert/4,
    cancel_timer/1,
    get_time/1
]).

-export([
    test_insert/1,
    test_insert/2,
    test_tick/1
]).

-record(time_tick, {tick = 0, interval = 50, current_ms = 0}).
-record(time_wheel, {
    index, % 自身索引
    next, % 下一个索引
    slot = [], % 槽位
    bsr_bit = 0, % 需要便宜的位数
    bit_len = 0, % 长度决定槽位大小
    mask = 0 % 快速计算值
}).
-record(time_wheel_item, {ref, exec_tick, mfa, repeat = false, cd = 0}).
-define(DEFAULT_WHEEL, [{1, 8}, {2, 6}, {3, 6}, {4, 6}, {5, 6}]).
-define(ETS_TIME_WHEEL, ets_time_wheel).

test_insert(After) ->
    test_insert(After, "test mfc ~n").

test_insert(After, Msg) ->
    insert(After, {io, format, [Msg, []]}).

test_tick(N) ->
    lists:foreach(fun(_) -> tick(false) end, lists:seq(1, N)).

i() ->
    [
        {ref, ets:tab2list(?ETS_TIME_WHEEL)},
        {all, all_wheel()},
        {tick, get_tick()}
    ].

all_wheel() ->
    Root = get_root_wheel(),
    all_wheel(Root, []).

all_wheel(#time_wheel{next = Next} = Cur, Acc) ->
    all_wheel(get_time_wheel(Next), [Cur | Acc]);
all_wheel(_, Acc) -> lists:reverse(Acc).

init_wheel() ->
    init_wheel(50, ?DEFAULT_WHEEL).

init_wheel(Interval) ->
    init_wheel(Interval, ?DEFAULT_WHEEL).

init_wheel(Interval, Wheels) ->
    NowMs = now_ms(),
    Tick = #time_tick{current_ms = NowMs, tick = 0, interval = Interval},
    set_tick(Tick),
    FixWheels = case Wheels =:= [] of
        true -> ?DEFAULT_WHEEL;
        false -> Wheels
    end,
    init_wheel_sub(FixWheels, 0),
    [{#time_wheel{index = RootIndex} = Root, _} |_] = FixWheels,
    set_root_index(RootIndex),
    set_time_wheel(Root),
    ets:new(?ETS_TIME_WHEEL, [named_table, set]),
    ok.

init_wheel_sub([{Index, Bit}, {NextIndex, NextBit} | T], AccBit) ->
    Mask = 1 bsl (AccBit + Bit),
    Wheel = #time_wheel{index = Index, next = NextIndex, bsr_bit = AccBit, bit_len = Bit, mask = Mask},
    set_time_wheel(Wheel),
    init_wheel_sub([{NextIndex, NextBit} | T], AccBit + Bit);
init_wheel_sub([{Index, Bit}], AccBit) ->
    Mask = 1 bsl (AccBit + Bit),
    Wheel = #time_wheel{index = Index, bsr_bit = AccBit, bit_len = Bit, mask = Mask},
    set_time_wheel(Wheel),
    ok.

%% 新增一个定时器
insert(After, MFA) ->
    insert(After, MFA, false, 0).

%% 新增一个定时器
%% After 都少秒后执行
%% MFA 回调
%% Repeat 是否需要循环触发
%% Repeat 循环触发间隔CD
insert(After, {_, _, _} = MFA, Repeat, CD) when After > 0 andalso erlang:is_integer(After) ->
    Ref = erlang:make_ref(),
    ExecTick = trans_after_tick(After),
    CDTick = trans_cd_tick(CD),
    Item = #time_wheel_item{ref = Ref, exec_tick = ExecTick, mfa = MFA, repeat = Repeat, cd = CDTick},
    do_insert(Item).

do_insert(#time_wheel_item{exec_tick = ExecTick} = Item) ->
    #time_tick{tick = Tick} = get_tick(),
    After = ExecTick - Tick,
    case After > 0 of
        true -> 
            Root = get_root_wheel(),
            do_insert(Root, Item, After);
        false -> 
            {error, inval_after}
    end.

do_insert(#time_wheel{} = TW, #time_wheel_item{} = Item, After) ->
    #time_wheel{mask = Mask, index = Index, next = NextIndex} = TW,
    #time_wheel_item{ref = Ref, exec_tick = ExecTick} = Item,
    case After < Mask of
        true -> 
            Offset = calc_slot(ExecTick, TW),
            TWX = do_insert_(Offset, Item, TW),
            set_time_wheel(TWX),
            do_insert_time_ref(Ref, Index, Offset),
            {ok, Ref};
        false -> 
            NextTw = get_time_wheel(NextIndex),
            do_insert(NextTw, Item, After)
    end;
do_insert(undefined, _, _) -> 
    {error, after_limit}.

do_insert_(SlotS, Item, #time_wheel{slot = SlotLists} = TW) ->
    SlotList2 = case lists:keytake(SlotS, 1, SlotLists) of
        false -> 
            [{SlotS, [Item]} | SlotLists];
        {value, {_, OldItemList}, Left} ->
            [{SlotS, [Item | OldItemList]} | Left]
        end,
    TW#time_wheel{slot = SlotList2}.

tick() ->
    tick(true).

tick(true) ->
    #time_tick{current_ms = CurMs, interval = Interval, tick = Tick} = TickInfo = get_tick(),
    NowMs = now_ms(),
    NewCurMs = CurMs + Interval, % tick一次的正常时间
    case NowMs - NewCurMs of
        Int when Int < 0 -> too_fast;
        Int when Int < Interval ->
            %% 偏离不多，算一份
            TickInfo1 = TickInfo#time_tick{current_ms = NewCurMs, tick = Tick + 1},
            set_tick(TickInfo1),
            do_wheel_update(Tick + 1),
            ok;
        DTime ->
            %% 偏离大于一份了，要多tick几下
            Cnt = (DTime div Interval) + 1,
            TickInfo1 = TickInfo#time_tick{current_ms = NowMs, tick = Tick + Cnt},
            set_tick(TickInfo1),
            TickList = lists:seq(Tick + 1, Tick + Cnt),
            lists:foreach(fun(T) -> do_wheel_update(T) end, TickList),
            ok
    end;
tick(false) ->
    %% 不修正
    #time_tick{current_ms = CurMs, interval = Interval, tick = Tick} = TickInfo = get_tick(),
    NewTickInfo = TickInfo#time_tick{current_ms = CurMs + Interval, tick = Tick + 1},
    set_tick(NewTickInfo),
    do_wheel_update(Tick + 1),
    ok.

%% 取消定时器
cancel_timer(Ref) ->
    case do_get_time_ref(Ref) of
        {WheelIdx, Slot} ->
            #time_wheel{slot = SlotList} = TW = get_time_wheel(WheelIdx),
            case lists:keytake(Slot, 1, SlotList) of
                {value, {_, SlotItemList}, Left} ->
                    NewSlotItemList = lists:keydelete(Ref, #time_wheel_item.ref, SlotItemList),
                    NewSlotList = [{Slot, NewSlotItemList} | Left],
                    set_time_wheel(TW#time_wheel{slot = NewSlotList}),
                    do_del_time_ref(Ref),
                    ok;
                _ -> 
                    ok
            end;
        _ -> 
            ok
    end.
    
get_time(Ref) ->
    case do_get_time_ref(Ref) of
        {WheelIdx, Slot} ->
            #time_wheel{slot = SlotList} = get_time_wheel(WheelIdx),
            case lists:keyfind(Slot, 1, SlotList) of
                {_, SlotItemList} ->
                    case lists:keyfind(Ref, #time_wheel_item.ref, SlotItemList) of
                        #time_wheel_item{exec_tick = ExecTick} ->
                            #time_tick{current_ms = CurMs, interval = Interval, tick = Tick} = get_tick(),
                            Ms = now_ms(),
                            DMs = erlang:min(Ms - CurMs, Interval),
                            DTick = ExecTick - Tick,
                            DTime = (DTick -1) * Interval + DMs,
                            {ok, DTime};
                        _ -> 
                            false
                    end;
                _ -> 
                    false
            end;
        _ -> 
            false
    end.

do_wheel_update(Tick) ->
    #time_wheel{slot = SlotList} = TW = get_root_wheel(),
    Offset = calc_slot(Tick, TW),
    case lists:keytake(Offset, 1, SlotList) of
        false -> 
            do_wheel_next(TW, Offset, Tick);
        {value, {_, ItemList}, Left} ->
            do_exec(ItemList),
            NewTW = TW#time_wheel{slot = Left},
            set_time_wheel(NewTW),
            do_wheel_next(NewTW, Offset, Tick)
    end.

do_wheel_next(#time_wheel{next = Next}, 0, Tick) ->
    ItemList = do_wheel_pull(Next, Tick),
    lists:foreach(
        fun(#time_wheel_item{ref = Ref} = I) ->
            do_del_time_ref(Ref),
            do_insert(I)
    end, ItemList),
    ok;
do_wheel_next(_, _, _) -> 
    ok.

do_wheel_pull(Index, Tick) ->
    case get_time_wheel(Index) of
        #time_wheel{slot = SlotList, next = Next} = TW ->
            SlotIndex = calc_slot(Tick, TW),
            PullList1 = case lists:keytake(SlotIndex, 1, SlotList) of
                {value, {_, SlotItemList}, Left} ->
                    set_time_wheel(TW#time_wheel{slot = Left}),
                    SlotItemList;
                _ -> []
            end,
            PullList2 = case SlotIndex =:= 0 of
                true -> do_wheel_pull(Next, Tick);
                _ -> []
            end,
            PullList2 ++ PullList1;
        _ -> []
    end.

calc_slot(Tick, #time_wheel{bsr_bit = BsrBit, bit_len = BitLen}) ->
    Mask = (1 bsl BitLen) - 1,
    OffsetValue = Tick bsr BsrBit,
    OffsetValue band Mask.

do_exec(ItemList) ->
    RepeatList = do_exec_item(ItemList, []),
    lists:foreach(fun(I) -> do_insert(I) end, RepeatList),
    ok.

do_exec_item([#time_wheel_item{ref = Ref, mfa = {M, F, A}, repeat = Repeat, cd = CD, exec_tick = Time} = Item | T], Result) ->
    catch erlang:apply(M, F, A),
    do_del_time_ref(Ref),
    case Repeat of
        true ->
            do_exec_item(T, [Item#time_wheel_item{exec_tick = Time + CD} | Result]);
        _ ->
            do_exec_item(T, Result)
    end;
do_exec_item([_ | T], Result) ->
    do_exec_item(T, Result);
do_exec_item(_, Result) ->
    Result.

trans_after_tick(After) ->
    #time_tick{tick = Tick, interval = Interval} = get_tick(),
    AfterTick = erlang:ceil(After / Interval),
    Tick + AfterTick.

trans_cd_tick(CD) when CD > 0 ->
    #time_tick{interval = Interval} = get_tick(),
    CDTick = erlang:floor(CD / Interval),
    erlang:max(CDTick, 1);
trans_cd_tick(_) -> 0.

do_del_time_ref(Ref) ->
    ets:delete(?ETS_TIME_WHEEL, Ref).

do_get_time_ref(Ref) ->
    case ets:lookup(?ETS_TIME_WHEEL, Ref) of
        [{_, Data}] -> Data;
        _ -> undefined
    end.

do_insert_time_ref(Ref, WheelIdx, Offset) ->
    ets:insert(?ETS_TIME_WHEEL, {Ref, {WheelIdx, Offset}}).

set_root_index(Index) ->
    erlang:put({?MODULE, root_index}, Index).
get_root_index() ->
    erlang:get({?MODULE, root_index}).

get_root_wheel() ->
    get_time_wheel(get_root_index()).
get_time_wheel(Index) ->
    erlang:get({?MODULE, wheel, Index}).
set_time_wheel(#time_wheel{index = Index} = Wheel) ->
    erlang:put({?MODULE, wheel, Index}, Wheel).

now_ms() ->
    erlang:system_time(millisecond).

get_tick() ->
    erlang:get({?MODULE, tick}).
set_tick(#time_tick{} = Tick) ->
    erlang:put({?MODULE, tick}, Tick).