-module(file_test).

-export([setup/0, setup/1, run/2, test/2]).

-record(cfg, {
    parent=nil,
    module=nil,
    reps=nil,
    fd=nil,
    pos=nil,
    ratios=nil
}).

setup() ->
    #cfg{
        module=zip_file,
        reps=1000,
        ratios=[{read, 1.0}, {write, 1.0}]
    }.

setup(Options) ->
    lists:foldl(fun(O, A) -> set_opt(A, O) end, setup(), Options).

set_opt(Cfg, {module, M}) ->
    Cfg#cfg{module=M};
set_opt(Cfg, {reps, R}) ->
    Cfg#cfg{reps=R};
set_opt(Cfg, {ratios, R}) ->
    Cfg#cfg{ratios=R}.

run(Procs, #cfg{module=Mod, ratios=Ratios}=Cfg) ->
    {ok, Fd} = Mod:overwrite("test.dat"),
    Cfg2 = Cfg#cfg{
        parent=self(),
        fd=Fd,
        ratios=normalize(Ratios)
    },
    {Time, _} = timer:tc(?MODULE, test, [Procs, Cfg2]),
    Time/1000000.0.
    
test(Procs, Cfg) ->
    spawn_procs(Procs, Cfg),
    wait(Procs).

spawn_procs(0, _) ->
    ok;
spawn_procs(Procs, Cfg) ->
    spawn(fun() -> work(Cfg) end),
    spawn_procs(Procs-1, Cfg).

wait(0) -> ok;
wait(N) -> receive _ -> wait(N-1) end.

work(#cfg{module=Mod, fd=Fd, reps=Reps}=Cfg) ->
    Pos = Mod:append(Fd, foobar),
    work(Reps, Cfg#cfg{pos=Pos}).

work(0, #cfg{parent=Parent}) ->
    Parent ! done;
work(Reps, Cfg) ->
    #cfg{
        module=Mod,
        fd=Fd,
        pos=Pos,
        ratios=Ratios
    } = Cfg,
    Sel = random:uniform(),
    {Type, _} = hd(lists:dropwhile(fun({_, V}) -> V < Sel end, Ratios)),
    case Type of
        read ->
            <<131,100,0,6,102,111,111,98,97,114>> = Mod:pread(Fd, Pos),
            work(Reps-1, Cfg);
        write ->
            Pos2 = Mod:append(Fd, <<131,100,0,6,102,111,111,98,97,114>>),
            work(Reps-1, Cfg#cfg{pos=Pos2})
    end.

normalize(Ratios) ->
    Total = lists:sum(lists:map(fun({_, R}) -> R end, Ratios)),
    {NewRatios, _} = lists:foldl(fun({T, R}, {Types, Acc}) ->
        New = {T, R/Total + Acc},
        {[New | Types], R/Total}
    end, {[], 0.0}, Ratios),
    lists:reverse(NewRatios).
    