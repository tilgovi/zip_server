-module(file_test).

-export([run/0, run/3, run/4, run/5]).

run() ->
    run(zip_file, 100, 1000).
run(Mod, Procs, Reps) ->
    run(Mod, Procs, Reps, [{read, 1.0}, {write, 1.0}]).
run(Mod, Procs, Reps, Ratios) ->
    {ok, Fd} = Mod:overwrite("test.dat"),
    {Time, _} = timer:tc(?MODULE, run, [Mod, Fd, Procs, Reps, Ratios]),
    Time/1000000.0.
    
run(Mod, Fd, Procs, Reps, Ratios) ->
    spawn_procs(Mod, Fd, Procs, Reps, Ratios),
    wait(Procs).

wait(0) ->
    ok;
wait(N) ->
    receive
        _ -> wait(N-1)
    end.

spawn_procs(_Mod, _Fd, 0, _Reps, _Ratios) ->
    ok;
spawn_procs(Mod, Fd, N, Reps, Ratios) ->
    Self = self(),
    spawn(fun() -> work(Mod, Self, Reps, Fd, Ratios) end),
    spawn_procs(Mod, Fd, N-1, Reps, Ratios).

work(Mod, Self, Reps, Fd, Ratios) ->
    Total = lists:sum(lists:map(fun({_, R}) -> R end, Ratios)),
    {NewRatios, _} = lists:foldl(fun({T, R}, {Types, Acc}) ->
        New = {T, R/Total + Acc},
        {[New | Types], R/Total}
    end, {[], 0.0}, Ratios),
    NewRatios2 = lists:reverse(NewRatios),
    Pos = Mod:append(Fd, foobar),
    work(Mod, Self, Reps, Fd, Pos, NewRatios2).

work(_Mod, Parent, 0, _Fd, _Pos, _Ratios) ->
    Parent ! done;
work(Mod, Parent, Reps, Fd, Pos, Ratios) ->
    % 2 on purpose for now
    Sel = random:uniform(),
    {Type, _} = hd(lists:dropwhile(fun({_, V}) -> V < Sel end, Ratios)),
    case Type of
        read ->
            <<131,100,0,6,102,111,111,98,97,114>> = Mod:pread(Fd, Pos),
            work(Mod, Parent, Reps-1, Fd, Pos, Ratios);
        write ->
            Pos2 = Mod:append(Fd, <<131,100,0,6,102,111,111,98,97,114>>),
            work(Mod, Parent, Reps-1, Fd, Pos2, Ratios)
    end.

