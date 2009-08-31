-module(file_test).

-export([run/0, run/3, run/4]).

run() ->
    run(zip_file, 100, 1000).

run(Mod, Procs, Reps) ->
    {ok, Fd} = Mod:overwrite("test.dat"),
    {Time, _} = timer:tc(?MODULE, run, [Mod, Fd, Procs, Reps]),
    io:format("Time: ~p (s)~n", [Time/1000000.0]).
    
run(Mod, Fd, Procs, Reps) ->
    spawn_procs(Mod, Fd, Procs, Reps),
    wait(Procs).

wait(0) ->
    ok;
wait(N) ->
    receive
        _ -> wait(N-1)
    end.

spawn_procs(_Mod, _Fd, 0, _Reps) ->
    ok;
spawn_procs(Mod, Fd, N, Reps) ->
    Self = self(),
    spawn(fun() -> work(Mod, Self, Reps, Fd) end),
    spawn_procs(Mod, Fd, N-1, Reps).

work(Mod, Self, Reps, Fd) ->
    Pos = Mod:append(Fd, foobar),
    work(Mod, Self, Reps, Fd, Pos).

work(_Mod, Parent, 0, _Fd, _Pos) ->
    Parent ! done;
work(Mod, Parent, Reps, Fd, Pos) ->
    % 2 on purpose for now
    case random:uniform(2) of
        1 ->
            foobar = Mod:pread_term(Fd, Pos),
            work(Mod, Parent, Reps-1, Fd, Pos);
        2 ->
            Pos2 = Mod:append(Fd, foobar),
            work(Mod, Parent, Reps-1, Fd, Pos2);
        3 ->
            ok = Mod:sync(Fd),
            work(Mod, Parent, Reps-1, Fd, Pos)
    end.
    