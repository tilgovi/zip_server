#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin/"),

    io:format("~nNo load bias:~n", []),
    io:format("~-10s ~10s ~10s => Time (s)~n", [module, procs, actions]),
    do_tests([{read, 0.5}, {write, 0.5}]),

    io:format("~nRead biased load:~n", []),
    io:format("~-10s ~10s ~10s => Time (s)~n", [module, procs, actions]),
    do_tests([{read, 1.0}, {write, 0.1}]),

    io:format("~nWrite biased load:~n", []),
    io:format("~-10s ~10s ~10s => Time (s)~n", [module, procs, actions]),
    do_tests([{read, 0.1}, {write, 0.9}]).

do_tests(Ratios) ->
    run_tests([
        {couch_file, 1, 100000},
        {zip_file, 1, 100000},

        {couch_file, 10, 10000},
        {zip_file, 10, 10000},

        {couch_file, 100, 1000},
        {zip_file, 100, 1000},

        {couch_file, 1000, 100},
        {zip_file, 1000, 100}
    ], Ratios).

run_tests([], _Ratios) ->
    ok;
run_tests([{M, P, R} | Rest], Ratios) ->
    io:format("~-10s ~10b ~10b => ", [M, P, R]),
    Cfg = file_test:setup([{module, M}, {reps, R}, {ratios, Ratios}]),
    Time = file_test:run(P, Cfg),
    io:format("~.3f~n", [Time]),
    run_tests(Rest, Ratios).
