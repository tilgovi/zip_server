#! /usr/bin/env escript

main([]) ->
    Tests = [
        {couch_file, 1, 100000},
        {couch_file, 10, 10000},
        {couch_file, 100, 1000},
        {zip_file, 1, 100000},
        {zip_file, 10, 10000},
        {zip_file, 100, 1000},
        {couch_file, 100, 10000},
        {zip_file, 100, 10000}
    ],
    run_tests(Tests).

run_tests([]) ->
    ok;
run_tests([{M, P, R} | Rest]) ->
    io:format("Module: ~p~nProcs: ~b~nReps: ~b~n", [M, P, R]),
    file_test:run(M, P, R),
    io:format("~n", []),
    run_tests(Rest).
