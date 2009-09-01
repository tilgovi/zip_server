zip_server + zip_file
=====================

Playing around with Erlang message passing and file stuffs.

The basic idea is that 100 simultaneous write requests for instance can
be sent to the file driver in one call. Similarly, there is no reason why
100 simultaneous requests for sync() need to issue 100 fsync calls.

So I wrote zip_server which is only a toy right now to see if we could get
speedups.

So far with my silly little benchmarks I see about a 2x or more improvement
for 100 active processes doing 1K read/write operations.

As expected, you only see real improvements when the zip_server is under
load from many simultaneous clients. The single client case appears to
be slightly faster, but its not dramatic enough to say for certain.

Build and Run
=============

    $ git clone git://github.com/davisp/zip_server.git
    $ cd zip_server
    $ make

Sample Output
=============

This is a fairly mindless benchmark that doesn't show what real
speedups you'd get. For instance, there are calls that I left synchronous.
Things like truncate could technically be compressed to truncate to the
shortest length, but that could confuse clients.

The important part is the relative numbers between corresponding tests.

    No load bias:
    module          procs    actions => Time (s)
    couch_file          1     100000 => 3.666
    zip_file            1     100000 => 3.354
    couch_file         10      10000 => 3.333
    zip_file           10      10000 => 2.550
    couch_file        100       1000 => 3.664
    zip_file          100       1000 => 1.841
    couch_file       1000        100 => 8.828
    zip_file         1000        100 => 1.839
    
    Read biased load:
    module          procs    actions => Time (s)
    couch_file          1     100000 => 4.053
    zip_file            1     100000 => 3.306
    couch_file         10      10000 => 3.168
    zip_file           10      10000 => 1.923
    couch_file        100       1000 => 3.781
    zip_file          100       1000 => 1.570
    couch_file       1000        100 => 8.951
    zip_file         1000        100 => 1.648
    
    Write biased load:
    module          procs    actions => Time (s)
    couch_file          1     100000 => 3.349
    zip_file            1     100000 => 3.187
    couch_file         10      10000 => 2.937
    zip_file           10      10000 => 2.162
    couch_file        100       1000 => 3.269
    zip_file          100       1000 => 1.165
    couch_file       1000        100 => 8.235
    zip_file         1000        100 => 1.255

