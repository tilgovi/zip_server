zip_server + zip_file
=====================

Playing around with Erlang message passing and file stuffs.

The basic idea is that 100 simultaneous write requests for instance can
be sent to the file driver in one call. Similarly, there is no reason why
100 simultaneous requests for sync() need to issue 100 fsync calls.

So I wrote zip_server which is only a toy right now to see if we could get
speedups.

Big Note
========

This obviously only gives improvements for multiple clients accessing the same
file.
