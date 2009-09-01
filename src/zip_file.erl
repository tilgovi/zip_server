% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
% A heavily modified couch_file.erl
-module(zip_file).
-behaviour(zip_server).

-export([open/1, create/1, overwrite/1, close/1]).
-export([bytes/1, truncate/2, sync/1]).
-export([append/2, pread/2, pread_term/2]).
-export([write_header/2, read_header/1]).

-export([init/2, zipok/0, handle_zip/3, handle_call/3]).

zipok() ->
    [append, pread, bytes, sync].

open(Filepath) -> start(Filepath, open).
create(Filepath) -> start(Filepath, create).
overwrite(Filepath) -> start(Filepath, overwrite).

start(Filepath, Type) ->
    case proc_lib:start(?MODULE, init, [Filepath, Type]) of
        {ok, Pid} ->
            link(Pid),
            {ok, Pid};
        {'EXIT', Error} ->
            throw(Error);
        Error ->
            throw(Error)
    end.

close(Fd) ->
    zip_zerver:call(Fd, close, nil),
    catch unlink(Fd),
    ok.

bytes(Fd) ->
    case zip_server:call(Fd, bytes, nil) of
        {ok, Length} -> Length;
        Error -> throw(Error)
    end.

truncate(Fd, Size) ->
    case zip_server:call(Fd, truncate, Size) of
        ok -> ok;
        Error -> throw(Error)
    end.

sync(Fd) ->
    case zip_server:call(Fd, sync, nil) of
        ok -> ok;
        Error -> throw(Error)
    end.

append(Fd, Bin) when is_binary(Bin) ->
    case zip_server:call(Fd, append, Bin) of
        {ok, Pos} -> Pos;
        Error -> throw(Error)
    end;
append(Fd, Term) ->
    append(Fd, term_to_binary(Term)).

pread(Fd, Pos) ->
    case zip_server:call(Fd, pread, Pos) of
        {ok, Data} -> Data;
        Error -> throw(Error)
    end.

pread_term(Fd, Pos) ->
    binary_to_term(pread(Fd, Pos)).

write_header(Fd, Header) ->
    case zip_server:call(Fd, write_header, Header) of
        ok -> ok;
        Error -> throw(Error)
    end.

read_header(Fd) ->
    case zip_server:call(Fd, read_header, nil) of
        {ok, Header} -> Header;
        Error -> throw(Error)
    end.

% zip_server callbacks

start(Fd) ->
    proc_lib:init_ack({ok, self()}),
    zip_server:enter_loop(?MODULE, Fd).

error(Error) ->
    proc_lib:init_ack(Error).

init(Filepath, open) ->
    % Open in read mode first, so we don't create
    % the file if it doesn't exist.
    case file:open(Filepath, [read, raw]) of
        {ok, Fd_Read} ->
            {ok, Fd} = file:open(Filepath, [read, write, raw, binary]),
            ok = file:close(Fd_Read),
            start(Fd);
        Error ->
            error(Error)
    end;
init(Filepath, create) ->
    filelib:ensure_dir(Filepath),
    case file:open(Filepath, [read, write, raw, binary]) of
        {ok, Fd} ->
            case file:position(Fd, eof) of
                % Not discriminating between a missing file
                % and an empty file.
                {ok, 0} ->
                    start(Fd);
                % File exists with data
                {ok, _} ->
                    ok = file:close(Fd),
                    error({error, eexist});
                Error ->
                    error(Error)
            end;
        Error ->
            error(Error)
    end;
init(Filepath, overwrite) ->
    filelib:ensure_dir(Filepath),
    case file:open(Filepath, [read, write, raw, binary]) of
        {ok, Fd} ->
            {ok, 0} = file:position(Fd, 0),
            ok = file:truncate(Fd),
            ok = file:sync(Fd),
            start(Fd);
        Error ->
            error(Error)
    end.


handle_zip(append, Bins, Fd) ->
    case file:position(Fd, eof) of
        {ok, Eof} ->
            {_, Writes} = lists:foldl(fun(B, {Pos, Acc}) ->
                Length = size(B),
                Parts = {Pos, <<Length:64/integer, B/binary>>},
                {Pos+8+Length, [Parts | Acc]}
            end, {Eof, []}, Bins),
            case file:pwrite(Fd, lists:reverse(Writes)) of
                ok ->
                    Resp = lists:map(fun(X) -> {ok, element(1, X)} end, Writes),
                    {reply, Resp, Fd};
                Error ->
                    {reply, [Error || _ <- lists:seq(1, length(Bins))], Fd}
            end;
        Error ->
            {reply, [Error || _ <- lists:seq(1, length(Bins))], Fd}
    end;
handle_zip(pread, Places, Fd) ->
    LenPos = [{P, 8} || P <- Places],
    case file:pread(Fd, LenPos) of
        {ok, Res} ->
            DPos = lists:zipwith(fun({Lp, 8}, <<Length:64/integer>>) ->
                {Lp+8, Length}
            end, LenPos, Res),
            case file:pread(Fd, DPos) of
                {ok, Bins} ->
                    {reply, [{ok, B} || B <- Bins], Fd};
                Error ->
                    {reply, [Error || _ <- lists:seq(1, length(Places))], Fd}
            end;
        Error ->
            {reply, [Error || _ <- lists:seq(1, length(Places))], Fd}
    end;
handle_zip(bytes, Msgs, Fd) ->
    Resp = file:position(Fd, eof),
    {reply, [Resp || _ <- lists:seq(1, length(Msgs))], Fd};
handle_zip(sync, Msgs, Fd) ->
    Resp = file:sync(Fd),
    {reply, [Resp || _ <- lists:seq(1, length(Msgs))], Fd}.


handle_call(close, _Arg, Fd) ->
    {stop, normal, Fd};
handle_call(truncate, Pos, Fd) ->
    case file:position(Fd, Pos) of
        {ok, Pos} ->
            {reply, file:truncate(Fd), Fd};
        Error ->
            {reply, Error, Fd}
    end;
handle_call(write_header, Header, Fd) ->
    case file:position(Fd, eof) of
        {ok, Pos} ->
            Data = term_to_binary(Header),
            Sha = crypto:sha(Data),
            Len = size(Data) + size(Sha),
            Hdr = <<0:64/integer, Len:64/integer, Sha/binary, Data/binary>>,
            {reply, file:pwrite(Fd, Pos, Hdr), Fd};
        Error ->
            {reply, Error, Fd}
    end;
handle_call(read_header, _Arg, Fd) ->
    case file:position(Fd, eof) of
        {ok, End} ->
            {reply, (catch find_header(Fd, End-16-20, End)), Fd};
        Error ->
            {reply, Error, Fd}
    end.


find_header(_Fd, -1, _) ->
    {error, no_header};
find_header(Fd, Pos, End) ->
    case file:pread(Fd, Pos, 16) of
        {ok, <<0:64/integer, Length:64/integer>>}
                when Length < 16#100000, Pos + Length =< End ->
            case check_header(Fd, Pos+16, Length) of
                {ok, Header} ->
                    {ok, Header};
                not_found ->
                    find_header(Fd, Pos-1, End);
                Error ->
                    Error
            end;
        {ok, _} ->
            find_header(Fd, Pos-1, End);
        Error ->
            Error
    end.

check_header(Fd, Pos, Length) ->
    case file:pread(Fd, Pos, Length) of
        {ok, <<Sha:20/binary, Header/binary>>} ->
            case crypto:sha(Header) == Sha of
                true ->
                    {ok, binary_to_term(Header)};
                false ->
                    not_found
            end;
        Error ->
            Error
    end.