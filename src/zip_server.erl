-module(zip_server).

-export([enter_loop/2]).
-export([call/2, call/3]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {zipok,0},
        {enter_loop,2}
    ];
behaviour_info(_Other) ->
    undefined.

call(Pid, Mesg) ->
    call(Pid, '$nozip', Mesg).

call(Pid, Type, Mesg) ->
    Pid ! {self(), Type, Mesg},
    receive
        {Pid, Resp} ->
            Resp
    end.

enter_loop(Mod, State) ->
    loop(queue:new(), Mod:zipok(), Mod, State).

loop(Queue, ZipOk, Mod, State) ->
    receive
        Input ->
            Queue2 = drain_mbox(queue:in(Input, Queue)),
            {Queue3, NewState} = drain_queue(Queue2, ZipOk, Mod, State),
            loop(Queue3, ZipOk, Mod, NewState)
    end.

drain_mbox(Queue) ->
    receive
        Input ->
            case queue:len(Queue) < 1000 of
                true ->
                    drain_mbox(queue:in(Input, Queue));
                _ ->
                    queue:in(Input, Queue)
            end
    after
        0 -> Queue
    end.

drain_queue(Queue, ZipOk, Mod, State) ->
    case queue:out(Queue) of
        {{value, {From, Type, Mesg}}, Queue2} ->
            {Queue3, NewState} = case lists:member(Type, ZipOk) of
                true ->
                    do_zip(Queue2, [From], Type, Mod, [Mesg], State);
                _ ->
                    do_call(Queue2, From, Mod, Mesg, State)
            end,
            drain_queue(Queue3, ZipOk, Mod, NewState);
        {empty, Queue2} ->
            {Queue2, State}
    end.

do_zip(Queue, Recips, Type, Mod, Messages, State) ->
    case queue:peek(Queue) of
        {value, {From, Type, Mesg}} ->
            Q2 = queue:drop(Queue),
            do_zip(Q2, [From|Recips], Type, Mod, [Mesg|Messages], State);
        _ ->
            handle_zip(Queue, Recips, Type, Mod, Messages, State)
    end.

handle_zip(Queue, Recips, Type, Mod, Messages, State) ->
    case catch Mod:handle_zip(Type, Messages, State) of
        {reply, Reply, NewState} ->
            reply(Recips, Reply),
            {Queue, NewState};
        Else ->
            exit(Else)
    end.

do_call(Queue, From, Mod, Mesg, State) ->
    case catch Mod:handle_call(Mesg, From, State) of
        {reply, Reply, NewState} ->
            reply(From, Reply),
            {Queue, NewState};
        Else ->
            exit(Else)
    end.

reply([], []) ->
    ok;
reply([From|Recips], [Resp|Messages]) ->
    From ! {self(), Resp},
    reply(Recips, Messages);
reply(From, Resp) when is_pid(From) ->
    From ! {self(), Resp}.