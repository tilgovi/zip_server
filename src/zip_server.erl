-module(zip_server).

-export([enter_loop/2]).
-export([call/2, call/3]).
-export([behaviour_info/1]).

-record(zipst, {
    zipok=[],
    queue=nil,
    module=nil,
    modst=nil
}).

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
    loop(#zipst{
        zipok=Mod:zipok(),
        queue=queue:new(),
        module=Mod,
        modst=State
    }).

loop(#zipst{queue=Q1}=State) ->
    receive
        Input ->
            Q2 = drain_mbox(queue:in(Input, Q1)),
            loop(drain_queue(State#zipst{queue=Q2}))
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

drain_queue(#zipst{zipok=ZOk, queue=Q1}=State) ->
    case queue:out(Q1) of
        {{value, {From, Type, Mesg}}, Q2} ->
            NewZipSt = case lists:member(Type, ZOk) of
                true ->
                    do_zip(State#zipst{queue=Q2}, Type, [From], [Mesg]);
                _ ->
                    do_call(State#zipst{queue=Q2}, Type, From, Mesg)
            end,
            drain_queue(NewZipSt);
        {empty, Q2} ->
            State#zipst{queue=Q2}
    end.

do_zip(#zipst{queue=Q1}=State, Type, Recips, Messages) ->
    case queue:peek(Q1) of
        {value, {From, Type, Mesg}} ->
            Q2 = queue:drop(Q1),
            do_zip(State#zipst{queue=Q2}, Type, [From|Recips], [Mesg|Messages]);
        _ ->
            handle_zip(State, Type, Recips, Messages)
    end.

handle_zip(#zipst{module=Mod}=State, Type, Recips, Messages) ->
    case catch Mod:handle_zip(Type, Messages, State#zipst.modst) of
        {reply, Reply, NewModSt} ->
            reply(Recips, Reply),
            State#zipst{modst=NewModSt};
        Else ->
            exit(Else)
    end.

do_call(#zipst{module=Mod}=State, Type, From, Mesg) ->
    case catch Mod:handle_call(Type, Mesg, State) of
        {reply, Reply, NewModSt} ->
            reply(From, Reply),
            #zipst{modst=NewModSt};
        Else ->
            exit(Else)
    end.

reply([], []) ->
    ok;
reply([From|Recips], [Resp|Messages]) ->
    reply(From, Resp),
    reply(Recips, Messages);
reply(From, Resp) when is_pid(From) ->
    From ! {self(), Resp}.