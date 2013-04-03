-module(addrbook).
-export([start/0]).

start() ->
    spawn(fun init/0).

init() ->
    {{ok, _}, Q} = qte:start("addrbook.ui"),
    [{ok, _, _} = qte:connect(Q, Sender, Signal) 
     || {Sender, Signal} <- 
            [
             {"::root", "lastWindowClosed()"},
             {"action_Add", "triggered()"}
            ]],
    loop(Q).

loop(Q) ->
    receive
        {signal, action_Add, triggered} ->
            io:format("add~n", []),
            loop(Q);
        {signal, _, lastWindowClosed} ->
            io:format("quitting (closed)...~n", []),
            erlang:halt();
        {signal, _Sender, _Signal} ->
            io:format("got signal: ~p from ~p~n", [_Signal, _Sender]),
            loop(Q);
        quit ->
            io:format("quitting (requested)...~n", []),
            erlang:halt();
        What ->
            io:format("got what? ~p~n", [What]),
            loop(Q)
    end.
