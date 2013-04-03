-module(addrbook).
-export([start/0]).

start() ->
    spawn(fun init/0).

init() ->
    {{ok, _}, Q} = qte:start("addrbook.ui"),
    {ok, _, _} = qte:connect(Q, "::application", "lastWindowClosed()"),
    loop(Q).

loop(Q) ->
    receive
        quit ->
            io:format("quitting (requested)...~n", []),
            erlang:halt();
        {signal, _, lastWindowClosed} ->
            io:format("quitting (closed)...~n", []),
            erlang:halt();
        {signal, _Sender, _Signal} ->
            io:format("got signal: ~p from ~p~n", [_Signal, _Sender]),
            loop(Q);
        What ->
            io:format("got what? ~p~n", [What]),
            loop(Q)
    end.
