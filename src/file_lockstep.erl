%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Run lockstep callback from json dump file for testing.
%% @end
-module(file_lockstep).

%% Public API.
-export([run/2]).

%% for timer:tc
-export([foreach_line/2]).

run(Mod, File) ->
    {ok, F} = file:open(File, [binary, read_ahead, raw]),
    try
        {ok, _, MState} = cb_init(Mod),
        timer:tc(?MODULE, foreach_line, [{Mod, MState}, F])
    after
        file:close(F)
    end.

cb_init(Mod) ->
    Mod:init([]).

foreach_line(MS, F) ->
    foreach_line(MS, F, file:read_line(F)).

foreach_line({Callback, CbState}, F, {ok, Line}) ->
    case catch mochijson2:decode(Line) of
        {struct, Props} ->
            case catch Callback:handle_msg(Props, CbState) of
                {noreply, CbState1} ->
                    NewMS = {Callback, CbState1},
                    foreach_line(NewMS, F);
                {stop, Reason, CbState1} ->
                    {Reason, CbState1};
                {'EXIT', Err} ->
                    {Err, CbState}
            end
    end;
foreach_line(MS, _F, eof) ->
    MS.

