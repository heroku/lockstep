%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Smallest 0-perf impact callback module for perf testing.
%% @end
-module(null_callback).

%% gen_lockstep_server callbacks
-export([init/1, handle_msg/2]).

-record(state, {nothing}).

init(_) ->
    {ok, undefined, #state{}}.

handle_msg(_Props, CbState) ->
    {noreply, CbState}.
