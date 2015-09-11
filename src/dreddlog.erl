-module(dreddlog).

-behaviour(gen_server).

%% API
-export([start_link/0, open_info/1, open_error/1, info/2, error/2]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(FORMAT, "[~4..0B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B][~s] ~s").

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open_info(Path) -> gen_server:call(?MODULE, {openi, Path}).
open_error(Path) -> gen_server:call(?MODULE, {opene, Path}).
info(Prefix, Msg) -> gen_server:call(?MODULE, {info, Prefix, Msg}).
error(Prefix, Msg) -> gen_server:call(?MODULE, {error, Prefix, Msg}).

%% gen_server callbacks
init([]) -> {ok, {undefined, undefined}}.

terminate(_, {IFH, EFH}) ->
    close_if_open(IFH),
    close_if_open(EFH).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({openi, Path}, _From, {IFH, EFH}) ->
    close_if_open(IFH),
    try
        {ok, FH} = file:open(Path, write),
        {reply, ok, {FH, EFH}}
    catch
        E       -> {reply, {exception, E}, {undefined, EFH}};
        error:E -> {reply, {error, E}, {undefined, EFH}}
    end;
handle_call({opene, Path}, _From, {IFH, EFH}) ->
    close_if_open(EFH),
    try
        {ok, FH} = file:open(Path, write),
        {reply, ok, {IFH, FH}}
    catch
        E       -> {reply, {exception, E}, {IFH, undefined}};
        error:E -> {reply, {error, E}, {IFH, undefined}}
    end;
handle_call({info, _, _}, _From, State={undefined, _}) ->
    {reply, log_not_open, State};
handle_call({error, _, _}, _From, State={_, undefined}) ->
    {reply, log_not_open, State};
handle_call({info, P, Msg}, _From, State={IFH,_}) ->
    {{Y, Mo, D}, {H, Mi, S}} = erlang:localtime(),
    io:format(IFH, ?FORMAT, [Y, Mo, D, H, Mi, S, P, Msg]),
    {reply, ok, State};
handle_call({error, P, Msg}, _From, State={_,EFH}) ->
    {{Y, Mo, D}, {H, Mi, S}} = erlang:localtime(),
    io:format(EFH, ?FORMAT, [Y, Mo, D, H, Mi, S, P, Msg]),
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

%% helper functions
close_if_open(undefined) -> ok;
close_if_open(FH) -> file:close(FH).
