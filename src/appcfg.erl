-module(appcfg).
-export([find/0,read/2, get_version/1,get_name/1,check/2, put_version/3]).

-define(DBG(X), io:format("DBG: X  ~p:~p := ~p~n",[?MODULE,?LINE, X]) ).

find() ->
    ListFiles = os:cmd("git ls-files src/*.app.src --full"), 
    case string:tokens(ListFiles,[10,13]) of
        []    -> {error, no_app_file};
        [X]   -> {ok, X};
        [_|_] -> {error, multiple_app_files}
    end.

read(AppFile,Branch) ->
    AppContent = os:cmd("git show "++Branch++":"++AppFile),
    {ok,Scanned,_} = erl_scan:string(AppContent),
    {ok,AppConf} = erl_parse:parse_term(Scanned),
    AppConf.

get_version({application,_,Params}) ->
    proplists:get_value(vsn,Params).

put_version(Filename,{application,Name,Params},NewVsn) ->
    NewParams = lists:keyreplace(vsn,1,Params,{vsn,NewVsn}),
    NewApp = {application,Name,NewParams},
    os:cmd("cp "++Filename++" "++Filename++"~"),
    file:write_file(Filename,io_lib:fwrite("~p.~n",[NewApp])).

get_name({application,Name,_}) -> Name.



check(undefined, _) -> {error, no_old_vsn};
check(_, undefined) -> {error, no_new_vsn};
check(git, _) -> {error, git_as_old_vsn};
check(_, git) -> {error, git_as_new_vsn};
check(OldVsn, OldVsn) -> {error, {bump_version, OldVsn}};
check(_, X) -> {ok, X}.

