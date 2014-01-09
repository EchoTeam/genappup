-module(appcfg).
-export([find/0,read/2, get_version/1,get_name/1,check/2, put_version/3]).
-compile({inline,[err/1]}).

-define(DBG(X), io:format("DBG: X  ~p:~p := ~p~n",[?MODULE,?LINE, X]) ).

err(X) -> io:format(X),halt().

find() ->
    ListFiles = os:cmd("git ls-files src/*.app.src --full"), 
    case string:tokens(ListFiles,[10,13]) of
        []    -> err("No src/...app.src file. Cannot read application version~n");
        [X]   -> X;
        [_|_] ->   err("src/...app.src files more than one. Cannot read application version~n")
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


check(undefined,_) -> err("app vsn in the branch is undefined. Cannot continue~n");
check(_,undefined) -> err("app vsn in the HEAD is undefined. Cannot continue~n");
check(git,_) -> err("Sorry, cannot handle git as application version~n");
check(_,git) -> err("Sorry, cannot handle git as application version~n");

check(X,X) ->
    case yesno(["Despite of the fact some erlang files are changed,~n",
		"current and old app versions are the same (",X,
		"). Should I bump up current app version?~n"]) of
        yes  -> 
            Vsn =  bump:version(X),
            io:format("New app vsn = ~p~n",[Vsn]),
            Vsn;
        _ -> 
            err("Ok, stop.~n")
    end;

check(_,X) -> X.



yesno(Msg) ->
    io:format(lists:concat(Msg)),
    Answer = io:get_line("YN>"),
    ClearAnswer = re:replace(Answer, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
    case ClearAnswer of
        "Y" -> yes;
        "y" -> yes;
        "n" -> no;
        "N" -> no;
        _ -> 
            io:format("Please answer Y or N~n"), yesno(Msg)
    end. 

