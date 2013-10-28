%%%-------------------------------------------------------------------
%%% @author Alexey Kishkin
%%% @copyright 2012 AboutEcho
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(bump).

-export([version/1]).


version(Str) ->
    PartList =  parse_version(Str),
    assemble(incr(PartList)).

parse_version(X) -> parse_version(X,[]).
parse_version([],Acc) -> lists:reverse(Acc);
parse_version(Str,Acc) ->
    {Digits,Rest} =  collect_digits(Str,[]),
    {NonDigits,Rest1} = collect_nondigits(Rest,[]),
    parse_version(Rest1,[{Digits,NonDigits}|Acc]).

collect_digits([],Acc) -> {lists:reverse(Acc),[]};
collect_digits([X|Rest],Acc) when X >= $0 andalso X =< $9 -> collect_digits(Rest,[X|Acc]);
collect_digits(R,Acc) -> {lists:reverse(Acc),R}.

collect_nondigits([],Acc) -> {lists:reverse(Acc),[]};
collect_nondigits([X|Rest],Acc) when X < $0 orelse X > $9 -> collect_nondigits(Rest,[X|Acc]);
collect_nondigits(R,Acc) -> {lists:reverse(Acc),R}.


assemble(Parts) -> assemble(Parts,[]).
assemble([],Acc) -> lists:concat(lists:reverse(Acc));
assemble([{X,Y}|Rest], Acc) -> assemble(Rest,[Y,X|Acc]).

incr(X) -> 
    case lists:reverse(X) of
        []            -> [{"1",[]}];
        [{D,ND}|Rest] -> lists:reverse([{incr_num(D),ND}|Rest])
    end.

incr_num(Str) ->
    {Num,[]} = string:to_integer(Str),
    integer_to_list(Num+1).

