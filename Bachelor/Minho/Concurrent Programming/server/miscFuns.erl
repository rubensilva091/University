-module(miscFuns).
-export([parseTriple/1, isUserStored/3, top10Players/0]).
-import(fileFuns, [readFile/0]).

%Function to parse triple
parseTriple(Str) ->
    Data = re:replace(Str,"\\n|\\r|", " ",[global,{return,list}]),
    ParsedData = string:split(Data, " ", all),
    ParsedData.

%Function to check if user exists
isUserStored([Head|Tail], User, Pass) ->
    [Username, Password, Level, Streak] = string:split(Head, " ", all),
    case Username == User andalso Password == Pass of
        true ->
            {ok, Level, Streak};
        false ->
            case Username == User andalso Password /= Pass of
                true ->
                    {error, 0};
                false ->
                    isUserStored(Tail, User, Pass)
            end
    end;

isUserStored([], _, _) ->
    {error, "User not found"}.

top10Players()->
    %Users = [[Username, Password, Level, Streak],...]
    Users = readFile(),
    %create a list of the top 10 players with most levels and then per streak
    Top10 = lists:reverse(lists:sort(fun(A,B) ->
        [_, _, LevelA, StreakA] = string:split(A, " ", all),
        [_, _, LevelB, StreakB] = string:split(B, " ", all),
        case list_to_integer(LevelA) < list_to_integer(LevelB) of
            true -> true;
            false ->
                case LevelA == LevelB of
                    true -> list_to_integer(StreakA) < list_to_integer(StreakB);
                    false -> false
                end
        end
    end, Users)),
    Top10,
    Top10Players = filterTop10(Top10, 10, []),
    Top10Players.


filterTop10(_, 0, List)->
    List;
filterTop10([H|T], N, List)->
    NewList = List ++[H],
    filterTop10(T, N-1, NewList).

