-module(lobby).
-import(game, [game/6]).
-export([lobby_manager/2, wake_Game/3]).

lobby_manager(Players, PlayersPidReady) ->
    N = length(PlayersPidReady),
    if
        N > 1 ->
            io:format("Lobby with 2 Players Ready~n"),
            if
                N == 4 ->
                    io:format("Lobby with 4 Players Ready~n"),
                    GamePid = spawn(fun() -> game([], [], [], 0, -1, self()) end),
                    wake_Game(PlayersPidReady, GamePid, 1),
                    PlayersForGame = filter(PlayersPidReady),
                    GamePid ! {start, PlayersForGame},
                    lobby_manager(Players, []);
                true ->
                    receive
                        {hi, Pid, Level} ->
                            [{_, Level2} | _] = PlayersPidReady,
                            IntLevel1 = list_to_integer(Level),
                            IntLevel2 = list_to_integer(Level2),
                            LevelDif = abs(IntLevel1 - IntLevel2),
                            if
                                LevelDif =< 1 ->
                                    case lists:member({Pid, Level}, PlayersPidReady) of
                                        true ->
                                            lobby_manager(Players, PlayersPidReady);
                                        false ->
                                            NewPlayersPidReady = PlayersPidReady ++ [{Pid, Level}],
                                            lobby_manager(Players, NewPlayersPidReady)
                                    end;
                                true ->
                                    NewPlayersList = Players ++ [{Pid, Level}],
                                    lobby_manager(NewPlayersList, PlayersPidReady)
                            end
                    after 5000 ->
                        GamePid = spawn(fun() -> game([], [], [], 0, -1, self()) end),
                        wake_Game(PlayersPidReady, GamePid, 1),
                        PlayersForGame = filter(PlayersPidReady),
                        GamePid ! {start, PlayersForGame},
                        lobby_manager(Players, [])
                    end
            end;
        true ->
            receive
                {hi, Pid, Level} ->
                    NewPlayers = Players ++ [{Pid, Level}],
                    lobby_manager(NewPlayers, PlayersPidReady)
            after 1000 ->
                {NewPlayers, NewPlayersPidReady} = matchPlayersNotReady(
                    Players, Players, PlayersPidReady
                ),
                lobby_manager(NewPlayers, NewPlayersPidReady)
            end
    end.

matchPlayersNotReady([], Players, PlayersPidReady) ->
    {Players, PlayersPidReady};
matchPlayersNotReady([H | T], Players, PlayersPidReady) ->
    case findMatch(H, Players) of
        noPlayer ->
            matchPlayersNotReady(T, Players, PlayersPidReady);
        [{Pid1, Level1}, {Pid2, Level2}] ->
            NewPlayerList = removePlayerList({Pid1, Level1}, Players, []),
            NewPlayerList2 = removePlayerList({Pid2, Level2}, NewPlayerList, []),
            NewPlayerPidReady = PlayersPidReady ++ [{Pid1, Level1}, {Pid2, Level2}],
            matchPlayersNotReady([], NewPlayerList2, NewPlayerPidReady)
    end.

filter([{Pid, _} | T]) ->
    [Pid | filter(T)];
  filter([]) ->
    [].

findMatch(_, []) ->
    noPlayer;
findMatch(Player, [H|T]) ->
    case Player of
        {Pid, Level} ->
            case H of
                {Pid2, Level2} ->
                    IntLevel1 = list_to_integer(Level),
                    IntLevel2 = list_to_integer(Level2),
                    if
                        abs(IntLevel1 - IntLevel2) < 2 andalso Pid /= Pid2 ->
                            [{Pid, Level}, {Pid2, Level2}];
                        true ->
                            findMatch(Player, T)
                    end
            end
    end.

removePlayerList(_, [], Newlist) ->
    Newlist;
removePlayerList(Player, [H | T], List) ->
    case Player of
        {Pid, _} ->
            case H of
                {Pid2, _} ->
                    if
                        Pid == Pid2 ->
                            removePlayerList(Player, T, List);
                        true ->
                            NewList = List ++ [H],
                            removePlayerList(Player, T, NewList)
                    end
            end
    end.

wake_Game([H | T], GamePid, IdPlayer) ->
    {Pid, _} = H,
    Pid ! {start, GamePid, IdPlayer},
    wake_Game(T, GamePid, IdPlayer + 1);
wake_Game([], _, _) ->
    io:format("Enjoy the game !~n").
