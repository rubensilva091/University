-module(server).
-import(fileFuns, [readFile/0, writeFile/1, deleteFileParcialy/1]).
-import(miscFuns, [parseTriple/1, isUserStored/3, top10Players/0]).
-import(lobby, [lobby_manager/2]).
-import(comunication, [speakClient/0, listenClient/2]).
-import(game, [game/6, listTuple_to_Command/3]).
-export([start/1, server/1, stop/1, waitConnection/1, process_login/1, loop/6]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

%Function to stop the server
stop(Server) -> Server ! stop.

%Function that will start the server
server(Port) ->
    case  gen_tcp:listen(Port, [{active, false}, {reuseaddr, true}]) of
        {ok, Socket} ->
            register(lobby,spawn(fun() -> lobby_manager([],[]) end)), 
            register(comunicator,spawn(fun() -> speakClient() end)), 
            waitConnection(Socket);
        {error, Reason} ->
            io:format("Error Server: ~p~n", [Reason])
    end.

%Function that will wait for N connections of clients (Maximum 4)
waitConnection(Socket) ->
        case gen_tcp:accept(Socket) of 
            {ok, NewSocket} ->
                %This Spawn will wait for other client to connect it
                %Then, we can garantee that the server will always have a Thread per user
                spawn(fun() -> waitConnection(Socket) end), 
                process_login(NewSocket);
            {error, Reason} ->
                io:fwrite("Error in waitConnection: ~p~n", [Reason])
        end.

process_login(Socket) ->
    Users = readFile(),
    sendTop10(Socket, top10Players()),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %Filter the listen of the client (login page)
            case parseTriple(Data) of
                ["CreateAcc", User, Pass] ->
                    case isUserStored(Users, User, Pass) of
                        %in case of user already exists and password is right
                        {ok, _} ->
                            process_login(Socket);
                        %in case of user already exists and password is not right
                        {error, 0} ->
                            process_login(Socket);
                        %in case of user does not exists
                        {error, _} ->

                            % User Pass Level Streak
                            NewUser = string:join([User, Pass, "1", "0"], " "),
                            writeFile(NewUser),
                            process_login(Socket)
                    end;
                ["LoginAcc", User, Pass] ->
                    case isUserStored(Users, User, Pass) of
                        %in case of user already exists and password is right
                        {ok, Level, Streak} ->
                            lobby ! {hi, self(), Level},  %Send a message to the lobby
                            comunicator ! {Socket, "lobby\n"}, %Send a message to the client that he is waiting for a game
                            receive 
                                {start, GamePid, IdPlayer} ->
                                    %Send a message to the client that we are READY!!!
                                    comunicator ! {Socket,"game\n"},
                                    comunicator ! {Socket, "id "++ integer_to_list(IdPlayer)++"\n"}
                            end,

                            spawn(fun() -> listenClient(Socket, GamePid) end),
                            
                            %The idPlayer is 0 For a "dummy" player before a real 1 is registered
                            loop(Socket,User,Pass,Level,Streak,IdPlayer);

                        %in case of user already exists and password is not right
                        {error, 0} ->
                            process_login(Socket);
                        %in case of user does not exists
                        {error, Reason} ->
                            io:format("~s~n", [Reason]),
                            process_login(Socket)
                    end;
                ["DeleteAcc", User, Pass] ->
                    case isUserStored(Users, User, Pass) of
                        %in case of user already exists and password is right
                        {ok, _, _} ->
                            deleteFileParcialy(User),
                            process_login(Socket);
                        %in case of user already exists and password is not right
                        {error, 0} ->
                            io:format("Wrong Password!~n"),
                            process_login(Socket);
                        %in case of user does not exists
                        {error, Reason} ->
                            io:format("~s~n", [Reason]),
                            process_login(Socket)
                    end;
                _ ->
                    io:format("Failed to parse data.~n"),
                    process_login(Socket)
            end;
        {error, Reason} ->
            io:format("Error receiving data from client: ~p~n", [Reason])
    end.

loop(Socket,User,Pass,Level,Streak,IdPlayer) ->
    receive
        {mobs, Str} ->
            %send player or planet or something else...
            comunicator ! {Socket,Str};%string already processed in game.erl
        {dead, Id, Flag} ->
            %Flag = "lose" or "win" or "draw"
            comunicator ! {Socket, "dead "++ Id ++" "++Flag++"\n"},

            IdPlayerStr = integer_to_list(IdPlayer),
            if 
                IdPlayerStr == Id ->
                    {NewLevel,NewStreak} = newLevelStreakCalc(Level,Streak ,Flag),
                    deleteFileParcialy(User),
                    NewUser = string:join([User, Pass, NewLevel, NewStreak], " "),
                    writeFile(NewUser);
                true ->
                    ok
            end;
        {_} -> 
            io:format("Command not recognized~n")
    end,
    loop(Socket,User,Pass,Level,Streak,IdPlayer).


newLevelStreakCalc(Level,Streak,Flag)->
    StreakInt = list_to_integer(Streak),
   
    %For Summing or reset the streak:
    case Flag of 
        "lose" -> 
            if
                StreakInt < 1 -> 
                    NewStreakInt = StreakInt -1 ;
                true -> 
                    NewStreakInt = 0
            end;
            
        "win" -> 
            if
                StreakInt > - 1 -> 
                    NewStreakInt = StreakInt + 1 ;
                true -> 
                    NewStreakInt = 0
            end;
        "draw" -> 
            NewStreakInt = StreakInt
    end,

    %For Losing or Winning Level:
    AbsStreak = abs(NewStreakInt),
    case Flag of
        "lose" ->
            LevelLosing = round(list_to_integer(Level)/2),
            if
                LevelLosing-1 < AbsStreak ->
                    NewLevel = round(list_to_integer(Level)-1),
                    if 
                        NewLevel > 0 ->
                            {integer_to_list(NewLevel),"0"};
                        true ->
                            {"1","0"}
                    end;
                true ->
                    NewLevel = Level,
                    {NewLevel,integer_to_list(NewStreakInt)}
            end;
            
        "win" ->
            LevelInt = list_to_integer(Level),
            if
                LevelInt < AbsStreak-1 ->
                    NewLevel = round(list_to_integer(Level)+1),
                    {integer_to_list(NewLevel),"0"};
                true ->
                    NewLevel = Level,
                    {NewLevel,integer_to_list(NewStreakInt)}    
            end;
        "draw" ->
            {Level,integer_to_list(NewStreakInt)}
    end.

sendTop10(Socket, [H|T]) ->
    [User, _, Level, Streak] = string:tokens(H, " "),
    ParsedTop10 = string:join([User, Level, Streak], " "),
    comunicator ! {Socket, "leaderboard "++ ParsedTop10++"\n"},
    sendTop10(Socket, T);
sendTop10(_, []) -> ok.