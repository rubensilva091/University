-module(game).
-export([game/6,sendDead/3,colisionDetectionPlayerPlayerHub/3,colisionDetectionPlayerPlayer/2,colisionMapBorder/1,colisionSun/1,colisionDetectionPlayer/2,colisionDetection/4, updatePlayer/2,updateMob/2, processPlayer/4,initPlayerPos/3, addPlayer/13, sendMob/3,listTuple_to_Command/3, tuple_to_string/1, initMobs/0, generateMobs/2, lastPlayer/2, winCountdown/2]).


%Game function
game(PlayersPID, PlayerList, MobList, GameOn, CountdownPid, GamePid) ->
    receive
        %init game Data
        {start, Players} -> 
            NewPlayerList=initPlayerPos(Players,1,[]),
            NewMobList=initMobs(),
            NewCountdownPid = spawn(fun() -> winCountdown(0, GamePid) end),
            sendMob(Players,"player" ,NewPlayerList),
            sendMob(Players,"mob" ,NewMobList),
            timer:sleep(1),
            game(Players,NewPlayerList,NewMobList,1, NewCountdownPid, GamePid);

        %Update the Cliente
        {updateCliente, Command, ID} ->
            NewPlayerList = processPlayer(Command,ID,PlayerList,[]),
            sendMob(PlayersPID,"player" ,NewPlayerList),
            sendMob(PlayersPID,"mob" ,MobList),
            timer:sleep(1),
            game(PlayersPID,NewPlayerList,MobList,GameOn, CountdownPid, GamePid);
        {win, IdWinner} ->
            sendDead(PlayersPID,IdWinner,"win");
        {draw, IdDrawer} ->
            sendDead(PlayersPID,IdDrawer,"draw")

    %Update the game every 1 ms
    after 1 -> 
        if
            GameOn > 0 ->
                NewPlayerList = colisionDetection(PlayerList,MobList,[],PlayersPID),
                NewPlayerList2 = updatePlayer(NewPlayerList,[]),
                NewPlayerList3 = colisionDetectionPlayerPlayerHub(NewPlayerList2,NewPlayerList2,[]),
                NewMobList = updateMob(MobList,[]),
                sendMob(PlayersPID,"player" ,NewPlayerList3),
                sendMob(PlayersPID,"mob" ,MobList),
                lastPlayer(NewPlayerList3, CountdownPid),
                game(PlayersPID,NewPlayerList3,NewMobList,GameOn, CountdownPid, GamePid);
            true ->
                game(PlayersPID,PlayerList,MobList,GameOn, CountdownPid, GamePid)
        end
    end.
    
%Function to check if there is only one player alive
lastPlayer(PlayerList, CountdownPid)->
    ListSize = length (PlayerList),
    if  
        ListSize == 1 ->
            [H|_]=PlayerList,
            CountdownPid ! {countdown, element(1,H)};
        ListSize == 0 ->
            CountdownPid ! {died};
        true ->
            ok
    end.
            
%Counter to Count 5 seconds after the last player is alive
winCountdown(N, GamePid)->
    receive
        {countdown, IdLastPlayer}->
            if 
                N == 0 ->
                    receive
                        {died} ->
                            GamePid ! {draw, IdLastPlayer}
                        after 5000 ->
                            GamePid ! {win, IdLastPlayer}
                    end;
                true ->
                    ok
            end
    end,
    winCountdown(N+1, GamePid).
            

%Function to process the player input from the client
processPlayer(Command,ID,[H|T], UpdatedList)->
    IDlist = element(1,H),
    case IDlist == ID of
        true ->
            F = list_to_float(element(11,H)),
            case Command of
                "up" -> 
                    Angl = list_to_float(element(7,H)),
                    AngRad = Angl * math:pi() / 180,
                    CosValue = math:cos(AngRad),
                    SinValue = math:sin(AngRad),
                    Rotation = list_to_float(element(12,H)),
                    case F of
                        _ when F < 0.1 ->
                            VeloX = list_to_float(element(5,H)),
                            VeloY = list_to_float(element(6,H)),
                            Fuel = 0.0;
                        _ ->
                            VeloX = list_to_float(element(5,H)) + 0.03*CosValue,
                            VeloY = list_to_float(element(6,H)) + 0.03*SinValue,
                            Fuel = list_to_float(element(11,H))-0.1
                    end;
                "left" -> 
                    VeloX = list_to_float(element(5,H)),
                    VeloY = list_to_float(element(6,H)),
                    case F of
                        _ when F < 0.1 ->
                            Rotation = list_to_float(element(12,H)),
                            Fuel = 0.0;
                        _ ->
                            Rotation = list_to_float(element(12,H))-0.015,
                            Fuel = list_to_float(element(11,H))-0.075
                    end;                   
                "right" -> 
                    VeloX = list_to_float(element(5,H)),
                    VeloY = list_to_float(element(6,H)),
                    case F of
                        _ when F < 0.1 ->
                            Rotation = list_to_float(element(12,H)),
                            Fuel = 0.0;
                        _ ->
                            Rotation = list_to_float(element(12,H))+0.015,
                            Fuel = list_to_float(element(11,H))-0.075
                    end
            end,
            NewH = {ID,element(2,H),
            element(3,H), 
            element(4,H), 
            float_to_list(VeloX), 
            float_to_list(VeloY),
            element(7,H), 
            element(8,H), 
            element(9,H), 
            element(10,H),
            float_to_list(Fuel),
            float_to_list(Rotation)},
            NewUpdatedList = UpdatedList ++ [NewH],
            processPlayer(Command,ID,T,NewUpdatedList);
        false -> 
            NewUpdatedList = UpdatedList ++ [H],
            processPlayer(Command,ID,T,NewUpdatedList)
    end;
processPlayer(_,_,[],UpdatedList)->
    UpdatedList.
    
%Function to update the player position
updatePlayer([H|T],MobList) ->
    VeloX = list_to_float(element(5,H)),
    VeloY = list_to_float(element(6,H)),

    %Gravity Calculation
    %Vetor to center (Center - Player)
    VectorX = 1280/2 - list_to_float(element(2,H)),
    VectorY = 720/2 - list_to_float(element(3,H)),
    %normalize the vector
    Norm = math:sqrt(math:pow(VectorX,2) + math:pow(VectorY,2)),
    PositiveNorm = abs(Norm),
    NormalizedVectorX = VectorX / PositiveNorm,
    NormalizedVectorY = VectorY / PositiveNorm,
    %calculate the Gravity
    GravityX = NormalizedVectorX * 0.001,
    GravityY = NormalizedVectorY * 0.001,

    %Calculate the new velocity with the gravity
    NewVeloX = VeloX + GravityX,
    NewVeloY = VeloY + GravityY,

    %Calculate the new position of the player
    PosX = list_to_float(element(2,H)) + NewVeloX,
    PosY = list_to_float(element(3,H)) + NewVeloY,
    Angl = list_to_float(element(7,H)) + list_to_float(element(12,H)),

    NewH = {element(1,H),
            float_to_list(PosX),
            float_to_list(PosY),
            element(4,H), 
            float_to_list(NewVeloX), 
            float_to_list(NewVeloY), 
            float_to_list(Angl), 
            element(8,H), 
            element(9,H),
            element(10,H),
            element(11,H),
            element(12,H)},
    NewMobList = MobList ++ [NewH],
    updatePlayer(T,NewMobList);
updatePlayer([],MobList) ->
    MobList.

%Function to update the mob/Planets position
updateMob([H|T],MobList)->
    VeloX = list_to_float(element(5,H)),
    VeloY = list_to_float(element(6,H)),

    %Gravity Calculation
    %Vetor to center (Center - Player)
    VectorX = 1280/2 - list_to_float(element(2,H)),
    VectorY = 720/2 - list_to_float(element(3,H)),
    %normalize the vector
    Norm = math:sqrt(math:pow(VectorX,2) + math:pow(VectorY,2)),
    PositiveNorm = abs(Norm),
    NormalizedVectorX = VectorX / PositiveNorm,
    NormalizedVectorY = VectorY / PositiveNorm,
    %calculate the Gravity
    GravityX = NormalizedVectorX * 0.002,
    GravityY = NormalizedVectorY * 0.002,

    %Calculate the new velocity with the gravity
    NewVeloX = VeloX + GravityX,
    NewVeloY = VeloY + GravityY,

    %Calculate the new position of the player
    PosX = list_to_float(element(2,H)) + NewVeloX,
    PosY = list_to_float(element(3,H)) + NewVeloY,

    NewH = {element(1,H),
            float_to_list(PosX),
            float_to_list(PosY),
            element(4,H), 
            float_to_list(NewVeloX), 
            float_to_list(NewVeloY)},
    NewMobList = MobList ++ [NewH],
    updateMob(T,NewMobList);
updateMob([],MobList) ->
    MobList.


%init all Players in pre-defined positions
% id, PosX, PosY, Size, VelocityX, VelocityY, Angle, R, G, B   Fuel  Rotation
%  1    2     3     4       5         6          7   8  9  10  1000      0
initPlayerPos([H|T],N,PlayerList)->
    case N of
        1 -> 
            NewPlayerList=addPlayer(PlayerList,"1", "100.0", "100.0", "50.0", "0.0","0.0", "30.0", "250","0", "0", "100.0", "0.0");
        2 -> 
            NewPlayerList=addPlayer(PlayerList, "2","1200.0", "650.0", "50.0", "0.0","0.0", "210.0", "0","250", "0", "100.0", "0.0");
        3 -> 
            NewPlayerList=addPlayer(PlayerList, "3","100.0", "650.0", "50.0","0.0","0.0" ,"300.0", "0","0", "250", "100.0","0.0");
        4 -> 
            NewPlayerList=addPlayer(PlayerList, "4","1200.0", "100.0", "50.0", "0.0","0.0" ,"120.0", "250","0", "250", "100.0","0.0")
    end,
    initPlayerPos(T,N+1,NewPlayerList);
initPlayerPos([],_,PlayerList)->
    PlayerList.

initMobs() ->
    Random = rand:uniform(7) + 5,
    MobList = generateMobs(Random,[]),
    MobList.


%generate all mobs randomly
% id, PosX, PosY, Size, VelocityX, VelocityY
%  1    2     3     4       5         6      
generateMobs(0,MobList)->
        MobList;
generateMobs(N,MobList)->
        %Generate the random PosX and PosY
        Num1 = round(rand:uniform()),
        Num2 = rand:uniform(230) + 180,  
        Num3 = rand:uniform(421) + 150,   

        PosX = Num2 + (Num1*640),
        PosY = Num3,

        %Generate the random Size, Velocity and Angle
        Size = rand:uniform(32) + 24, 
        VelocityX = (rand:uniform(4) - 2) / 3,
        VelocityY = (rand:uniform(4) - 2) / 3,


        Mob = {integer_to_list(N),
            float_to_list(float(round(PosX))), 
            float_to_list(float(round(PosY))), 
            float_to_list(float(Size)), 
            float_to_list(float(VelocityX)),
            float_to_list(float(VelocityY))},
        NewMobList=MobList++[Mob],
        generateMobs(N-1,NewMobList).


%Function to add a mob to the list
addPlayer(Lista, Id,PosX, PosY, Size, VelocityX, VelocityY, Angle, R, G, B, Fuel, Rotation) ->
    Player = {Id,PosX, PosY, Size, VelocityX,VelocityY, Angle, R ,G ,B, Fuel, Rotation},
    Lista ++ [Player].

colisionDetection([H|T],MobList, NewPlayerList, PlayersPID) ->
    case colisionDetectionPlayer(H,MobList) of
        dead ->
            %Send a msn to the player
            sendDead(PlayersPID,element(1,H), "lose"),
            colisionDetection(T,MobList,NewPlayerList, PlayersPID);
        ok ->               
            case colisionMapBorder(H) of
                dead ->
                    sendDead(PlayersPID,element(1,H),"lose"),
                    colisionDetection(T,MobList,NewPlayerList, PlayersPID);
                ok ->
                    case colisionSun(H) of
                        dead ->
                            sendDead(PlayersPID,element(1,H), "lose"),
                            colisionDetection(T,MobList,NewPlayerList, PlayersPID);
                        ok ->
                            NewPlayerList2 = NewPlayerList ++ [H],
                            colisionDetection(T,MobList,NewPlayerList2, PlayersPID)
                    end
            end
    end;
colisionDetection([], _, NewPlayerList, _) ->
    NewPlayerList.

colisionMapBorder(Player) ->
    %Obtain the pos of the player
    PosPlayerX = list_to_float(element(2,Player)),
    PosPlayerY = list_to_float(element(3,Player)),

    %Check if the player is inside the map
    if
        PosPlayerX  < 0 -> 
            dead;
        PosPlayerX > 1280 -> 
            dead;
        PosPlayerY < 0 -> 
            dead;
        PosPlayerY > 720 -> 
            dead;
        true -> 
            ok
    end.

colisionSun(Player) ->
    %Obtain the pos of the player and the Sun
    PosPlayerX = list_to_float(element(2,Player)),
    PosPlayerY = list_to_float(element(3,Player)),
    PosSunX = 1280/2,
    PosSunY = 720/2,

    %Obtain the size of the player and the Sun in minimum range
    SizeSun = 150,
    SizePlayer = list_to_float(element(4,Player)),
    SizeCombined = SizeSun + SizePlayer,

    %Calcute the distance between Sun and the player
    Distance = math:sqrt(math:pow(PosPlayerX - PosSunX,2) + math:pow(PosPlayerY - PosSunY,2))+25,
    %Check if the player is inside the Sun
    if
        Distance > SizeCombined ->
            ok;
        true ->
            dead
    end.

colisionDetectionPlayer(Player, [H|T])->
    %Collision Detection Player and Planet
    %Obtain the pos of the player and the planet
    PosPlayerX = list_to_float(element(2,Player)),
    PosPlayerY = list_to_float(element(3,Player)),
    PosPlanetX = list_to_float(element(2,H)),
    PosPlanetY = list_to_float(element(3,H)),

    %Obtain the size of the player and the planet in minimum range
    SizePlanet = list_to_float(element(4,H)),
    SizePlayer = list_to_float(element(4,Player)),
    SizeCombined = SizePlanet + SizePlayer,

    %Calcute the distance between planet and the player
    Distance = math:sqrt(math:pow(PosPlayerX - PosPlanetX,2) + math:pow(PosPlayerY - PosPlanetY,2))+25,


    %Check if the player is inside the planet
    if
        Distance > SizeCombined ->
            colisionDetectionPlayer(Player,T);
        true ->
            dead
    end;
colisionDetectionPlayer(_,[])->
    ok.

colisionDetectionPlayerPlayer([H|T],Player)->
    %Collision Detection Player and Player
    IdPlayer = list_to_integer(element(1,Player)),
    IdPlayer2 = list_to_integer(element(1,H)),
    %Obtain the pos of the player1 and the player2
    PosPlayer1X = list_to_float(element(2,Player)),
    PosPlayer1Y = list_to_float(element(3,Player)),
    PosPlayer2X = list_to_float(element(2,H)),
    PosPlayer2Y = list_to_float(element(3,H)),

    %Obtain Velocity of player1 and player2 to swap if needed for the elastic collision
    VeloPlayer1X = list_to_float(element(5,Player)),
    VeloPlayer1Y = list_to_float(element(6,Player)),

    %Obtain the size of the player1 and the player2 in minimum range
    SizePlayer1 = list_to_float(element(4,Player)),
    SizePlayer2 = list_to_float(element(4,H)),
    SizeCombined = SizePlayer1 + SizePlayer2,

    %Calcute the distance between player1 and the player2
    Distance = math:sqrt(math:pow(PosPlayer1X - PosPlayer2X,2) + math:pow(PosPlayer1Y - PosPlayer2Y,2))+50,

    if 
        Distance > SizeCombined ->
            colisionDetectionPlayerPlayer(T,Player);
        true ->
            if  
                IdPlayer /= IdPlayer2 ->
                    NewPlayer = {element(1,H),
                    element(2,H),
                    element(3,H),
                    element(4,H), 
                    float_to_list(VeloPlayer1X), 
                    float_to_list(VeloPlayer1Y), 
                    element(7,H),
                    element(8,H), 
                    element(9,H),
                    element(10,H),
                    element(11,H),
                    element(12,H)},
                    colisionDetectionPlayerPlayer([],NewPlayer);
                true ->
                    colisionDetectionPlayerPlayer(T,Player)
            end
    end;
colisionDetectionPlayerPlayer([],Player)->
    Player.

%Tenho de ciclar toda a lista again ://
colisionDetectionPlayerPlayerHub([H|T], PlayerList, NewPlayerList) ->
    NewPlayer = colisionDetectionPlayerPlayer(PlayerList,H),
    NewPlayerList2 = NewPlayerList ++ [NewPlayer],
    colisionDetectionPlayerPlayerHub(T,PlayerList,NewPlayerList2);
colisionDetectionPlayerPlayerHub([],_,NewPlayerList) ->
    NewPlayerList.

%Function to send the Dead Player signal to the Clients
sendDead([H|T], Id, Flag) ->
    H ! {dead,Id, Flag},
    sendDead(T,Id,Flag);
sendDead([],_,_) ->
    ok.

%Function to send the mobs to the Clients
sendMob([H|T], Token,MobList) ->
    StringMobList = listTuple_to_Command(MobList,Token,""),
    H ! {mobs,StringMobList},
    sendMob(T,Token,MobList);
sendMob([],_,_) ->
    ok.

%Function to convert a list of tuples to a string
listTuple_to_Command([H|T], Token,Str) ->
    NewStr = Str ++ Token ++ " " ++ tuple_to_string(H)++"\n",
    listTuple_to_Command(T,Token,NewStr);
listTuple_to_Command([],_,Str) ->
    Str.

%Function to convert a tuple to a string
tuple_to_string(Tuple) ->
    lists:flatten(string:join(tuple_to_list(Tuple), " ")).



