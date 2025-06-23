-module(fileFuns).
-export([readFile/0, writeFile/1, deleteFileParcialy/1, deleteLine/3]).

%cd("C:\\Users\\ruben\\Desktop\\Minho\\PC\\Projeto\\server\\").

% This function reads the file users.txt and returns a list with the users. If the file doesn't exist, it creates the file and calls the function again.
readFile() ->

    %Read the file "users.txt"
    case file:read_file("users.txt") of
        {ok, Binary} ->

            %Create list of users
            Lines = binary_to_list(Binary),

            %Remove the \r from the end of the lines
            LinesList = lists:map(fun(Line) -> string:strip(Line, right, $\r) end, string:tokens(Lines, "\n")),
            LinesList;

        {error, enoent} ->
            %Create the file "users.txt"
            case file:open("users.txt", [write]) of
                {ok, File} ->
                    file:close(File),
                    readFile(); % Go to the function to read the file again
                {error, Reason} ->
                    io:format("Error Creating File: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("Error Reading the File: ~p~n", [Reason])
    end.


%Function to append a user to the file
writeFile(Str) ->
    case file:open("users.txt", [write, append]) of
        {ok, Device} ->
            io:format(Device, "~n~s", [Str]),
            file:close(Device);
        {error, Reason} ->
            io:format("Error opening file: ~p", [Reason])
    end.

%Fuction to rescreve the file without the user
deleteFileParcialy(User) ->
    Users = readFile(),
    case file:open("users.txt", [write]) of
        {ok, Device} ->
            FileWithoutLine = deleteLine(Users, User, []),
            io:format(Device, "~s", [string:join(FileWithoutLine, "\n")]);
        {error, Reason} ->
            io:format("Error opening file: ~p", [Reason])
    end.

%Function to restruct the list of users without the user selected
deleteLine([H|T], User, List) ->
    [Username, _, _, _] = string:split(H, " ", all),
    case Username == User of
        true ->
            deleteLine(T, User, List);
        false ->
            deleteLine(T, User, List++[H])
    end;
deleteLine([], _, List) ->
    List.