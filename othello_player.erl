%%Archivo: othello_player.erl

%%Hecho por:
%%	Andres Morales.
%%	Irene Gamboa.

%%Implementacion de minimax para othello.
%%Progra #2 Inteligencia Artificial.



-module(othello_player).

-export([connect/1]).

connect(Color) ->
	register(
		oplayer,
		spawn(fun() -> 
			oserver ! {connect, self(), Color},
			receive
				{ok, State} -> io:format("state: ~w~n", [State]),
				eventLoop(Color);
			_ -> io:format("Error")
			end
		end
		)
	).

eventLoop(Color)->
	receive
	{your_turn, {_,_,_,_,_,_, Timer, _, Board, Border}} ->
		Answer =
		io:format("My turn, should do something here!~nState: ~w~n", [Posibles]),
		eventLoop(Color);
	{ok, {_,_,_,_,_,_, Timer, _, Board, Posibles}} ->
		io:format("It's not my turn :(~nState: ~w~n", [Posibles]),
		eventLoop(Color);
	{move, Pos} ->
		move(Pos, Color),
		eventLoop(Color);
	_ -> io:format("Error"),
		eventLoop(Color)
	end.

move(Pos, Color) ->
	oserver!{move, self(), {Color, Pos}}.


alpha_beta(Node, Depth, Alpha, Beta, Player) ->
	Childs = get_posibles(Border, [], Board, Color),
	case Depth of
		0 -> heuristic(Node)
	end,
	case Childs of
		[] -> heuristic(Node)
	end,
	case Player of
		true -> 
			Alpha_list = lists:map(fun(X)->alpha_beta(X, Depth-1, Alpha, Beta, not(Player)) end, Childs),
			New_alpha = lists:max(Alpha_list),
			if New_alpha < Beta ->
				ok;
				true -> New_alpha
			end;
		_-> Beta_list = lists:map(fun(X)->alpha_beta(X, Depth-1, Alpha, Beta, not(Player)) end, Childs),
			New_beta = lists:min(Beta_list),
			if New_beta < Alpha ->
				ok;
				true -> New_beta
			end
	end.


heuristic(Node) -> 0.
get_childs(Node, Player) -> [].


get_posibles(Border, Posibles, Board, Player) -> 
	case Border of
		[] -> Posibles;
		[H|T] -> 
			case othello:check_move(H, Board, othello:directions(), color(Player)) of
				true -> NewPosibles = lists:append(Posibles, [H]),
					get_posibles(T, NewPosibles, Board, Player);
				_-> get_posibles(T, Posibles, Board, Player)
			end
	end.

color(white) ->  1;
color(black) -> -1.
