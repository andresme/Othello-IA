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
	
directions() -> [-11,-10,-9,-1,1,9,10,11].

heuristics() ->
		{0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		0,200,-20, 20,  5,  5, 20,-20,200,  0,
		0,-20,-40, -5, -5, -5, -5,-40,-20,  0,
		0, 20, -5, 15,  3,  3, 15, -5, 20,  0,
		0,  5, -5,  3,  3,  3,  3, -5,  5,  0,
		0,  5, -5,  3,  3,  3,  3, -5,  5,  0,
		0, 20, -5, 15,  3,  3, 15, -5, 20,  0,
		0,-20,-40, -5, -5, -5, -5,-40,-20,  0,
		0,200,-20, 20,  5,  5, 20,-20,200,  0,
		0,  0,  0,  0,  0,  0,  0,  0,  0,  0}.

eventLoop(Color)->
	receive
	{your_turn, {_,_,_,_,_,_, Timer, _, Board, Border}} ->
		Answer = alpha_beta_init(Board, Border, 3, -100000, 100000, Color),
		io:format("My turn, should do something here!~nState: ~w~n", [Answer]),
		eventLoop(Color);
	{ok, {_,_,_,_,_,_, Timer, _, Board, Border}} ->
		io:format("It's not my turn :(~nState: ~w~n", [Border]),
		eventLoop(Color);
	{move, Pos} ->
		move(Pos, Color),
		eventLoop(Color);
	_ -> io:format("Error"),
		eventLoop(Color)
	end.

move(Pos, Color) ->
	oserver!{move, self(), {Color, Pos}}.

alpha_beta_init(Board, Border, Depth, Alpha, Beta, Player) ->
	Childs = get_posibles(Border, [], Board, Player),
	Solution = lists:max(lists:map(fun(X) -> alpha_beta(X, Depth, Alpha, Beta, Player) end, Childs)).

alpha_beta(Node = {Pos, Board, Border}, Depth, Alpha, Beta, Player) ->
	Childs = get_childs(Node, Player),
	if Depth == 0 ->
			%%io:format("aqui1~n"),
			calc_heuristic(Node);
		Childs == [] ->
			%%io:format("aqui2~n"),
			calc_heuristic(Node);
		true ->
			case Player of
				true -> %%io:format("player: ~w~n", [Alpha]),
					Alpha_list = lists:map(fun(X) -> alpha_beta(X, Depth-1, Alpha, Beta, change(Player)) end, Childs),
					New_alpha = lists:max(Alpha_list),
					if New_alpha < Beta ->
						ok;
						true -> New_alpha
					end;
				_-> %%io:format("player: ~w~n", [Beta]),
					Beta_list = lists:map(fun(X) -> alpha_beta(X, Depth-1, Alpha, Beta, change(Player)) end, Childs),
					New_beta = lists:min(Beta_list),
					if New_beta < Alpha ->
						ok;
						true -> New_beta
					end
			end
	end.


calc_heuristic(Node = {_, Board, _}) -> 
	heuristic(tuple_to_list(Board), tuple_to_list(heuristics()), 0).
	
heuristic(Board = [H|T], [H1|T1], Value) ->
	case Board of
		[] -> Value;
		[H|T] -> 
		if 
			H == 1 ->
				NewValue = Value + (H * H1),
				heuristic(T, T1, NewValue);
			true -> heuristic(T, T1, Value)
		end
	end.


get_childs(Node = {Pos, Board, Border}, Player) -> 
		New_board = othello:make_move(Pos, Board, directions(), Player),
		New_border = othello:new_frontier(Border, Pos, New_board),
		io:format("=========================~n"),
		othello:print_board(tuple_to_list(New_board), 1, New_border),
		io:format("=========================~n"),
		get_posibles(New_border, [], New_board, Player).


get_posibles(Border, Posibles, Board, Player) -> 
	case Border of
		[] -> Posibles;
		[H|T] -> 
			case othello:check_move(H, Board, othello:directions(), color(Player)) of
				true -> NewPosibles = lists:append(Posibles, [{H, Board, Border}]),
					get_posibles(T, NewPosibles, Board, Player);
				_-> get_posibles(T, Posibles, Board, Player)
			end
	end.

color(white) ->  1;
color(black) -> -1.

change(white) -> black;
change(black) -> white.
