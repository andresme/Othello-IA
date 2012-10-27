%%Archivo: othello_player.erl

%%Hecho por:
%%	Andres Morales.
%%	Irene Gamboa.

%%Implementacion de minimax para othello.
%%Progra #2 Inteligencia Artificial.



-module(othello_player).

-export([connect/1]).

connect(Color) ->
	spawn(fun() -> 
			oserver ! {connect, self(), Color},
			receive
				{ok, State} -> io:format("state: ~w~n", [State]),
				eventLoop(Color);
			_ -> io:format("Error")
			end
		end
		).
	
directions() -> [-11,-10,-9,-1,1,9,10,11].

%%Heuristic board
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

%%Main loop, recieves messages from server
eventLoop(Color)->
	receive
	%%In case it is my turn do the alpha beta prunning minimax!
	{your_turn, {_,_,_,_,_,_, _, _, Board, Border}} ->
		Posibles = get_posibles(Border, [], Board, Color, Color),
		[_, Move, _, _] = alpha_beta(Posibles, Color, [-100001, 0, {}, []], Color),
		%%io:format("Answer: ~w~n", [Move]),
		oserver!{move, self(), {Color, Move}},
		eventLoop(Color);
	%%Not my turn just print something :P
	{ok, {_,_,_,_,_,_, _, _, _, _}} ->
		io:format("It's not my turn :(~n"),
		eventLoop(Color);
	%%Any other case:
	_ -> io:format("unhandled message"),
		eventLoop(Color)
	end.

%%Do the alpha beta Initialization
alpha_beta(Childs, Player, Move = [HeurM, _, _, _], Color) ->
	%%time that the algorithm starts, for stopping by an amount of time
	%%and not by Depth Levels.
	{Mega,Sec,Micro} = erlang:now(),
	Depth = (Mega*1000000+Sec)*1000000+Micro,
	case Childs of
		[] -> Move;
		[H|T] -> [Heur, _, _, _]= value(H, Player, -100000, +100000, Color, Depth),
			if Heur >= HeurM ->
				alpha_beta(T, Player, H, Color);
			true -> alpha_beta(T, Player, Move, Color)
			end
	end.

%%alpha beta algorithm (Mini-Max Variant)
value(Node = [_, _, _, Border], Player, Alpha, Beta, Color, Depth) ->
	{Mega,Sec,Micro} = erlang:now(),
	Tiempo = ((Mega*1000000+Sec)*1000000+Micro) - Depth,
	if
		(Border =:= []) or (Tiempo >= 100000)->
			%%io:format("Tiempo: ~w~n", [Tiempo]),
			Node;
		Player =:= Color ->
			max_value(Node, Player, Alpha, Beta, Color, Depth);
		true ->
			min_value(Node, Player, Alpha, Beta, Color, Depth)
	end.
	
%%Calculates max value of level
max_value([_, _, Board, Border], Player, Alpha, Beta, Color, Depth) ->
	V = [-10000, 0, {}, []],
	Childs = get_posibles(Border, [], Board, Player, Color),
	A = calc_value_max(Childs, Player, Alpha, Beta, Color, Depth, V),
	A.
	
%%Iteration over the childs for max.
calc_value_max(Childs, Player, Alpha, Beta, Color, Depth, V = [HeurV, _, _, _]) ->
	case Childs of
		[] -> V;
		[H|T] -> [Heur, _, _, _] = value(H, change(Player), Alpha, Beta, Color, Depth),
			if Heur >= HeurV ->
				NewV = H;
				true -> NewV = V
			end,
			if NewV >= Beta -> calc_value_max(T, Player, Alpha, Beta, Color, Depth, NewV);
				true -> NewAlpha = max(Alpha, NewV),
				calc_value_max(T, Player, NewAlpha, Beta, Color, Depth, NewV)
			end
	end.
	
%%Calculates min value of level
min_value([_, _, Board, Border], Player, Alpha, Beta, Color, Depth) ->
	V = [10000, 0, {}, []],
	Childs = get_posibles(Border, [], Board, Player, Color),
	A = calc_value_min(Childs, Player, Alpha, Beta, Color, Depth, V),
	A.
	
%%Iteration over the childs for min
calc_value_min(Childs, Player, Alpha, Beta, Color, Depth, V = [HeurV, _, _, _]) ->
	case Childs of
		[] -> V;
		[H|T] -> [Heur, _, _, _] =  value(H, change(Player), Alpha, Beta, Color, Depth),
			if Heur =< HeurV ->
				NewV = H;
				true -> NewV = V
			end,
			if NewV =< Alpha -> calc_value_max(T, Player, Alpha, Beta, Color, Depth, NewV);
				true ->
					NewBeta = min(Beta, NewV),
					calc_value_max(T, Player, Alpha, NewBeta, Color, Depth, NewV)
			end
	end.
	
%%get the possible moves on a list
get_posibles(Border, Posibles, Board, Player, Color) -> 
	case Border of
		[] -> Posibles;
		[H|T] -> 
			case othello:check_move(H, Board, othello:directions(), color(Player)) of
				true ->
					New_board = othello:make_move(H, Board, directions(), color(Player)),
					New_border = othello:new_frontier(Border, H, Board),
					PosiblesTemp = [calc_heuristic(New_board, Color), H, New_board, New_border],
					NewPosibles = lists:append(Posibles, [PosiblesTemp]),
					get_posibles(T, NewPosibles, Board, Player, Color);
				_-> get_posibles(T, Posibles, Board, Player, Color)
			end
	end.
	
%%Gets the heuristic value of a board
calc_heuristic(Board, Color) ->
	heuristic(tuple_to_list(Board), tuple_to_list(heuristics()), 0, Color).
	
heuristic([], [], Value, _) -> Value;
heuristic([H|T], [H1|T1], Value, Color) ->
	if 
		(H =:= 1) or (H =:= -1) ->
			NewValue = Value + (H * H1 * color(Color)),
			heuristic(T, T1, NewValue, Color);
		true -> heuristic(T, T1, Value, Color)
	end.

%%misc functions taken from othello.erl:
color(white) ->  1;
color(black) -> -1.

change(white) -> black;
change(black) -> white.
