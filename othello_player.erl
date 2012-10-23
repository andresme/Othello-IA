%%Archivo: othello_player.erl

%%Hecho por:
%%	Andres Morales.
%%	Irene Gamboa.

%%Implementacion de minimax para othello.
%%Progra #2 Inteligencia Artificial.



-module(othello_player).

-export([start/0]).

start() ->
	register(
		oplayer,
		spawn(fun() -> 
			eventLoop()
			end
		)
	).

eventLoop()->
	receive
	{connect, Color} ->
		oserver ! {connect, self(), Color},
		receive
			{ok, State} -> io:format("state: ~w~n", [State]);
			_ -> io:format("Error")
		end,
		eventLoop();
	{your_turn, State} ->
		io:format("My turn, should do something here!~nState: ~w~n", [State]),
		eventLoop();
	{ok, State} ->
		io:format("It's not my turn :(~nState: ~w~n", [State]),
		eventLoop();
	_ -> io:format("Error"),
		eventLoop()
	end.

