AIconst(X, o).

play(NewStates, CurrentState, PlayerNumber) :-
	findMoves(CS, NSS).

alphabeta(Value,State,Depth,Alpha,Beta,PlayerNumber) :-
	abifstat(VALUE,X,D,A,B,JN).

alphabeta(VALUE,X,D,A,B,o) :-

alphabeta(VALUE,X,D,A,B,x) :-

abifstat(VALUE,X,0,A,B,JN) :-
	calcPoints(X,JN,VALUE).
abifstat(VALUE,X,D,A,B,JN) :-
	isEOG(X,JN),
	calcPo

/*FUNCTIONS WE NEED TO IMPLEMENT*/

/**
*	findNextMoves(X, JN, XL), where X = current game state, JN = player number
*	('o' or 'x') and XL is a list of all the possible states after the current
*	move.
**/

/**
*	calcPoints(X, JN, AIN, Value), where X = the game state to be evaluated,
*	JN = the player to make the move, AIN = the player represented by the AI
*	and Value = The value of the game state according to the heuristic
*	implemented in this method.
**/

/**
*	Modified isEOG(X, JN), where we can return the value of the player who has
*	won (or a specified value for a game which ends as a draw) within the
*	variable JN.
**/

/**
*	The alphabeta itself which will be a pain i the ass! :(
**/
