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
findNextMoves(JN,[C1,C2,C3,C4,C5,C6,C7],[X1,X2,X3,X4,X5,X6,X7]):- 
addChips(JN,C1,NewC1),
addChips(JN,C2,NewC2),
addChips(JN,C3,NewC3),
addChips(JN,C4,NewC4),
addChips(JN,C5,NewC5),
addChips(JN,C6,NewC6),
addChips(JN,C7,NewC7),
X1 is [NewC1,C2,C3,C4,C5,C6,C7],
X2 is [C1,NewC2,C3,C4,C5,C6,C7],
X3 is [C1,C2,NewC3,C4,C5,C6,C7],
X4 is [C1,C2,C3,NewC4,C5,C6,C7],
X5 is [C1,C2,C3,C4,NewC5,C6,C7].
X6 is [C1,C2,C3,C4,C5,NewC6,C7].
X7 is [C1,C2,C3,C4,C5,C6,NewC7].

addChips(JN,X,NewX):- not(member('_',X)),NewX is X.
addChips(JN,X,NewX):- split(X,'_',L,R),append(L,[JN],C),append(C,R,NewX).

/**
*	calcPoints(X, JN, AIN, Value), where X = the game state to be evaluated,
*	JN = the player to make the move, AIN = the player represented by the AI
*	and Value = The value of the game state according to the heuristic
*	implemented in this method.
**/
%scoreLigne calcule le score d'une ligne (List) de n'importe quelle taille et l'ajoute à (POINTS) pour donner le résultat final (FINAL) LEFT et RIGHT sont des variables utiles pour la récurence

scoreLigne(List, Pivot, Left, Right, POINTS,FINAL) :- not(member(Pivot, List)),not(member('_', List)),calcPoints(List,POINTS,FINAL).
scoreLigne(List, Pivot, Left, Right, POINTS,FINAL) :- decoup(List, Pivot, Left, Right),calcPoints(Left,POINTS,NEWPOINT),scoreLigne(Right,Pivot,X,Y,NEWPOINT,FINAL).

%split amélioré splutant les '_'

decoup(List, Pivot, Left, Right):- split(List, Pivot, Left, Right),not(member('_', Left)).
decoup(List, Pivot, Left, Right):-split(List, '_', Left, Right).

%split permet de separer un liste en une liste a gauche du pivot et une liste à droite du pivot

split(List, Pivot, Left, Right) :- append(Left, [Pivot|Right], List).

%calcPoints incrémente le nombre de points en fonction de la taille de l'alignement

calcPoints(List,TOT,NEWTOT):-length(List,M), NEWTOT is TOT +M*M-M.
% TODO TODO TODO TODO TODO TODO TODO 
%heuri une fonction qui donne la valeur de la board board

heuri(BOARD,SCORE,Pivot):-scoreLigne([X46,X55,X64,X73], Pivot, L1, R1, 0,FINAL1),scoreLigne(diagoTODO, Pivot, L2, R2, FINAL1,FINAL2),TODO

% TODO TODO TODO TODO TODO TODO TODO 


% MES TESTS
%[o,x,x,x,o,x,o]
% not(member(x, [o,x,'_',x,o,x,o]))
% split([o,x,x,x,o,x,o], x, Left, Right).
% not(member(x, [x,x,o,x,o])).
% scoreLigne([o,o,'_',x,o,o,o], x, Left, Right,0,X).
% calcPoints([a,b,c],X,tampon).

/**
*	Modified isEOG(X, JN), where we can return the value of the player who has
*	won (or a specified value for a game which ends as a draw) within the
*	variable JN.
**/

/**
*	The alphabeta itself which will be a pain i the ass! :(
**/
