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


%heuri une fonction qui donne la valeur de la board board
heuri(BOARD,SCORE,Pivot):-
checkDiagoScore1(BOARD,0,FINAL1, Pivot),
checkDiagoScore2(BOARD,FINAL1,FINAL2, Pivot),
checkDiagoScore3(BOARD,FINAL2,FINAL3, Pivot),
checkDiagoScore4(BOARD,FINAL3,FINAL4, Pivot),
checkDiagoScore5(BOARD,FINAL4,FINAL5, Pivot),
checkDiagoScore6(BOARD,FINAL5,FINAL6, Pivot),
checkDiagoScore7(BOARD,FINAL6,FINAL7, Pivot),
checkDiagoScore8(BOARD,FINAL7,FINAL8, Pivot),
checkDiagoScore9(BOARD,FINAL8,FINAL9, Pivot),
checkDiagoScore10(BOARD,FINAL9,FINAL10, Pivot),
checkDiagoScore11(BOARD,FINAL10,FINAL11, Pivot),
checkDiagoScore12(BOARD,FINAL11,FINAL12, Pivot),
checkHorizontalScore(BOARD,FINAL12,FINAL13, Pivot),
checkVerticalScore(BOARD,FINAL13,SCORE, Pivot).

checkDiagoScore1([_,_,_,[X41|X4T],[X51,X52|X5T],[X61,X62,X63|X6T],[X71,X72,X73,X74|X7T]], Fin,NewFin,Pivot) :-
	scoreLigne([X41,X52,X63,X74], Pivot, L, R, Fin,NewFin).

checkDiagonalScore2([_,_,[X31|X3T],[X41,X42|X4T],[X51,X52,X53|X5T],[X61,X62,X63,X64|X6T],[X71,X72,X73,X74,X75|X7T]], Fin,NewFin,Pivot) :-
	scoreLigne([X31,X42,X53,X64,X75], Pivot, L, R, Fin,NewFin).

checkDiagonalScore3([_,[X21|X2T],[X31,X32|X3T],[X41,X42,X43|X4T],[X51,X52,X53,X54|X5T],[X61,X62,X63,X64,X65|X6T],[X71,X72,X73,X74,X75,X76|X7T]], Fin,NewFin,Pivot) :-
	scoreLigne([X21,X32,X43,X54,X65,X76], Pivot, L, R, Fin,NewFin).

checkDiagonalScore4([[X11|X1T],[X21,X22|X2T],[X31,X32,X33|X3T],[X41,X42,X43,X44|X4T],[X51,X52,X53,X54,X55|X5T],[X61,X62,X63,X64,X65,X66|X6T],_], Fin,NewFin,Pivot) :-
	scoreLigne([X11,X22,X33,X44,X55,X66], Pivot, L, R, Fin,NewFin).

checkDiagonalScore5([[X11,X12|X1T],[X21,X22,X23|X2T],[X31,X32,X33,X34|X3T],[X41,X42,X43,X44,X45|X4T],[X51,X52,X53,X54,X55,X56|X5T],_,_], Fin,NewFin,Pivot) :-
	scoreLigne([X12,X23,X34,X45,X56], Pivot, L, R, Fin,NewFin).

checkDiagonalScore6([[X11,X12,X13|X1T],[X21,X22,X23,X24|X2T],[X31,X32,X33,X34,X35|X3T],[X41,X42,X43,X44,X45,X46|X4T],_,_,_], Fin,NewFin,Pivot) :-
	scoreLigne([X13,X24,X35,X46], Pivot, L, R, Fin,NewFin).

checkDiagonalScore7([[X11,X12,X13,X14|X1T],[X21,X22,X23|X2T],[X31,X32|X3T],[X41|X4T],_,_,_], Fin,NewFin,Pivot) :-
	scoreLigne([X14,X23,X32,X41], Pivot, L, R, Fin,NewFin).

checkDiagonalScore8([[X11,X12,X13,X14,X15|X1T],[X21,X22,X23,X24|X2T],[X31,X32,X33|X3T],[X41,X42|X4T],[X51|X5T],_,_], Fin,NewFin,Pivot) :-
	scoreLigne([X15,X24,X33,X42,X51], Pivot, L, R, Fin,NewFin).

checkDiagonalScore9([[X11,X12,X13,X14,X15,X16|X1T],[X21,X22,X23,X24,X25|X2T],[X31,X32,X33,X34|X3T],[X41,X42,X43|X4T],[X51,X52|X5T],[X61|X6T],_], Fin,NewFin,Pivot) :-
	scoreLigne([X16,X25,X34,X43,X52,X61], Pivot, L, R, Fin,NewFin).

checkDiagonalScore10([_,[X21,X22,X23,X24,X25,X26|X2T],[X31,X32,X33,X34,X35|X3T],[X41,X42,X43,X44|X4T],[X51,X52,X53|X5T],[X61,X62|X6T],[X71|X7T]], Fin,NewFin,Pivot) :-
	scoreLigne([X26,X35,X44,X53,X62,X71], Pivot, L, R, Fin,NewFin).

checkDiagonalScore11([_,_,[X31,X32,X33,X34,X35,X36|X3T],[X41,X42,X43,X44,X45|X4T],[X51,X52,X53,X54|X5T],[X61,X62,X63|X6T],[X71,X72|X7T]], Fin,NewFin,Pivot) :-
	scoreLigne([X36,X45,X54,X63,X72], Pivot, L, R, Fin,NewFin).

checkDiagonalScore12([_,_,_,[X41,X42,X43,X44,X45,X46|X4T],[X51,X52,X53,X54,X55|X5T],[X61,X62,X63,X64|X6T],[X71,X72,X73|X7T]], Fin,NewFin,Pivot) :-
	scoreLigne([X46,X55,X64,X73], Pivot, L, R, Fin,NewFin).

checkHorizontalScore([],[],[],[],[],[],[]], Fin,NewFin,Pivot) :-!.
checkHorizontalScore([[X1|X1T],[X2|X2T],[X3|X3T],[X4|X4T],[X5|X5T],[X6|X6T],[X7|X7T]], Fin,NewFin,Pivot) :-
scoreLigne([X1,X2,X3,X4,X5,X6,X7], Pivot, L, R, Fin,NewFin),checkHorizontalScore([X1T,X2T,X3T,X4T,X5T,X6T,X7T]).

checkVerticalScore([], Fin,NewFin,Pivot) :-!.
checkVerticalScore([X|XT], Fin,NewFin,Pivot) :-
scoreLigne(X, Fin,NewFin,Pivot),checkVertical(XT).


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
