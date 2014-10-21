test :- new(@w, window('Ma Première Fenêtre')),
		send(@w, open),
		send(@w, size, size(790,690)).
		
% I numero de ligne (0-5) et J numero de colonne (0-6) %
addRouge(I,J) :- send(@w, display, new(Ci, circle(80)), point(J*100+50,I*100+50)),
				 send(Ci, fill_pattern, colour(red)).
				 
% I numero de ligne (0-5) et J numero de colonne (0-6) %
addJaune(I,J) :- send(@w, display, new(Ci, circle(80)), point(J*100+50,I*100+50)),
				 send(Ci, fill_pattern, colour(yellow)).
				 

%circle(diametre),point(point_haut_gauche)%
