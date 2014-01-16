Printf.printf "Chargement de l'étape 4\n";;
(*
	Etape 4: Fin des deux étages du bas
		a) c.1.2 : 3/4/5
		b) s.2.3 : 4/5
		c) s.1.3 : 3/4
		d) c.1.3 : 4/1/5
		e) s.1.0 : 4/1
*)

let rec step4Bis (c:cube)=
	c#set_step 4;
	(* On place 4/6 en position 0.3 (étape normalement inutile)*)
	c#searchSide 4 6;
	if(c#get_coordTemp.(0) == 0 && c#get_coordTemp.(1) == 3)then
	(
		(* On Bouge la surface jaune pr compléter sa croix*)
		c#calc_yelCross;
		if(c#get_nbYelCross>0)then
		(
			c#mvR;
			c#mvU;
			c#mvU;
			c#mvR';
			c#mvU';
			c#mvR;
			c#mvU';
			c#mvR';
			step4Bis c;
		)
		else
		(
			(* On Bouge la surface bleu pr compléter sa croix*)
			c#calc_nbBluCross;
			if(c#get_nbBluCross>0)then
			(
				c#mvL;
				c#mvF;
				c#mvF;
				c#mvL';
				c#mvF';
				c#mvL;
				c#mvF';
				c#mvL';
				step4Bis c;
			)
		)
	)else
	(
		c#mvU;
		step4Bis c;
	)
;;

let preMov4 (c:cube) (numMov:int)=
	match (numMov)
	with 0 -> (c#mvF';c#mvU;c#mvF;c#mvU';c#mvF;c#mvU;c#mvU;c#mvF;c#mvU;c#mvU;c#mvF;c#mvF;)
	| 1 -> (c#mvF;c#mvU';c#mvF;c#mvF;)
	| 2 -> (c#mvF;c#mvU;c#mvF;c#mvF;c#mvU';c#mvF';)
	| 3 -> (c#mvF;c#mvF;c#mvU';c#mvF';c#mvU;c#mvF;c#mvF;)
	| 4 -> (c#mvF';c#mvU;c#mvF;)
	| 5 -> (c#mvF;c#mvU';c#mvF;c#mvF;)

	| 6 -> (c#mvF;c#mvU';c#mvR';c#mvL; c#mvF;c#mvF;c#mvR;c#mvL'; c#mvU';c#mvF';)

	| _ -> Printf.printf "Error\n";
;;

let rec step4a (c:cube)=
	(* On place le coin 3/4/5 en 1.2*)
	c#searchCorner 3 4 5;
	if(c#get_coordTemp.(0) == 1 && c#get_coordTemp.(1) == 2)then
	(
		if(c#get_coordTemp.(2) == 1)then
		(

		)else
		(
			preMov4 c 1;
			step4a c;
		)
	)else
	(
		if(c#get_coordTemp.(0) == 1)then
		(
			c#mvF;
			step4a c;
		)else
		(
			if(c#get_coordTemp.(1) == 2 || c#get_coordTemp.(1) == 3)then
			(
				c#mvF;
				step4a c;
			)else
			(
				c#mvU;
				step4a c;
			)
		)
	)
;;

let rec step4b (c:cube)=
	c#searchSide 4 5;
	if(c#get_coordTemp.(0) == 0)then
	(
		if(c#get_coordTemp.(1) == 3 )then
		(
			preMov4 c 2;
			step4b c;

		)else
		(
			c#mvU;
			step4b c;
		)
	)else
	(
		if(c#get_coordTemp.(0) == 2 )then
		(

		)else
		(
			if(c#get_coordTemp.(1) == 0 )then
			(
				preMov4 c 4;
				step4b c;

			)else
			(
				preMov4 c 5;
				step4a c;
				step4b c;
			)
		)
	)
;;

let rec step4c (c:cube)=
	c#searchSide 3 4;
	if(c#get_coordTemp.(0) == 0)then
	(
		if(c#get_coordTemp.(1) == 3 )then
		(
			preMov4 c 3;
			step4c c;

		)else
		(
			c#mvU;
			step4c c;
		)
	)else
	(
		if(c#get_coordTemp.(1) == 3 )then
		(

		)else
		(
			preMov4 c 4;
			step4c c;
		)
	)
;;

let rec step4d (c:cube)=
	c#searchCorner 4 1 5;
	(* On vérifie que le coin est à sa position*)
	if(c#get_coordTemp.(0) == 1)then
	(
		if(c#get_coordTemp.(2) == 1 )then
		(

		)else
		(
			preMov4 c 4;
			c#mvU';
			step4d c;	
		)			
	)else
	(
		(*Sinon on le place*)
		if(c#get_coordTemp.(1) == 2 )then
		(
			c#mvU;
			c#mvU;
			step4d c;
		)else
		(
			preMov4 c 4;
			step4d c;	
		)		
	)
;;


let rec step4e (c:cube)=
	c#searchSide 4 1;
	if(c#get_coordTemp.(0) != 0)then
	(
	)else
	(
		if(c#get_coordTemp.(1) == 2)then
		(
			preMov4 c 6;
			step4e c;
		)else
		(
			c#mvU;
			step4e c;
		)
	)
;;



let step4 (c:cube)=
	c#set_step 4;
	step4a c;
	step4b c;
	step4c c;
	step4d c;
	step4e c;
;;