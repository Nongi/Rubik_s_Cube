Printf.printf "Chargement de l'étape 3\n";;
(*
	Etape 3: On prépare chaque arête pour qu'elles soient bien positionné pour les étapes suivante
*)

let preMov3 (c:cube) (numMov:int)=
	match (numMov)
	(* mouvement pour 2 arêtes mal orientées*)
	with 0 -> (c#mvR;c#mvU;c#mvR';)
	| 1 -> (c#mvR;c#mvU';c#mvR';)
	| 2 -> (c#mvR;c#mvU;c#mvU;c#mvR';)
	| 3 -> (c#mvR';c#mvF;c#mvR;)
	| 4 -> (c#mvF;c#mvR;c#mvU;c#mvR';)

	(* mouvement pour 4 arêtes mal orientées simple*)
	| 5 -> (c#mvR;c#mvU;c#mvR;c#mvR;c#mvF';c#mvR;)
	| 6 -> (c#mvR';c#mvF;c#mvR';c#mvR';c#mvU;c#mvR';)
	| 7 -> (c#mvR;c#mvU;c#mvR';c#mvR';c#mvF;c#mvF;c#mvR;)
	(* mouvement pour 4 arêtes mal orientées semi complexe 1*)
	| 8 -> (c#mvR';c#mvF';c#mvR;c#mvL';c#mvU';c#mvL;)
	| 9 -> (c#mvR;c#mvU';c#mvU';c#mvR';c#mvR';c#mvF;c#mvR;)
	| 10 -> (c#mvR';c#mvF;c#mvR;c#mvL';c#mvU;c#mvL;)
	(* mouvement pour 4 arêtes mal orientées semi complexe 2*)
	| 11 -> (c#mvR';c#mvF;c#mvR;c#mvR;c#mvU;c#mvU;c#mvR';)
	| 12 -> (c#mvL';c#mvR;c#mvU;c#mvL;c#mvR')
	| 13 -> (c#mvL;c#mvR';c#mvF';c#mvR;c#mvL';)

	(* mouvement pour 6 arêtes mal orientées*)
	| 14 -> (c#mvL;c#mvR';c#mvF';c#mvR;c#mvL;c#mvL;c#mvU';c#mvL;)

	| _ -> Printf.printf "Error\n";
;;


let rec step3tri (c:cube) =
	c#searchSide 4 6;

	if(c#lastCross_done)then
	(
	)
	else
	(
		if(c#get_Sides.(0).(3)#get_surfb==6)then
		(
			c#mvU;
			step3tri c;
		)else
		(
			c#mvF;
			step3tri c;
		)
	)
;;

(*
let rec step3movA (c:cube)=
	c#mvL';
	c#mvU;
	c#mvL;
;;

let rec step3movB (c:cube)=
	c#mvR;
	c#mvU';
	c#mvR';
;;

let rec step3 (c:cube)=
	c#set_step 3;
	c#calc_BadSide;
	c#calc_BadSidefS;
	if(c#get_nbBadSide!=0)then
	(
		if(c#get_nbBadSidefS >0)then
		(
			if(c#get_Sides.(1).(3)#get_surfa==4 || c#get_Sides.(1).(3)#get_surfb==6)then
			(
				
			)
			else
			(
				c#mvF;
			)
		)else
		(
			c#mvU;
			step3 c;
		)
	)
	else
	(	
		(*tri quand il n'y a pu d'erreur afin d'obtenir la croix bleu et jaune*)		
		step3tri c;
	)
;;*)

let rec step3old (c:cube)=
	c#set_step 3;

	c#calc_BadSide;
	c#set_counterStep3 (c#get_counterStep3 + 1);
	if(c#get_nbBadSide==0)then
	(
		(*tri quand il n'y a pu d'erreur afin d'obtenir la croix bleu et jaune*)		
		step3tri c;
	)else
	(
		if(c#get_nbBadSide==2)then
		(
			preMov3 c (Random.int 5);
			step3old c;
		)else
		(
			if(c#get_nbBadSide>2||c#get_nbBadSide<6)then
			(
				preMov3 c ((Random.int 9)+5);
				step3old c;
			)else
			(
				preMov3 c 14;
				step3old c;
			)
		)
	)
;;
