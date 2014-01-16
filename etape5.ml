Printf.printf "Chargement de l'étape 5\n";;
(*
	Etape 5: Placement des coins à leurs place grâce à l'algo Niklas
	a) c.0.0 : 1/2/6
*)

let niklas (c:cube)=
	c#mvL;
	c#mvU';
	c#mvR';
	c#mvU;
	c#mvL';
	c#mvU';
	c#mvR;
	c#mvU;
	c#mvU;
;;

let rec step5bis (c:cube)=
	c#set_step 5;
	c#calc_statLastCorner;
	match (c#get_statLastCorner)
	with 0 -> (niklas c;step5bis c;)
	| 1 -> (c#mvU;niklas c;c#mvU';step5bis c;)
	| 2 -> (niklas c;c#mvU';niklas c;c#mvU;niklas c;step5bis c;)
	| 3 -> (c#mvU;c#mvU;niklas c;c#mvU';c#mvU';step5bis c;)
	| 4 -> (c#mvU';niklas c;c#mvU;step5bis c;)
	| 5 -> (niklas c;c#mvU;niklas c;c#mvU';niklas c;step5bis c;)
	| 6 -> (c#mvU;niklas c;c#mvU';step5bis c;)

	| 9 -> Printf.printf "Etape 5 done";
	| _ -> Printf.printf "Error\n";
;;

let rec step5a (c:cube)=
	c#searchCorner 1 2 6;
	if(c#get_coordTemp.(1) == 0)then
	(
	)else
	(
		c#mvU;
		step5a c;
	)
;;

let rec step5 (c:cube)=
	c#set_step 5;

	step5a c;

	c#searchCorner 1 2 6;
	if(c#get_coordTemp.(1)==0)then
	(
		c#searchCorner 2 3 6;
		if(c#get_coordTemp.(1)==1)then
		(
			c#searchCorner 3 4 6;
			if(c#get_coordTemp.(1)==2)then
			(
			(* Tout les coins sont à leurs places *)
			)else
			(
			(* Les coins supérieur gauche sont inversés *)
				c#mvU';
				niklas c;
				c#mvU;
				step5 c;
			)
		)else
		(
			c#searchCorner 3 4 6;
			if(c#get_coordTemp.(1)==2)then
			(
				(* Les coins supérieur du fond droit et bas gauche sont inversés : diagonal fausse*)
				niklas c;
				c#mvU;
				niklas c;
				c#mvU';
				niklas c;
				step5 c;
			
			)else
			(
				(* Les coins supérieur du fond sont inversés *)
				c#mvU;
				c#mvU;
				niklas c;
				c#mvU;
				c#mvU;
				step5 c;
			)
		)
	)else
	(
		c#searchCorner 2 3 6;
		if(c#get_coordTemp.(1)==1)then
		(
			c#searchCorner 3 4 6;
			if(c#get_coordTemp.(1)==2)then
			(
				(* Les coins supérieur du bas sont inversés *)
				niklas c;
				step5 c;
			)else
			(
				(* Les coins supérieur du bas droit et du fond gauche sont inversés *)
				niklas c;
				c#mvU';
				niklas c;
				c#mvU;
				niklas c;
				step5 c;
			)
		)else
		(
			(* Les coins supérieur gauche sont inversés *)
				c#mvU;
				niklas c;
				c#mvU';
				step5 c;
		)
	)
;;