Printf.printf "Chargement de l'étape 1\n";;
(*
	Etape 1: cube 2*2*2
		a) c.1.1 : 2/3/5
		b) s.2.2 : 3/5
		c) s.1.2 : 2/3
		d) s.2.1 : 2/5
*)


let rec step1a (c:cube)=
	c#searchCorner 2 3 5;
	if(c#get_coordTemp.(0) == 1 && c#get_coordTemp.(1) == 1 && c#get_coordTemp.(2) == 1) then
	(
	)
	else
	(
		if(c#get_coordTemp.(0) == 0) then
		(
			c#mvL;
			c#mvL;
			c#mvR;
			c#mvR;
			step1a c;
		)
		else
		(
			if(c#get_coordTemp.(1) != 1) then
			(		
				c#mvD;
				step1a c;
			)
			else
			(
				if(c#get_coordTemp.(2) != 1) then
				(
					c#mvB;	
					c#mvD;
					step1a c;
				)
			)
		)
	)
;;



let rec step1b (c:cube)=
	c#searchSide 3 5;
	if(c#get_coordTemp.(0) == 2 && c#get_coordTemp.(1) == 2 && c#get_coordTemp.(2) == 1)then
	(
	)
	else(
		if(c#get_coordTemp.(0) == 0 && c#get_coordTemp.(1) == 2 && c#get_coordTemp.(2) == 1) then
		(
			c#mvB;
			c#mvL;
			c#mvL;
			c#mvB';
			step1b c;
		)
		else
		(
			if( (c#get_coordTemp.(0) == 0) && (c#get_coordTemp.(1) == 2) && (c#get_coordTemp.(2) == 2)) then
			(
				c#mvU';
				c#mvF;
				c#mvR;
				c#mvU;
				c#mvU;
				step1b c;
			)
			else
			(
				if(c#get_coordTemp.(0)==0 && c#get_coordTemp.(1)!= 2) then
				(
					c#mvU;
					step1b c;
				)
				else
				(
					if(c#get_coordTemp.(0)==1)then
					(
						match (c#get_coordTemp.(1))
						with 0 -> (c#mvR; step1b c;)
						| 1 -> (c#mvR'; step1b c;)
						| 2 -> (c#mvD'; c#mvL; c#mvD; step1b c;)
						| 3 -> (c#mvF; step1b c;)
						| _ -> Printf.printf "Error\n";
					)
					else
					(
						match (c#get_coordTemp.(1))
						with 0 -> (c#mvR; step1b c;)
						| 1 -> (c#mvL'; c#mvB; c#mvL; step1b c;)
						| 2 -> (c#mvB; c#mvL'; c#mvB'; step1b c;)
						| 3 -> (c#mvF; step1b c;)
						| _ -> Printf.printf "Error\n";
					)
				)
			)
		)
	)
;;

let rec step1c (c:cube)=
	c#searchSide 2 3;
	if(c#get_coordTemp.(0) == 1 && c#get_coordTemp.(1) == 2 && c#get_coordTemp.(2) == 1)then
	(
	)
	else(
		if(c#get_coordTemp.(0) == 0 && c#get_coordTemp.(1) == 2 && c#get_coordTemp.(2) == 2) then
		(
			c#mvD';
			c#mvL';
			c#mvD;
			step1c c;
		)
		else
		(
			if( (c#get_coordTemp.(0) == 0) && (c#get_coordTemp.(1) == 2) && (c#get_coordTemp.(2) == 1)) then
			(
				c#mvU';
				c#mvF;
				c#mvR;
				c#mvU;
				c#mvU;
				step1c c;
			)
			else
			(
				if(c#get_coordTemp.(0)==0 && c#get_coordTemp.(1)!= 2) then
				(
					c#mvU;
					step1c c;
				)
				else
				(
					if(c#get_coordTemp.(0)==1)then
					(
						match (c#get_coordTemp.(1))
						with 0 -> (c#mvR; step1c c;)
						| 1 -> (c#mvR'; step1c c;)
						| 2 -> (c#mvD'; c#mvL; c#mvD; step1c c;)
						| 3 -> (c#mvF; step1c c;)
						| _ -> Printf.printf "Error\n";
					)
					else
					(
						match (c#get_coordTemp.(1))
						with 0 -> (c#mvR; step1c c;)
						| 1 -> (c#mvL'; c#mvB; c#mvL; step1c c;)
						| 2 -> (c#mvB; c#mvL'; c#mvB'; step1c c;)
						| 3 -> (c#mvF; step1c c;)
						| _ -> Printf.printf "Error\n";
					)
				)
			)
		)
	)
;;


let rec step1d (c:cube)=
	c#searchSide 2 5;
	if(c#get_coordTemp.(0) == 2 && c#get_coordTemp.(1) == 1 && c#get_coordTemp.(2) == 1)then
	(
	)
	else
	(
		if(c#get_coordTemp.(0) == 2 && c#get_coordTemp.(1) == 0) then
		(
			if(c#get_coordTemp.(2) == 1) then
			(
				c#mvL;
				c#mvD;
				c#mvL';
				step1d c;
			)
			else
			(
				c#mvR;
				c#mvF';
				c#mvU';
				c#mvR;
				c#mvR';
				step1d c;
			)
		)
		else
		(
			if(c#get_coordTemp.(0)==2 && c#get_coordTemp.(1)==2 && c#get_coordTemp.(2)==1) then
			(
				c#mvR;
				c#mvF';
				c#mvU';
				c#mvR;
				c#mvR;
				step1d c;
			)
			else
			(
				if(c#get_coordTemp.(0)==2 && c#get_coordTemp.(1)!= 0) then
				(
					c#mvL;
					c#mvD;
					c#mvL';
					step1d c;
				)
				else
				(
					if(c#get_coordTemp.(0)==1)then
					(
						(* side trouver sur l'anneau du milieu*)
						match (c#get_coordTemp.(1))
						with 0 -> (c#mvR'; step1d c;)
						| 1 -> (c#mvR; step1d c;)
						| 2 -> (step1a c;step1b c;step1c c; step1d c;)
						| 3 -> (c#mvF; step1d c;)
						| _ -> Printf.printf "Error\n";
					)
					else
					(
						(* side trouver sur l'anneau supérieur*)
						if(c#get_coordTemp.(1)==0)then
						(
							c#mvR;
							c#mvR;
							step1d c;
						)
						else
						(
							c#mvU;
							step1d c;
						)
					)
				)
			)
		)
	)
;;

let step1 (c:cube)=
	c#set_step 1;
	step1a c;
	step1b c;
	step1c c;
	step1d c;
;;
