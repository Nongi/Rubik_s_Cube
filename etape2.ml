Printf.printf "Chargement de l'étape 2\n";;
(*
	Etape 2: cube 2*2*3
		a) c.1.0 : 1/2/5
		b) s.1.1 : 1/2
		c) s.2.0 : 1/5
*)


let rec step2a (c:cube)=
	c#searchCorner 1 2 5;
	if(c#get_coordTemp.(0) == 0 && c#get_coordTemp.(1) == 0) then
	(
		if(c#get_coordTemp.(2) == 6) then
		(
			c#mvR;
		)
		else
		(
			c#mvU;
			c#mvF';
			c#mvU;
			c#mvU;
			step2a c;
		)
	)
	else
	(
		if(c#get_coordTemp.(0) == 0) then
		(
			c#mvU;
			step2a c;
		)
		else
		(
			c#mvR;
			c#mvF';
			c#mvR;
			step2a c;
		)
	)
;;


let rec step2b (c:cube)=
	c#searchSide 1 2;
	if(c#get_coordTemp.(0) == 1 && c#get_coordTemp.(1) == 0 ||c#get_coordTemp.(0) == 1 && c#get_coordTemp.(1) == 1)then
	(
		if(c#get_coordTemp.(2) == 2) then
		(		
			c#mvD;	
			c#mvR;	
			c#mvR;	
			c#mvD';	
			if(c#get_coordTemp.(0) == 1 && c#get_coordTemp.(1) == 1)then
			(
				step2b c;
			)
		)
		else
		(		
			c#mvF';	
			c#mvU';		
			c#mvD;
			c#mvR';	
			c#mvD';
			step2b c;
		)
	)
	else(
		if(c#get_coordTemp.(0) == 0 && c#get_coordTemp.(1) == 3) then
		(		
			c#mvF;
			step2b c;
		)
		else
		(		
			if(c#get_coordTemp.(0) == 0) then
			(
				c#mvU;
				step2b c;
			)
			else
			(
				if(c#get_coordTemp.(1) == 0) then
				(
					c#mvB';
					c#mvR';
					c#mvB;
					step2b c;
				)
				else
				(
					c#mvF;
					step2b c;
				)
			)
		)		
	)
;;


let rec step2c (c:cube)=
	c#searchSide 1 5;
	if(c#get_coordTemp.(0) == 2 && c#get_coordTemp.(1) == 0)then
	(
		if(c#get_coordTemp.(2) == 2 )then
		(
			c#mvB';
			c#mvR';
			c#mvB;
			c#mvU;
			step2c c;
		)
	)
	else(
		if(c#get_coordTemp.(0) == 0 && c#get_coordTemp.(1) == 0)then
		(
			if(c#get_coordTemp.(2) == 1 )then
			(
				c#mvB';
				c#mvR;
				c#mvR;
				c#mvB;
				step2c c;
			)
			else(
				c#mvU';
				c#mvB';
				c#mvR;
				c#mvB;
				step2c c;
			)
		)
		else(
			if(c#get_coordTemp.(0) == 0 )then
			(
				c#mvU';
				step2c c;
			)
			else(
				c#mvF;
				step2c c;
			)
		)		
	)
;;

let step2 (c:cube)=
	c#set_step 2;
	step2a c;
	step2b c;
	step2c c;
;;
