Printf.printf "Chargement de l'étape 6\n";;
(*
	Etape 6: Placement des coins dans leurs bonnes position avec l'algo Sune
*)

let sune (c:cube) (numMov:int)=
	match (numMov)
	with 0 -> ()
	| 1 -> (c#mvB;	c#mvU;	c#mvB';	c#mvU;	c#mvB;	c#mvU;	c#mvU;	c#mvB';	c#mvU;	c#mvU;)
	| 2 -> (c#mvR;	c#mvU;	c#mvR';	c#mvU;	c#mvR;	c#mvU;	c#mvU;	c#mvR';	c#mvU;	c#mvU;)
	| 3 -> (c#mvF;	c#mvU;	c#mvF';	c#mvU;	c#mvF;	c#mvU;	c#mvU;	c#mvF';	c#mvU;	c#mvU;)
	| 4 -> (c#mvL;	c#mvU;	c#mvL';	c#mvU;	c#mvL;	c#mvU;	c#mvU;	c#mvL';	c#mvU;	c#mvU;)

	| _ -> Printf.printf "Error\n";
;;

let rec step6 (c:cube)=
	c#set_step 6;
	if(	 c#get_Corners.(0).(0)#get_surfc==6 &&
		 c#get_Corners.(0).(1)#get_surfc==6 &&
		 c#get_Corners.(0).(2)#get_surfc==6 &&
		 c#get_Corners.(0).(3)#get_surfc==6)
	then()else(
		if(c#get_Corners.(0).(0)#get_surfc==6 &&
		 c#get_Corners.(0).(1)#get_surfc!=6 &&
		 c#get_Corners.(0).(2)#get_surfc!=6 &&
		 c#get_Corners.(0).(3)#get_surfc!=6)then(
			sune c 4;
			step6 c;
		)else(
			if(c#get_Corners.(0).(0)#get_surfc!=6 &&
			 c#get_Corners.(0).(1)#get_surfc==6 &&
			 c#get_Corners.(0).(2)#get_surfc!=6 &&
			 c#get_Corners.(0).(3)#get_surfc!=6)then(
				sune c 3;
				step6 c;
			)else(
				if(c#get_Corners.(0).(0)#get_surfc!=6 &&
				 c#get_Corners.(0).(1)#get_surfc!=6 &&
				 c#get_Corners.(0).(2)#get_surfc==6 &&
				 c#get_Corners.(0).(3)#get_surfc!=6)then(
					sune c 2;
					step6 c;
				)else(
					if(c#get_Corners.(0).(0)#get_surfc!=6 &&
					 c#get_Corners.(0).(1)#get_surfc!=6 &&
					 c#get_Corners.(0).(2)#get_surfc!=6 &&
					 c#get_Corners.(0).(3)#get_surfc==6)then(
						sune c 1;
						step6 c;
					)else(
						if(c#get_Corners.(0).(0)#get_surfc!=6)then(
							sune c 4;
							step6 c;
						)else(
							if(c#get_Corners.(0).(1)#get_surfc!=6)then(
								sune c 3;
								step6 c;
							)else(
								if(c#get_Corners.(0).(2)#get_surfc!=6)then(
									sune c 2;
									step6 c;
								)else(
										sune c 1;
										step6 c;
								)								
							)
						)
					)
				)
			)
		)
	)
;;