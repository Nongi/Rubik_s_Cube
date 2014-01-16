Printf.printf "Chargement de l'étape 7\n";;
(*
	Etape 7: Placement des arêtes dans leurs bonnes positions avec l'algo Allan
*)

let allan (c:cube) (numMov:int)=
	match (numMov)
	with 0 -> (c#mvF;c#mvF;c#mvU';c#mvL;c#mvR';c#mvF;c#mvF;c#mvL';c#mvR;c#mvU';c#mvF;c#mvF;)
	| 1 -> (c#mvL;c#mvL;c#mvU';c#mvB;c#mvF';c#mvL;c#mvL;c#mvB';c#mvF;c#mvU';c#mvL;c#mvL;)
	| 2 -> (c#mvB;c#mvB;c#mvU';c#mvR;c#mvL';c#mvB;c#mvB;c#mvR';c#mvL;c#mvU';c#mvB;c#mvB;)
	| 3 -> (c#mvR;c#mvR;c#mvU';c#mvF;c#mvB';c#mvR;c#mvR;c#mvF';c#mvB;c#mvU';c#mvR;c#mvR;)

	| _ -> Printf.printf "Error\n";
;;

let rec step7 (c:cube)=
	c#set_step 7;
	if(	 c#get_Sides.(0).(0)#get_surfa==1 &&
		 c#get_Sides.(0).(1)#get_surfa==2 &&
		 c#get_Sides.(0).(2)#get_surfa==3 &&
		 c#get_Sides.(0).(3)#get_surfa==4)
	then()else(
		if( c#get_Sides.(0).(0)#get_surfa==1 &&
		 c#get_Sides.(0).(1)#get_surfa!=2 &&
		 c#get_Sides.(0).(2)#get_surfa!=3 &&
		 c#get_Sides.(0).(3)#get_surfa!=4)then(
			allan c 1;
			step7 c;
		)else(
			if( c#get_Sides.(0).(0)#get_surfa!=1 &&
			 c#get_Sides.(0).(1)#get_surfa==2 &&
			 c#get_Sides.(0).(2)#get_surfa!=3 &&
			 c#get_Sides.(0).(3)#get_surfa!=4)then(
				allan c 0;
				step7 c;
			)else(
				if( c#get_Sides.(0).(0)#get_surfa!=1 &&
				 c#get_Sides.(0).(1)#get_surfa!=2 &&
				 c#get_Sides.(0).(2)#get_surfa==3 &&
				 c#get_Sides.(0).(3)#get_surfa!=4)then(
					allan c 3;
					step7 c;
				)else(
					if( c#get_Sides.(0).(0)#get_surfa!=1 &&
					 c#get_Sides.(0).(1)#get_surfa!=2 &&
					 c#get_Sides.(0).(2)#get_surfa!=3 &&
					 c#get_Sides.(0).(3)#get_surfa==4)then(
						allan c 2;
						step7 c;
					)else(
						if(c#get_Sides.(0).(0)#get_surfa!=1)then(
							allan c 1;
							step7 c;
						)else(
							if(c#get_Sides.(0).(1)#get_surfa!=2)then(
								allan c 0;
								step7 c;
							)else(
								if(c#get_Sides.(0).(2)#get_surfa!=3)then(
									allan c 3;
									step7 c;
								)else(
									allan c 2;
									step7 c;
								)								
							)
						)
					)
				)
			)
		)
	)
;;