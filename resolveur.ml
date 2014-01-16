Printf.printf "Chargement du Resolveur\n";;

module L=List;;
module A=Array;;

Printf.printf "Debut \n";;

#use "cubeObject.ml";;
#use "etape1.ml";;
#use "etape2.ml";;
#use "etape3.ml";;
#use "etape4.ml";;
#use "etape5.ml";;
#use "etape6.ml";;
#use "etape7.ml";;

let c = new cube;;

c#init;;
c#print;;

Printf.printf "\n		___________________________________________________\n";;
Printf.printf "\n			Liste des commandes :";;
Printf.printf "\n		___________________________________________________\n\n";;
Printf.printf " resolv (cube) :\n    r�sout le cube pass� en param�tre et l'affiche\n\n";;
Printf.printf " shake (cube) (nombre de mouvement):\n    m�lange avec le nombre de mouvement al�atoire en param�tre\n\n";;
Printf.printf " init (cube) :\n    remet � z�ro le cube pass� en param�tre\n\n";;
Printf.printf " next (cube):\n    applique les �tapes de r�solution du cube, les mouvements qui ont �t� n�cessaire et l'�tat du cube. Next bloque � l'�tape 7(cube r�solu)\n\n";;
Printf.printf " all (cube) (nombre de mouvement):\n    m�lange avec le nombre de mouvement al�atoire en param�tre puis utilise resolv\n\n";;
Printf.printf " how (cube) (num�ro de l'�tape):\n    donne les mouvements effectu�s � l'�tape pass� en param�tre\n\n";;
Printf.printf " howMany (cube) (num�ro de l'�tape):\n    donne le nombre de mouvements effectu�s � l'�tape pass� en param�tre avec 0=l'ensemble des �tapes\n\n";;
Printf.printf "\n		___________________________________________________\n";;

let shake (c:cube) (nb:int)=
	c#mv nb;
;;

let init (c:cube)=
	c#init;
;;

let resolv (c:cube)=
	step1 c;
	step2 c;

	c#set_counterStep3 0;
	step3old c;

	step4 c;
	step5 c;
	step6 c;
	step7 c;
	c#print;
;;

let all (c:cube) (nb:int)=
	c#mv nb;
	resolv c;
;;

let next (c:cube)=
	match (c#get_step)
	with 0 -> (step1 c; Printf.printf "Etape %d :" c#get_step; c#print; L.iter (Printf.printf "%c ") c#get_listMv1; )
	| 1 -> (step2 c;  Printf.printf "Etape %d :" c#get_step;c#print; L.iter (Printf.printf "%c ") c#get_listMv2;)
	| 2 -> (step3old c;  Printf.printf "Etape %d :" c#get_step;c#print; L.iter (Printf.printf "%c ") c#get_listMv3;)
	| 3 -> (step4 c;  Printf.printf "Etape %d :" c#get_step;c#print; L.iter (Printf.printf "%c ") c#get_listMv4;)
	| 4 -> (step5 c;  Printf.printf "Etape %d :" c#get_step;c#print; L.iter (Printf.printf "%c ") c#get_listMv5;)
	| 5 -> (step6 c;  Printf.printf "Etape %d :" c#get_step;c#print; L.iter (Printf.printf "%c ") c#get_listMv6;)
	| 6 -> (step7 c;  Printf.printf "Etape %d :" c#get_step;c#print; L.iter (Printf.printf "%c ") c#get_listMv7;)

	| _ -> Printf.printf "Error\n";
;;

let how (c:cube) (step:int)=
	match (step)
	with 1 -> (L.iter (Printf.printf "%c ") c#get_listMv1; )
	| 2 -> (L.iter (Printf.printf "%c ") c#get_listMv2;)
	| 3 -> (L.iter (Printf.printf "%c ") c#get_listMv3;)
	| 4 -> (L.iter (Printf.printf "%c ") c#get_listMv4;)
	| 5 -> (L.iter (Printf.printf "%c ") c#get_listMv5;)
	| 6 -> (L.iter (Printf.printf "%c ") c#get_listMv6;)
	| 7 -> (L.iter (Printf.printf "%c ") c#get_listMv7;)

	| _ -> Printf.printf "Error\n";
;;


let howMany (c:cube) (step:int)=
	match (step)
	with 0 -> (Printf.printf "%d mouvements au total" ((L.length c#get_listMv1) + (L.length c#get_listMv2) + (L.length c#get_listMv3) + (L.length c#get_listMv4) + (L.length c#get_listMv5) + (L.length c#get_listMv6) + (L.length c#get_listMv7)  ); )
	| 1 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv1;);)
	| 2 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv2;);)
	| 3 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv3;);)
	| 4 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv4;);)
	| 5 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv5;);)
	| 6 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv6;);)
	| 7 -> (Printf.printf "%d mouvements � l'�tape 1" (L.length c#get_listMv7;);)

	| _ -> Printf.printf "Error\n";
;;