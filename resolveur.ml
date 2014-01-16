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
Printf.printf " resolv (cube) :\n    résout le cube passé en paramètre et l'affiche\n\n";;
Printf.printf " shake (cube) (nombre de mouvement):\n    mélange avec le nombre de mouvement aléatoire en paramètre\n\n";;
Printf.printf " init (cube) :\n    remet à zéro le cube passé en paramètre\n\n";;
Printf.printf " next (cube):\n    applique les étapes de résolution du cube, les mouvements qui ont été nécessaire et l'état du cube. Next bloque à l'étape 7(cube résolu)\n\n";;
Printf.printf " all (cube) (nombre de mouvement):\n    mélange avec le nombre de mouvement aléatoire en paramètre puis utilise resolv\n\n";;
Printf.printf " how (cube) (numéro de l'étape):\n    donne les mouvements effectués à l'étape passé en paramètre\n\n";;
Printf.printf " howMany (cube) (numéro de l'étape):\n    donne le nombre de mouvements effectués à l'étape passé en paramètre avec 0=l'ensemble des étapes\n\n";;
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
	| 1 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv1;);)
	| 2 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv2;);)
	| 3 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv3;);)
	| 4 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv4;);)
	| 5 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv5;);)
	| 6 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv6;);)
	| 7 -> (Printf.printf "%d mouvements à l'étape 1" (L.length c#get_listMv7;);)

	| _ -> Printf.printf "Error\n";
;;