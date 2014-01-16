Printf.printf "Chargement de cubeObject\n";;



class center (surf_init:int) =
object (self)
	val mutable surf = surf_init

	method get_surf = surf
	method set_surf d = surf <- d
end;;



class side ((surfa_init:int),(surfb_init:int)) =
object (self)
	val mutable surfa = surfa_init
	val mutable surfb = surfb_init

	method get_surfa = surfa
	method get_surfb = surfb

	method set_surfa d = surfa <- d
	method set_surfb d = surfb <- d

	method verifSide (a:int) (b:int) = if(a == surfa && b ==surfb)then 1 else if(b==surfa && a==surfb)then 2 else 0
	
end;;

class corner ((surfa_init:int), (surfb_init:int), (surfc_init:int)) =
object (self)
	val mutable surfa = surfa_init
	val mutable surfb = surfb_init
	val mutable surfc = surfc_init

	method get_surfa = surfa
	method get_surfb = surfb
	method get_surfc = surfc

	method set_surfa d = surfa <- d
	method set_surfb d = surfb <- d
	method set_surfc d = surfc <- d

	method verifCorner (a:int) (b:int) (c:int) =  if(a == surfa && b == surfb && c == surfc)then 1
						 else if(b == surfa && c == surfb && a == surfc)then 2
						 else if(c == surfa && a == surfb && b == surfc)then 3 

						 else if(c == surfa && b == surfb && a == surfc)then 4
						 else if(b == surfa && a == surfb && c == surfc)then 5
						 else if(a == surfa && c == surfb && b == surfc)then 6 else 0
end;;

class cube =
object (self)

	val mutable step = 0;
	method get_step  = step
	method set_step  d = step <- d

	(*------Ensemble des attributs d'enregistrement-----*)
	val mutable recordMv = true	
	val mutable listMv1 = []
	method get_listMv1 = listMv1	
	val mutable listMv2 = []
	method get_listMv2 = listMv2	
	val mutable listMv3 = []
	method get_listMv3 = listMv3	
	val mutable listMv4 = []
	method get_listMv4 = listMv4	
	val mutable listMv5 = []
	method get_listMv5 = listMv5
	val mutable listMv6 = []
	method get_listMv6 = listMv6
	val mutable listMv7 = []
	method get_listMv7 = listMv7

	method saveMov (mov:char)=
	begin
		match (step)
		with 0 -> listMv1 <- [];
		| 1 -> listMv1 <- mov::listMv1;
		| 2 -> listMv2 <- mov::listMv2;
		| 3 -> listMv3 <- mov::listMv3;
		| 4 -> listMv4 <- mov::listMv4;
		| 5 -> listMv5 <- mov::listMv5;
		| 6 -> listMv6 <- mov::listMv6;
		| 7 -> listMv7 <- mov::listMv7;

		| _ -> Printf.printf "Error\n";
	end

	val mutable arrayCenters = A.make 6 (new center(0))
	val mutable arraySides = A.make_matrix 3 4(new side(0,0))
	val mutable arrayCorners = A.make_matrix 2 4(new corner(0,0,0))

	val mutable centerTemp = new center(0)
	val mutable sideTemp = new side(0,0)
	val mutable cornerTemp = new corner(0,0,0)

	method get_Centers = arrayCenters
	method get_Sides = arraySides
	method get_Corners = arrayCorners

	method init =
	begin
		listMv1 <- [];
		listMv2 <- [];
		listMv3 <- [];
		listMv4 <- [];
		listMv5 <- [];
		listMv6 <- [];
		listMv7 <- [];

		arraySides.(0).(0)<-new side(1,6);
		arraySides.(0).(1)<-new side(2,6);
		arraySides.(0).(2)<-new side(3,6);
		arraySides.(0).(3)<-new side(4,6);

		arraySides.(1).(0)<-new side(4,1);
		arraySides.(1).(1)<-new side(1,2);
		arraySides.(1).(2)<-new side(2,3);
		arraySides.(1).(3)<-new side(3,4);

		arraySides.(2).(0)<-new side(1,5);
		arraySides.(2).(1)<-new side(2,5);
		arraySides.(2).(2)<-new side(3,5);
		arraySides.(2).(3)<-new side(4,5);

		arrayCorners.(0).(0)<-new corner(1,2,6);
		arrayCorners.(0).(1)<-new corner(2,3,6);
		arrayCorners.(0).(2)<-new corner(3,4,6);
		arrayCorners.(0).(3)<-new corner(4,1,6);

		arrayCorners.(1).(0)<-new corner(1,2,5);
		arrayCorners.(1).(1)<-new corner(2,3,5);
		arrayCorners.(1).(2)<-new corner(3,4,5);
		arrayCorners.(1).(3)<-new corner(4,1,5);

		for i = 0 to 5 do
			arrayCenters.(i)<-new center(i+1);
		done

	end

	method print =
	begin
		Printf.printf "Cube: \n";
		Printf.printf "       | %d %d %d |\n" arrayCorners.(0).(2)#get_surfc arraySides.(0).(2)#get_surfb arrayCorners.(0).(1)#get_surfc;
		Printf.printf "       | %d %d %d |\n" arraySides.(0).(3)#get_surfb arrayCenters.(5)#get_surf arraySides.(0).(1)#get_surfb;
		Printf.printf "       | %d %d %d |\n" arrayCorners.(0).(3)#get_surfc arraySides.(0).(0)#get_surfb arrayCorners.(0).(0)#get_surfc;


		Printf.printf " %d %d %d |" arrayCorners.(0).(2)#get_surfb arraySides.(0).(3)#get_surfa arrayCorners.(0).(3)#get_surfa;
		Printf.printf " %d %d %d |" arrayCorners.(0).(3)#get_surfb arraySides.(0).(0)#get_surfa arrayCorners.(0).(0)#get_surfa;
		Printf.printf " %d %d %d |" arrayCorners.(0).(0)#get_surfb arraySides.(0).(1)#get_surfa arrayCorners.(0).(1)#get_surfa;
		Printf.printf " %d %d %d\n" arrayCorners.(0).(1)#get_surfb arraySides.(0).(2)#get_surfa arrayCorners.(0).(2)#get_surfa;

		Printf.printf " %d %d %d |" arraySides.(1).(3)#get_surfb arrayCenters.(3)#get_surf arraySides.(1).(0)#get_surfa;
		Printf.printf " %d %d %d |" arraySides.(1).(0)#get_surfb arrayCenters.(0)#get_surf arraySides.(1).(1)#get_surfa;
		Printf.printf " %d %d %d |" arraySides.(1).(1)#get_surfb arrayCenters.(1)#get_surf arraySides.(1).(2)#get_surfa;
		Printf.printf " %d %d %d\n"  arraySides.(1).(2)#get_surfb arrayCenters.(2)#get_surf arraySides.(1).(3)#get_surfa;

		Printf.printf " %d %d %d |" arrayCorners.(1).(2)#get_surfb arraySides.(2).(3)#get_surfa arrayCorners.(1).(3)#get_surfa;
		Printf.printf " %d %d %d |" arrayCorners.(1).(3)#get_surfb arraySides.(2).(0)#get_surfa arrayCorners.(1).(0)#get_surfa;
		Printf.printf " %d %d %d |" arrayCorners.(1).(0)#get_surfb arraySides.(2).(1)#get_surfa arrayCorners.(1).(1)#get_surfa;
		Printf.printf " %d %d %d\n"  arrayCorners.(1).(1)#get_surfb arraySides.(2).(2)#get_surfa arrayCorners.(1).(2)#get_surfa;


		Printf.printf "       | %d %d %d |\n" arrayCorners.(1).(3)#get_surfc arraySides.(2).(0)#get_surfb arrayCorners.(1).(0)#get_surfc;
		Printf.printf "       | %d %d %d |\n" arraySides.(2).(3)#get_surfb arrayCenters.(4)#get_surf arraySides.(2).(1)#get_surfb;
		Printf.printf "       | %d %d %d |\n" arrayCorners.(1).(2)#get_surfc arraySides.(2).(2)#get_surfb arrayCorners.(1).(1)#get_surfc;

	end

	method mvU =
	begin
		cornerTemp <-arrayCorners.(0).(3);
		arrayCorners.(0).(3)<-arrayCorners.(0).(0);
		arrayCorners.(0).(0)<-arrayCorners.(0).(1);
		arrayCorners.(0).(1)<-arrayCorners.(0).(2);
		arrayCorners.(0).(2)<-cornerTemp;

		sideTemp <-arraySides.(0).(3);
		arraySides.(0).(3)<-arraySides.(0).(0);
		arraySides.(0).(0)<-arraySides.(0).(1);
		arraySides.(0).(1)<-arraySides.(0).(2);
		arraySides.(0).(2)<-sideTemp;

		if (recordMv) then
		(
			self#saveMov 'U';

		)
		else
		();

		Printf.printf "Move U done in step %d\n"step;
	end

	method mvU' =
	begin
		self#mvU;
		self#mvU;
		self#mvU;

		Printf.printf "Move U' done in step %d\n"step;
	end

	method mvE =
	begin

		self#mvU;
		self#mvD';

		Printf.printf "Move E done in step %d\n"step;
	end

	method mvE' =
	begin
		self#mvU';
		self#mvD;

		Printf.printf "Move E' done in step %d\n"step;
	end

	method mvD =
	begin
		cornerTemp <-arrayCorners.(1).(0);
		arrayCorners.(1).(0)<-arrayCorners.(1).(3);
		arrayCorners.(1).(3)<-arrayCorners.(1).(2);
		arrayCorners.(1).(2)<-arrayCorners.(1).(1);
		arrayCorners.(1).(1)<-cornerTemp;

		sideTemp <-arraySides.(2).(1);
		arraySides.(2).(1)<-arraySides.(2).(0);
		arraySides.(2).(0)<-arraySides.(2).(3);
		arraySides.(2).(3)<-arraySides.(2).(2);
		arraySides.(2).(2)<-sideTemp;

		if (recordMv) then
		(
			self#saveMov 'D';
		)
		else
		();

		Printf.printf "Move D done in step %d\n"step;
	end

	method mvD' =
	begin
		self#mvD;
		self#mvD;
		self#mvD;

		Printf.printf "Move D' done in step %d\n"step;
	end

	method mvF=
	begin
		cornerTemp <-arrayCorners.(0).(3);
		arrayCorners.(0).(3)<- new corner(arrayCorners.(0).(2)#get_surfb,arrayCorners.(0).(2)#get_surfc,arrayCorners.(0).(2)#get_surfa);
		arrayCorners.(0).(2)<- new corner(arrayCorners.(1).(2)#get_surfc,arrayCorners.(1).(2)#get_surfb,arrayCorners.(1).(2)#get_surfa);
		arrayCorners.(1).(2)<- new corner(arrayCorners.(1).(3)#get_surfc,arrayCorners.(1).(3)#get_surfa,arrayCorners.(1).(3)#get_surfb);
		arrayCorners.(1).(3)<- new corner(cornerTemp#get_surfa,cornerTemp#get_surfc,cornerTemp#get_surfb);

		sideTemp <-arraySides.(1).(0);
		arraySides.(1).(0)<- arraySides.(0).(3);
		arraySides.(0).(3)<- new side(arraySides.(1).(3)#get_surfb , arraySides.(1).(3)#get_surfa);
		arraySides.(1).(3)<- new side(arraySides.(2).(3)#get_surfb , arraySides.(2).(3)#get_surfa);
		arraySides.(2).(3)<- sideTemp;

		if (recordMv) then
		(
			self#saveMov 'F';
		)
		else
		();

		Printf.printf "Move F done in step %d\n"step;
	end

	method mvF' =
	begin
		self#mvF;
		self#mvF;
		self#mvF;

		Printf.printf "Move F' done in step %d\n"step;
	end

	method mvS =
	begin
		self#mvF';
		self#mvB;

		Printf.printf "Move S done in step %d\n"step;
	end

	method mvS' =
	begin
		self#mvF;
		self#mvB';

		Printf.printf "Move S' done in step %d\n"step;
	end

	method mvB' =
	begin
		cornerTemp <-arrayCorners.(0).(0);
		arrayCorners.(0).(0)<- new corner(arrayCorners.(0).(1)#get_surfc,arrayCorners.(0).(1)#get_surfa,arrayCorners.(0).(1)#get_surfb);
		arrayCorners.(0).(1)<- new corner(arrayCorners.(1).(1)#get_surfa,arrayCorners.(1).(1)#get_surfc,arrayCorners.(1).(1)#get_surfb);
		arrayCorners.(1).(1)<- new corner(arrayCorners.(1).(0)#get_surfb,arrayCorners.(1).(0)#get_surfc,arrayCorners.(1).(0)#get_surfa);
		arrayCorners.(1).(0)<- new corner(cornerTemp#get_surfc,cornerTemp#get_surfb,cornerTemp#get_surfa);

		sideTemp <-arraySides.(1).(1);
		arraySides.(1).(1)<- new side(arraySides.(0).(1)#get_surfb , arraySides.(0).(1)#get_surfa);
		arraySides.(0).(1)<- arraySides.(1).(2);
		arraySides.(1).(2)<- arraySides.(2).(1);
		arraySides.(2).(1)<- new side(sideTemp#get_surfb , sideTemp#get_surfa);

		Printf.printf "Move B done in step %d\n"step;
	end

	method mvB =
	begin
		self#mvB';
		self#mvB';
		self#mvB';

		if (recordMv) then
		(
			self#saveMov 'B';
		)
		else
		();

		Printf.printf "Move B' done in step %d\n"step;
	end



	method mvL =
	begin
		cornerTemp <-arrayCorners.(0).(2);
		arrayCorners.(0).(2)<- new corner(arrayCorners.(0).(1)#get_surfb,arrayCorners.(0).(1)#get_surfc,arrayCorners.(0).(1)#get_surfa);
		arrayCorners.(0).(1)<- new corner(arrayCorners.(1).(1)#get_surfc,arrayCorners.(1).(1)#get_surfb,arrayCorners.(1).(1)#get_surfa);
		arrayCorners.(1).(1)<- new corner(arrayCorners.(1).(2)#get_surfc,arrayCorners.(1).(2)#get_surfa,arrayCorners.(1).(2)#get_surfb);
		arrayCorners.(1).(2)<- new corner(cornerTemp#get_surfa,cornerTemp#get_surfc,cornerTemp#get_surfb);


		sideTemp <-arraySides.(0).(2);
		arraySides.(0).(2)<- new side(arraySides.(1).(2)#get_surfb , arraySides.(1).(2)#get_surfa);
		arraySides.(1).(2)<- new side(arraySides.(2).(2)#get_surfb , arraySides.(2).(2)#get_surfa);
		arraySides.(2).(2)<- arraySides.(1).(3);
		arraySides.(1).(3)<- sideTemp;

		if (recordMv) then
		(
			self#saveMov 'L';
		)
		else
		();

		Printf.printf "Move L done in step %d\n"step;
	end

	method mvL' =
	begin
		self#mvL;
		self#mvL;
		self#mvL;

		Printf.printf "Move L' done in step %d\n"step;
	end

	method mvM =
	begin
		self#mvL';
		self#mvR;

		Printf.printf "Move M done in step %d\n"step;
	end

	method mvM' =
	begin
		self#mvL;
		self#mvR';

		Printf.printf "Move M' done in step %d\n"step;
	end

	method mvR =
	begin
		cornerTemp <-arrayCorners.(0).(0);
		arrayCorners.(0).(0)<- new corner(arrayCorners.(0).(3)#get_surfb,arrayCorners.(0).(3)#get_surfc,arrayCorners.(0).(3)#get_surfa);
		arrayCorners.(0).(3)<- new corner(arrayCorners.(1).(3)#get_surfc,arrayCorners.(1).(3)#get_surfb,arrayCorners.(1).(3)#get_surfa);
		arrayCorners.(1).(3)<- new corner(arrayCorners.(1).(0)#get_surfc,arrayCorners.(1).(0)#get_surfa,arrayCorners.(1).(0)#get_surfb);
		arrayCorners.(1).(0)<- new corner(cornerTemp#get_surfa,cornerTemp#get_surfc,cornerTemp#get_surfb);

		sideTemp <-arraySides.(0).(0);
		arraySides.(0).(0)<- new side(arraySides.(1).(0)#get_surfb , arraySides.(1).(0)#get_surfa);
		arraySides.(1).(0)<- new side(arraySides.(2).(0)#get_surfb , arraySides.(2).(0)#get_surfa);
		arraySides.(2).(0)<- arraySides.(1).(1);
		arraySides.(1).(1)<- sideTemp;

		if (recordMv) then
		(
			self#saveMov 'R';
		)
		else
		();

		Printf.printf "Move R done in step %d\n"step;
	end

	method mvR' =
	begin
		self#mvR;
		self#mvR;
		self#mvR;

		Printf.printf "Move R' done in step %d\n"step;
	end

	method mv (n:int)=
	begin
		for i = 0 to n-1 do
			recordMv <- false;
			self#mvAlea;
			recordMv <- true;
		done
	end

	method mvAlea =
	begin
		match (Random.int 9)
		(* Mouvement horinzontale *)
		with 0 -> self#mvU;
		| 1 -> self#mvE;
		| 2 -> self#mvD;
		(* Mouvement verticale *)
		| 3 -> self#mvF;
		| 4 -> self#mvS;
		| 5 -> self#mvB';
		(* Mouvement en profondeur *)
		| 6 -> self#mvL;
		| 7 -> self#mvM;
		| 8 -> self#mvR;

		| _ -> Printf.printf "Error\n";

	end

	val mutable coordTemp = A.make 3 0;

	method get_coordTemp = coordTemp

	method searchSide (a:int) (b:int) = 		
	begin
		coordTemp <- A.make 3 0;
		for i = 0 to 2 do
			for j = 0 to 3 do
				if (coordTemp.(2) = 0) then
				(
					coordTemp.(2)<-arraySides.(i).(j)#verifSide a b;
					if (coordTemp.(2) > 0 ) then
					(
						Printf.printf "Find (%d-%d) in %d %d\n"a b i j;
						coordTemp.(0)<-i;
						coordTemp.(1)<-j;
					)
				)
		
			done
		done
	end

	method searchCorner (a:int) (b:int) (c:int) =
	begin
		coordTemp <- A.make 3 0;
		for i = 0 to 1 do
			for j = 0 to 3 do
				if (coordTemp.(2) = 0) then
				(
					coordTemp.(2)<-arrayCorners.(i).(j)#verifCorner a b c;
					if (coordTemp.(2) > 0 ) then
					(
						Printf.printf "Find (%d %d %d) en (%d,%d) positionné en %d\n"a b c i j coordTemp.(2);
						coordTemp.(0)<-i;
						coordTemp.(1)<-j;
					)
				)
		
			done
		done
	end

	val mutable nbBadSide = 0;
	method get_nbBadSide = nbBadSide
	
	method calc_BadSide =
	begin
		nbBadSide <- 0;
		if(arraySides.(0).(0)#get_surfa==6 ||arraySides.(0).(0)#get_surfb==4 )then nbBadSide<- (nbBadSide+1);
		if(arraySides.(0).(1)#get_surfa==6 ||arraySides.(0).(1)#get_surfb==4 )then nbBadSide<- (nbBadSide+1);
		if(arraySides.(0).(2)#get_surfa==6 ||arraySides.(0).(2)#get_surfb==4 )then nbBadSide<- (nbBadSide+1);

		if(arraySides.(0).(3)#get_surfa==6 || arraySides.(0).(3)#get_surfb==4)then nbBadSide<- (nbBadSide+1);

		if(arraySides.(1).(0)#get_surfa==6 ||arraySides.(1).(0)#get_surfb==4 )then nbBadSide<- (nbBadSide+1);
		if(arraySides.(1).(3)#get_surfb==6 ||arraySides.(1).(3)#get_surfa==4 )then nbBadSide<- (nbBadSide+1);
		if(arraySides.(2).(3)#get_surfa==6 ||arraySides.(2).(3)#get_surfb==4 )then nbBadSide<- (nbBadSide+1);
	end


	val mutable counterStep3 = 0;
	method get_counterStep3  = counterStep3
	method set_counterStep3  d = counterStep3 <- d


	val mutable nbYelCross = 0;
	method get_nbYelCross = nbYelCross
	method calc_yelCross =
	begin
		nbYelCross <- 0;
		if(arraySides.(0).(0)#get_surfa!=1)then nbYelCross<- (nbYelCross+1);
		if(arraySides.(0).(1)#get_surfa!=2)then nbYelCross<- (nbYelCross+1);
		if(arraySides.(0).(2)#get_surfa!=3)then nbYelCross<- (nbYelCross+1);
	end

	val mutable nbBluCross = 0;
	method get_nbBluCross = nbBluCross
	method calc_nbBluCross =
	begin
		nbBluCross <- 0;
		if(arraySides.(1).(0)#get_surfb!=1)then nbBluCross<- (nbBluCross+1);
		if(arraySides.(1).(3)#get_surfa!=3)then nbBluCross<- (nbBluCross+1);
		if(arraySides.(2).(3)#get_surfb!=5)then nbBluCross<- (nbBluCross+1);
	end

	method lastCross_done = if(arraySides.(0).(0)#get_surfb==6 
			&& arraySides.(0).(1)#get_surfb==6
			&& arraySides.(0).(2)#get_surfb==6
			&& arraySides.(0).(3)#get_surfb==6

			&& arraySides.(0).(3)#get_surfa==4
			&& arraySides.(1).(0)#get_surfa==4
			&& arraySides.(2).(3)#get_surfa==4
			&& arraySides.(1).(3)#get_surfb==4
		)then true else false


	val mutable statLastCorner = 0;
	method get_statLastCorner = statLastCorner
	method calc_statLastCorner =
	begin
		statLastCorner <- 9;
		self#searchCorner 1 2 6;
		if(self#get_coordTemp.(1)!=0)then
		(
			self#searchCorner 4 1 6;
			if(self#get_coordTemp.(1)==3)then
			(
				self#searchCorner 2 3 6;
				if(self#get_coordTemp.(1)==1)then
				(
					statLastCorner <- 2;
				)
				else
				(
					statLastCorner <- 1;
				)		
			)
			else
			(
				statLastCorner <- 0;
			)		
		)
		else
		(
			self#searchCorner 3 4 6;
			if(self#get_coordTemp.(1)!=2)then
			(
				self#searchCorner 2 3 6;
				if(self#get_coordTemp.(1)!=1)then
				(
					statLastCorner <- 3;
				)
				else
				(
					statLastCorner <- 4;
				)		
			)
			else
			(
				self#searchCorner 2 3 6;
				if(self#get_coordTemp.(1)!=1)then
				(
					statLastCorner <- 9;
				)
				else
				(
					statLastCorner <- 5;
				)	
			)
		
		)
	end
 
end;;


