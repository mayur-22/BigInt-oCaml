open String;;

type sign = Neg | NonNeg

type bigint = sign * int list

exception CannotDivide;;

(*helper function to convert string to bigint*)
let rec mk_big_helper s l = try
		let lis = (l@[(Char.code (get s 0))-48]) in
		mk_big_helper (String.sub s 1 ((String.length  s)-1) ) lis
	with
		| _ -> l

(*converts the string into bigint*)
let mk_big s =	let lis =  mk_big_helper s [] in
								match lis with
								| [] -> (NonNeg,[0])
								| x::xs -> if x<0 then (Neg,xs)
													else (NonNeg,lis);;

(*return the absolute value of bigint*)
let abs a = let (x,y) = a in (NonNeg,y);;

(*helper function for mkl_int. Convert int to int list*)
let rec conversion x = match x with
		| 0 -> []
		| _ -> if (x>=0) then (conversion (x/10))@[x mod 10]
			   else (conversion ((-x)/10))@[(-x) mod 10];;


(*make bigint from int*)
let mkl_int x = if x>=0 then (NonNeg,(conversion x))
				else (Neg,(conversion x));;

(*return true if val(a)>val(b)*)
let rec gte a b = if ((List.length a)>(List.length b)) then true
			  	  else if (((List.length a)<(List.length b))) then false
			 	  else (match a with
			  		| [] -> true			(*both have length =0*)
			  		| x::xs-> (match b with
			  				| [] -> true (*Not possible cases*)
			  				| y::ys -> if(x>y) then true
			  						   else if (x<y) then false
			  						   else gte xs ys));;

(*return true if val(a)>=val(b)*)
let geq x y = let (a,b)=x in
			  let (c,d)=y in
			  match a with
				| NonNeg -> if (c==NonNeg) then (gte b d) else true
				| Neg -> if (c==Neg) then (gte d b) else false;;


(* helper function for gt *)
let rec greater_than a b = if ((List.length a)>(List.length b)) then true
			  			   else if (((List.length a)<(List.length b))) then false
			  			   else (match a with
			  					| [] -> false			(*both have length =0*)
			  					| x::xs-> (match b with
			  						| [] -> false (*Not possible cases*)
			  						| y::ys -> if(x>y) then true
			  						   		   else if (x<y) then false
			  						   		   else greater_than xs ys));;

(*return true if val(a)>val(b)*)
let gt x y = let (a,b)=x in
			 let (c,d)=y in
			 match a with
				| NonNeg -> if (c==NonNeg) then (greater_than b d) else true
				| Neg -> if (c==Neg) then (greater_than d b) else false;;


(*Helper function for leq*)
let rec lte a b = if ((List.length a)>(List.length b)) then false
			  else if (((List.length a)<(List.length b))) then true
			  else (match a with
			  	| [] -> true			(*both have length =0*)
			  	| x::xs-> (match b with
			  				| [] -> false (*Not possible cases*)
			  				| y::ys -> if(x>y) then false
			  						   else if (x<y) then true
			  						   else lte xs ys));;

(*return true if val(a)<=val(b)*)
let leq x y = let (a,b)=x in
				let (c,d)=y in
				match a with
				| NonNeg -> if (c==NonNeg) then (lte b d) else false
				| Neg -> if (c==Neg) then (lte d b) else true;;


(*helper function for lt*)
let rec less_than a b = if ((List.length a)>(List.length b)) then false
			  			else if (((List.length a)<(List.length b))) then true
			  			else (match a with
			  				| [] -> false			(*both have length =0*)
			  				| x::xs-> (match b with
			  						| [] -> false (*Not possible cases*)
			  						| y::ys -> if(x>y) then false
			  						   		   else if (x<y) then true
			  						   		   else less_than xs ys));;
(*return true if val(a)<=val(b)*)
let lt x y = let (a,b)=x in
			 let (c,d)=y in
			 match a with
				| NonNeg -> if (c==NonNeg) then (less_than b d) else false
				| Neg -> if (c==Neg) then (less_than d b) else true;;

(*Helper function for eq*)
let rec equal a b = if ((List.length a)>(List.length b)) then false
			  		else if (((List.length a)<(List.length b))) then false
			  		else (match a with
			  			| [] -> true			(*both have length =0*)
			  			| x::xs-> (match b with
			  					| [] -> true (*Not possible cases*)
			  					| y::ys -> if(x>y) then false
			  						   	   else if (x<y) then false
			  						   	   else equal xs ys));;

(*return true if val(a)=val(b)*)
let rec eq x y = let (a,b)=x in
				 let (c,d)=y in
				 if(a==c) then (equal b d)
				 else false;;

(* unary operation *)
let minus a = let (x,y) = a in
			  if ((equal [0] y)||(equal [] y)) then (NonNeg,[0]) 	(*special case if y = [0]*)
			  else
			  	match x with
					| Neg -> (NonNeg,y)
					| _ -> (Neg,y);;

(*this function will trim zeroes in front of list a*)
let rec cut_zero a = match a with
	| [] -> []
	| x::xs -> ( if x==0 then cut_zero xs
				 else a);;

(* add constant 'a' to list l assuming l is in reverse order gives answer in reverse order*)
let rec add_c a l = match l with
	| [] -> if a=0 then [] else [a]
	| x::xs -> if (x+a>9) then  (x+a) mod 10::(add_c 1 xs)
			   else (x+a) :: xs;;


(*add two lists given in reverse order to give answer in reverse order. Here c is carry*)
let rec add_list c a b = match a with
		| [] -> add_c c b 			   (*if a is empty then add c to b*)
		| x::xs -> (match b with
				| [] -> add_c c a    	(*if b is empty then add c to a*)
				| y::ys ->if (x+y+c>9) then (x+y+c) mod 10::(add_list 1 xs ys)
						  else ((x+y+c) mod 10)::(add_list 0 xs ys));;

(*subtract two lists given in reverse order to give answer in reverse order. Here c is carry*)
let rec sub_list a l1 l2 = match l1 with
		| [] -> []
		| x::xs ->( match l2 with
					| [] ->(if a==0 then l1
				 			else if (x-1)<0 then 9::(sub_list 1 xs l2)
				 			else (x-1)::xs )
					| y::ys -> (if a==0 then ( if (x-y)<0 then (10+x-y)::(sub_list 1 xs ys)
											   else (x-y)::(sub_list 0 xs ys))
								else
									if(x-y-1)<0 then (10+x-y-1)::(sub_list 1 xs ys)
									else (x-y-1)::(sub_list 0 xs ys)));;

(*this function will add two bigint*)
let add x y = let (a,b) = x in
			  let (c,d)=y in
		 	  match a with
				|Neg -> (match c with
						|Neg -> (Neg, List.rev (add_list 0 (List.rev b) (List.rev d)))
						|NonNeg -> if (gte d b) then (NonNeg, cut_zero (List.rev (sub_list 0 (List.rev d) (List.rev b)))) 			(*we have to peform d-b*)
								   else (Neg, cut_zero (List.rev (sub_list 0 (List.rev b) (List.rev d)))))
				|NonNeg -> (match c with
						|Neg -> if (gte b d) then (NonNeg, cut_zero (List.rev (sub_list 0 (List.rev b) (List.rev d))))			(*we have to peform b-d*)
								else (Neg, cut_zero (List.rev (sub_list 0 (List.rev d) (List.rev b))))
						|NonNeg -> (NonNeg, List.rev (add_list 0 (List.rev b) (List.rev d))));;

(*this function will subtract two bigint*)
let sub x y = let (a,b) = x in
			  let (c,d)=y in
			  match a with
				|Neg -> (match c with
						|Neg -> if (gte d b) then (NonNeg, cut_zero (List.rev (sub_list 0 (List.rev d) (List.rev b)))) 			(*we have to peform d-b*)
						 		else (Neg, cut_zero (List.rev (sub_list 0 (List.rev b) (List.rev d))))
						|NonNeg -> (Neg, List.rev (add_list 0 (List.rev b) (List.rev d))))
				|NonNeg -> (match c with
						|Neg -> (NonNeg, List.rev (add_list 0 (List.rev b) (List.rev d)))
						|NonNeg -> if (gte b d) then (NonNeg, cut_zero (List.rev (sub_list 0 (List.rev b) (List.rev d))))			(*we have to peform b-d*)
								   else (Neg, cut_zero (List.rev (sub_list 0 (List.rev d) (List.rev b)))));;

(*multiply constant 'a' with list. Input is taken in reverse order to give output in correct order. Here c is carry*)
let rec mult_c c a l = match l with
	| [] -> [c]
	| x::xs -> (if ((a*x+c) < 10) then cut_zero ((mult_c 0 a xs)@[(a*x+c)])
				else cut_zero ((mult_c ((a*x+c)/10) a xs)@[((a*x+c) mod 10)]));;

(* multiply two lists taken in reverse order to produce a list in correct order *)
let rec mult_list a b = match a with
			| [] -> []
			| x::xs -> cut_zero (List.rev (add_list 0 (List.rev (mult_c 0 10 (List.rev (mult_list xs b)))) (List.rev (mult_c 0 x b))));;

(*Multiply two bigint*)
let mult x y  = let (a,b) = x in
				let (c,d)=y in
				if ((equal [0] b)||(equal [0] d)) then (NonNeg,[0])
				else if (a==c) then (NonNeg, mult_list (List.rev b) ((List.rev d)))
				else (Neg, mult_list (List.rev b) ((List.rev d)));;

(*FInds quotient, Take 2 lists in same order to give l1/l2 = i(integer) where i is between 0-9*)
let rec find i l1 l2 =
		if (less_than l1 (mult_c 0 i (List.rev l2))) then (i-1)
		else (find (i+1) l1 l2);;

(*Divide two list l1 and l2 and returns a tuple (q,r) where q is quotient and r is remainder but *)
let rec divi l1 l2 = let q = find 0 (cut_zero l1) l2 in
					 let r = cut_zero(List.rev (sub_list 0 (List.rev l1) (List.rev (mult_c 0 q (List.rev l2))))) in
					 (q,r);;

(*Divides two list with the help of fnction divi*)
let rec div_helper l l1 l2 = match l1 with
| [] -> []
| x::xs -> if ( gte (l@[x]) l2) then
				let (q,r) = (divi (l@[x]) l2) in
				q::(div_helper r xs l2)
		   else [0]@(div_helper (l@[x]) xs l2);;

(*divide two big int*)
let div x y = let (a,b) = x in
			  let (c,d)=y in
			  if ((equal [0] d)||(equal [] d)||(equal [] b)) then raise CannotDivide		(*raise error since cannot divide by zero*)
			  else
			  	let k=(cut_zero (div_helper [] b d)) in
			  	if (equal k []) then (NonNeg, [0])
			  	else
				   if (a==c) then (NonNeg,cut_zero (div_helper [] b d))
				   else (Neg,cut_zero  (div_helper [] b d));;

(*helper function to find remainder. Takes two list in same order to return outpur in correct order*)
let rem_helper l1 l2 = let x = (cut_zero (div_helper [] l1 l2)) in
					   if (equal x []) then l1
					   else
							let k = cut_zero(List.rev (sub_list 0 (List.rev l1) (List.rev (mult_list (List.rev x) (List.rev l2))))) in
							if (equal k []) then [0]
							else k

(*this function will give remainder*)
let rem x y = let (a,b) = x in
			  let (c,d)=y in
			  if ((equal [0] d)||(equal [] d)||(equal [] b)) then raise CannotDivide		(*raise error since cannot divide by zero*)
			  else
			  	let k = (rem_helper b d) in
			  	if (equal [0] k) then (NonNeg,[0])
			  	else if(a==c) then (NonNeg,k)
			  	else (Neg,k);;

(*helper function in converting int list to string*)
let rec num_helper l = match l with
			|[] -> ""
			|x::xs -> (string_of_int x)^(num_helper xs);;

(*return a list of string from bigint*)
let give_num x = let (a,b) = x in
				  match a with
					|Neg -> "-"^(num_helper b)
					|NonNeg -> ""^(num_helper b)
