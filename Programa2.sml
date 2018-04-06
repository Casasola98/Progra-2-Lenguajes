(* Jose Gomez Casasola*)
(* Orlando Hidalgo *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*Retorna el color de la carta*)
fun card_color (s : suit, r : rank) =
    case s of
	(Diamonds | Hearts) => Red (*Diamantes y corazones rojos*)
      | (Clubs | Spades)  => Black (*Picas y treboles negros*)

(*Retorna el valor de la carta*)
fun card_value (s : suit, r: rank) =
    case r of 
	Num(v) => v (*si es un numero del 2 al 10, retorna el valor*)
      | (Jack | Queen | King) => 10 (*J, Q y K, retorna 10*)
      |   Ace => 11 (*Si es un as, retorna 11*)

(*Remueve la carta solicitada*)
fun remove_card (cs : card list, c : card, e) = 
    case cs of
	[] => raise e (*Si la carta no estaba entre la lista retorna la excepcion*)
      | hd::tl => if (hd = c ) 
		      then tl (*Si la carta que esta en el head es la carta a eliminar retorna el tail*)
		      else  hd :: remove_card(tl, c, e) (*Si la carta no lo es, sigue buscando*)

(*Retorna si todas las cartas son del mismo color*)
fun all_same_color (cs : card list) =
    case cs of
    [] => true
  | hd::[] => true
  | (s,r)::((s1,r1)::rest) => (case (s, s1) of 
	 (Spades, Spades) => true
       | (Clubs, Spades) => true
       | (Clubs, Clubs) => true
       | (Spades, Clubs) => true
       | (Hearts, Hearts) => true
       | (Diamonds, Hearts) => true
       | (Diamonds, Diamonds) => true
       | (Hearts, Diamonds) => true
       | (_,_) => false) andalso all_same_color((s1, r1)::rest)


fun sum_cards (cs : card list) = 
    let
	fun sum_aux (cs, sum)=
	    case cs of
		[] => sum
	      | hd::tl => sum_aux(tl, sum + card_value(hd)) 
    in
	sum_aux(cs,0)
    end

	
fun score (cs : card list, goal : int) = let
    val sum = sum_cards cs
    val pre = if (sum > goal) then 3 * (sum - goal) else (goal - sum)
  in
    if (all_same_color cs) then pre div 2 else pre
  end


fun officiate (cards : card list, moves : move list, goal : int) = 
    let
	fun officiate_aux (cards : card list, held : card list, moves : move list, goal : int) =
	    case (cards, held, moves, goal) of
		(_, _, [], _) => score (held, goal)
	      | ([], _, _, _) => score (held, goal)
	      | (c::cs, _, m::ms, _) => case m of
				      Discard d => officiate_aux (c::cs, remove_card (held, d, IllegalMove), ms, goal)
				    | Draw => case c::cs of
						  [] => score (held, goal)
						  | _ => 
						    let
							val held' = c::held
							val held_sum = sum_cards (held')
						    in
							if (held_sum > goal)
							then score (held', goal)
							else officiate_aux (cs, held', ms, goal)
						    end
    in
	officiate_aux (cards, [] , moves, goal)
    end

