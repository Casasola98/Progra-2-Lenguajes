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
    [] => true (*Condicion de parada*)
  | hd::[] => true (*Condicion de parada*)
  | (s,r)::((s1,r1)::rest) => (case (s, s1) of (*verifica que la siguiente carta en la lista sea del mismo color*)
	 (Spades, Spades) => true (*Utiliza Pattern Matchin Anidado para verificar que sean del mismo color*)
       | (Clubs, Spades) => true
       | (Clubs, Clubs) => true
       | (Spades, Clubs) => true
       | (Hearts, Hearts) => true
       | (Diamonds, Hearts) => true
       | (Diamonds, Diamonds) => true
       | (Hearts, Diamonds) => true
       | (_,_) => false) andalso all_same_color((s1, r1)::rest) (*Realiza llamada recursiva eliminando la primera carta de la lista*)

(*Retorna la suma de todas las cartas en la lista*)
fun sum_cards (cs : card list) = 
    let
	fun sum_aux (cs, sum)=
	    case cs of
		[] => sum (*Condicion de parada, retorna la suma cuando la lista esta vacia*)
	      | hd::tl => sum_aux(tl, sum + card_value(hd)) (*Llamada recursiva, le suma el valor de la primera carta y la elimina del inicio de la lista*)
    in
	sum_aux(cs,0) (*Llama a la funcion auxiliar*)
    end

(*Retorna la puntuacion de la lista de cartas dada*)	
fun score (cs : card list, goal : int) = let
    val sum = sum_cards cs (*Obtiene la suma de las cartas*)
    val pre = if (sum > goal) then 3 * (sum - goal) else (goal - sum)
  in
    if (all_same_color cs) then pre div 2 else pre
  end

(*Corre el juego*)
fun officiate (cards : card list, moves : move list, goal : int) = 
    let
	fun officiate_aux (cards : card list, held : card list, moves : move list, goal : int) =
	    case (cards, held, moves, goal) of
		(_, _, [], _) => score (held, goal) (*Condicion de parada: No mas movimientos*)
	      | ([], _, _, _) => score (held, goal) (*Condicion de parada: No mas cartas*)
	      | (c::cs, _, m::ms, _) => case m of
				      Discard d => officiate_aux (c::cs, remove_card (held, d, IllegalMove), ms, goal)
				    | Draw => case c::cs of
						  [] => score (held, goal)  (*Condicion de parada: No mas cartas*)
						  | _ => 
						    let
							val held' = c::held
							val held_sum = sum_cards (held')
						    in
							if (held_sum > goal)  (*Condicion de parada: Suma mas grande que goal*)
							then score (held', goal) 
							else officiate_aux (cs, held', ms, goal) (*Continua el juego*)
						    end
    in
	officiate_aux (cards, [] , moves, goal) (*Llama a funcion auxiliar*)
    end

