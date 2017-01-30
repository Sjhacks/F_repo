module hw4

(* Assignment 4 *) (* Do not edit this line. *)
(* Student name: Shawn Chin-Bellemare, Id Number: 260640152 *) (* Edit this line. *)


type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp

type substitution = (char * typExp) list

(* check if a variable occurs in a term *)
let rec occurCheck (v: char) (tau: typExp) : bool =
    match tau with
    | TypInt -> false
    | TypVar x -> if v = x then true else false 
    | Arrow (z,y) -> 
        let o = occurCheck v z
        match o with
        | true -> true
        | false -> occurCheck v y
        
    | Lst(w) -> occurCheck v w
    

(* substitute typExp tau1 for all occurrences of type variable v in typExp tau2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  | TypInt -> TypInt
  | TypVar x -> if v=x then tau1 else TypVar x
  | Arrow (z,y) -> Arrow ((substitute tau1 v z), substitute tau1 v y)
  | Lst (w) -> Lst(substitute tau1 v w)

let applySubst (sigma: substitution) (tau: typExp) : typExp =
    List.fold (fun acc (c,t) -> substitute t c acc) tau sigma
(* This is a one-line program *)



let rec unify (tau1: typExp) (tau2:typExp) : substitution =
    match (tau1, tau2) with
    | (TypInt,TypInt) -> []
    | (TypInt, TypVar x) | (TypVar x,TypInt)-> [x, TypInt]
    | (TypVar x, TypVar y) -> if x=y then [] else [x , TypVar y]
    | (Arrow(x,z), Arrow(y,w)) -> (unify x y) @ (unify (applySubst (unify x y) z) (applySubst (unify x y) w))
    | (Arrow(s,t) as x, TypVar y) -> if occurCheck y x then failwith "failed occurcheck" else [y,x] 
    | (TypVar y, (Arrow(s,t) as x)) -> if occurCheck y x then failwith "failed occurs check" else [y, x]  
    | (TypInt, Arrow(_,_)) | (Arrow(_,_), TypInt) -> failwith "not unifiable"
    | (Arrow(_,_), Lst (w)) | (Lst (w), Arrow(_,_)) -> failwith "clash in principal type constructor"
    | (Lst (w),(_ as x)) -> unify w x
    | ((_ as x),Lst (w)) -> unify x w
(* Use the following signals if unification is not possible:

 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"

*)


(*

> let te4 = Prod(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

val te4 : typExp = Prod (TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

val te3 : typExp = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))

> unify te3 te4;;
val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]
> let result = it;;

val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> applySubst result te3;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))
> applySubst result te4;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))

*)


    

    
(*

> let te4 = Prod(TypInt, Arrow(TypVar 'c', TypVar 'a'));;

val te4 : typExp = Prod (TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;

val te3 : typExp = Prod (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))

> unify te3 te4;;
val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]
> let result = it;;

val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> applySubst result te3;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))
> applySubst result te4;;
val it : typExp = Prod (TypInt,Arrow (TypInt,TypInt))

*)


  
