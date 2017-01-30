(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Shawn Chin-Bellemare, Id Number: 260640152 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code must compile and must not go into infinite
loops.  *)

(* Question 1 *) (* Do not edit this line. *)

(* val sumlist : l:float list -> float *)
let rec sumlist l = 
    match l with
    | [] -> 0.0
    | x::xs -> x + sumlist xs

(* val squarelist : l:float list -> float list *)
let rec squarelist l = List.map (fun x -> x**2.0) l

(* val mean : l:float list -> float *)
let mean l = sumlist l / (float l.Length)

(* val mean_diffs : l:float list -> float list *)
let mean_diffs l = List.map (fun x -> x - mean l) l


(* val variance : l:float list -> float *)
let variance l = mean (squarelist (mean_diffs l))


(* End of question 1 *) (* Do not edit this line. *)


(* Question 2 *) (* Do not edit this line. *)

(* val memberof : 'a * 'a list -> bool when 'a : equality *)
let rec memberof l = 
    match l with
    | (c,[]) -> false
    | (c, x::xs) -> if c = x then true else memberof (c, xs)


(* val remove : 'a * 'a list -> 'a list when 'a : equality *)
let rec remove l = 
    match l with
    | (c,[]) -> []
    | (c, x::xs) -> if c = x then remove (c,xs) else x::(remove (c,xs))


(* End of question 2 *) (* Do not edit this line *)



(* Question 3 *) (* Do not edit this line *)

(* val isolate : l:'a list -> 'a list when 'a : equality *)
let rec isolate l = 
    match l with
    | [] -> []
    | x::xs -> if memberof (x, xs) then isolate xs else x::isolate xs


(* End of question 3 *) (* Do not edit this line *)


(* Question 4 *) (* Do not edit this line *)

(* val common : 'a list * 'a list -> 'a list when 'a : equality *)
let rec common l = 
    match l with
    | ([],_) -> []
    | (x::xs, c) -> if memberof(x,c) then x::(common ((isolate xs), c)) else common ((isolate xs), c) 

(* End of question 4 *) (* Do not edit this line *)

(* Question 5 *) (* Do not edit this line *)

(* val split : l:'a list -> 'a list * 'a list *)
let rec split l = 
    match l with
    | [] -> ([], [])
    | [c] -> ([c], [])
    | x::xs::xt -> let (y, ys) = split xt in (x::y, xs::ys)

(* val merge : 'a list * 'a list -> 'a list when 'a : comparison *)
let rec merge l =
    match l with
    | (x,[]) -> x
    | ([],y) -> y
    | (xs::xt, ys::yt) -> if xs<=ys then xs::(merge (xt, ys::yt)) else ys::(merge (xs::xt, yt))

(* val mergesort : l:'a list -> 'a list when 'a : comparison *)
let rec mergesort l =
    match l with
    | [] -> []
    | [x] -> [x]
    | xs -> let (y,ys) = split xs in merge (mergesort y, mergesort ys)


(* End of question 5 *) (* Do not edit this line *)

