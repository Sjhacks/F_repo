(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Shawn Chin-Bellemare, Id Number: 260640152 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code must compile and must not go into infinite
loops.  You are free to use library functions now.  But loops are BANNED
as is any kind of imperative code based on updating values.  You can make functions
recursive at top level or modify them so that the recursion is hidden inside.  The
only things we really insist on are: (a) use the names we have used and (b) the
functions must have the types that we have shown.  We hope by now you understand that
everywhere where it says failwith "Error - not implemented" you have to remove this
and replace it with your code.  *)

(* Question 1 *)

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)
(* val deriv : f:(float -> float) * dx:float -> x:float -> float *)

let rec newton(f,guess:float,tol:float,dx:float) = 
    if ((f guess) < tol) then guess else newton(f, guess-((f guess)/(deriv(f, dx) guess)), tol, dx )
(* val newton : f:(float -> float) * guess:float * tol:float * dx:float -> float *)

(* For testing 
let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c);;
newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001);;
*)

(* Question 2 *)

type term = float * int
type poly = term list

exception EmptyList

(* Multiply a term by a polynomial. *)
let mtp(t:term,p:poly):poly =
    let (a,b) = t in
        List.map (fun (x,y) -> (x*a, y+b)) p
(* val mtp : t:term * p:poly -> poly *)

(* Add a term to a polynomial. *)
let rec atp(t:term,p:poly):poly = 
    let (a,b) = t in
        match p with
        | [] -> [t]
        | (c,d)::xs -> if b = d then (a+c,d)::xs elif b > d then (a,b)::(c,d)::xs else (c,d)::(atp (t,xs))
(* val atp : t:term * p:poly -> poly *)

(* Add two polynomials.  The result must be properly represented. This means you
cannot have more than one term with the same exponent, you should not have a
term with a zero coefficient, except when the whole polynomial is zero and the
terms should be decreasing order of exponents.   Thus, for example,
5.2 x^7 - 3.8 x^4 +2.0 x - 1729.0 should be represented as
[(5.2,7);(-3.8,4);(2.0,1);(-1729.0,0)] *)

let rec addpolys(p1:poly,p2:poly):poly =
    match p1 with
    | [] -> p2
    | x::xs -> addpolys (xs, atp (x,p2))
(* val addpolys : p1:poly * p2:poly -> poly *)

(* Multiply two polynomials.  All the remarks above apply here too. Raise an
exception if one of the polynomials is the empty list. *)
let rec multpolys(p1:poly,p2:poly) =
    match p1 with
    | [] -> p2
    | x::xs -> multpolys (xs, mtp(x,p2))
(* val multpolys : p1:poly * p2:poly -> poly *)

(* This is the tail-recursive version of Russian peasant exponentiation.  I have
done it for you.  You will need it for the next question.  *)
let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

(* Here is how you evaluate a term. *)
let evalterm (v:float) ((c,e):term) = if (e=0) then c else c * exp(v,e)

(* Evaluate a polynomial viewed as a function of the indeterminate.  Use the function
above and List.fold and List.map and a dynamically created function for a one-line
answer.  *)
let evalpoly(p:poly,v:float):float = List.fold (fun acc t -> acc+ evalterm v t ) 0.0 p
(* val evalpoly : p:poly * v:float -> float *)

(* Compute the derivative of a polynomial as a symbolic representation.  Do NOT use
deriv defined above.  I want the answer to be a polynomial represented as a list.
I have done a couple of lines so you can see how to raise an exception.  *)
let rec diff (p:poly):poly = 
  match p with
    | [] -> raise EmptyList
    | (a,b)::xs -> if not (b = 0) then (a*(float b),b-1)::(diff xs) else []
(*  val diff : p:poly -> poly *)
    
(* Question 3 *)
(* Most of these functions are only one or two lines.  One of them, the longest is
about 5 lines.  However, they require some thought.  They are short because I used
the Set library functions wherever I could.  I especially found Set.fold useful. *)

type Country = string
type Chart = Set<Country*Country>
type Colour = Set<Country>
type Colouring = Set<Colour>

(* This is how you tell that two countries are neghbours.  It requires a chart.*)
let areNeighbours ct1 ct2 chart =
  Set.contains (ct1,ct2) chart || Set.contains (ct2,ct1) chart
(* val areNeighbours :
  ct1:'a -> ct2:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
  *)

(* The colour col can be extended by the country ct when they are no neighbours
according to chart.*)
  
let canBeExtBy col ct chart = 
    Set.forall (fun c -> not (areNeighbours c ct chart)) col
(*
   val canBeExtBy :
  col:Set<'a> -> ct:'a -> chart:Set<'a * 'a> -> bool when 'a : comparison
*)

(* Here you have to extend a colouring by a fixed country. *)
let rec extColouring (chart: Chart) (colours : Colouring) (country : Country) =
    if Set.isEmpty colours then Set.singleton (Set.singleton country)
    elif canBeExtBy (Set.minElement colours) country chart then Set.add (Set.add country (Set.minElement colours)) (Set.remove (Set.minElement colours) colours)
    else Set.add (Set.minElement colours) (extColouring chart (Set.remove (Set.minElement colours) colours) country)
(*
val extColouring :
  chart:Chart -> colours:Colouring -> country:Country -> Set<Set<Country>>
*)

(* This collects the names of the countries in the chart.  A good place
to use Set.fold *) 
let countriesInChart (chart : Chart) = Set.fold (fun set (a,b) -> Set.add a (Set.add b set)) Set.empty chart
(* val countriesInChart : chart:Chart -> Set<Country> *)

(* Here is the final function.  It is also most conveniently done with Set.fold *)
let colourTheCountries (chart: Chart)  = Set.fold (extColouring chart) Set.empty (countriesInChart chart)
(* val colourTheCountries : chart:Chart -> Colouring *)

(* Question 4 *)

(* These functions are a bit longer but easier to code than Q3.  It is very similar
to the evaluator that I showed in class.  However I have used the Option type so that
the program gracefully returns None if no value is found.  This can be preferred to
raising an exception in some situations.  Learn option types from the web.  *)

type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* The bindings are stored in a list rather than a BST for simplicity.  The
list is sorted by name, which is a string. *)
let rec lookup(name:string, env: Bindings) =
    match env with
    | [] -> None
    | (a,b)::xs -> if name = a then Some b else lookup (name, xs)
(* val lookup : name:string * env:Bindings -> int option *)

(* Insert a new binding.  If the name is already there then the new binding should
be put in front of it so that the lookup finds the latest binding.  *)
let rec insert(name:string, value: int, b: Bindings) = 
    match b with
    | [] -> [name,value]
    | (a,b)::xs -> if name = a then (name,value)::(a,b)::xs else insert (name, value, xs)
(* val insert : name:string * value:int * b:Bindings -> (string * int) list*)

(* The recursive evaluator.  You have to match on the exp.  If a variable is not
found return None.  If you are applying an operator to None and something else the
answer is None.  This leads to a program of about 20 lines but it is conceptually
very easy.  *)

let rec eval(exp : Exptree, env:Bindings) = failwith "Error - not implemented"
        
(* val eval : exp:Exptree * env:Bindings -> int option  *)



