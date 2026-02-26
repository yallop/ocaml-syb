open Syb_constructors
open Syb_classes

(* Various utilities *)
let unjust l o = match o with Some x -> x :: l | None -> l
let singleton = function [s] -> Some s | _ -> None
let sum l = List.fold_left (+) 0 l
let maximum = List.fold_left max 0


(** Apply a transformation everywhere in bottom-up manner *)
let rec everywhere : genericT -> genericT =
  fun f (module X) x -> f (module X) (gmapT (everywhere f) (module X) x)


(** Apply a transformation everywhere in top-down manner *)
let rec everywhere' : genericT -> genericT =
  fun f (module X) x -> gmapT (everywhere' f) (module X) (f (module X) x)


(** Variation on everywhere with an extra stop condition *)
let rec everywhereBut : bool genericQ -> genericT -> genericT =
  fun stop f (module X) x ->
    if stop (module X) x then x else f (module X) ((gmapT (everywhereBut stop f) (module X)) x)


(** Monadic variation on everywhere *)
(* [TODO]: everywhereM *)


(** Apply a monadic transformation at least somewhere *)
(* [TODO]: somewhere *)


(** Summarise all nodes in top-down, left-to-right order *)
let rec everything : 'r. ('r -> 'r -> 'r) -> 'r genericQ -> 'r genericQ =
  fun (@) g (module X) x ->
    let f = g (module X) x in  List.fold_left (@) f (gmapQ (everything (@) g) (module X) x)


(** Variation of "everything" with an added stop condition *)
let rec everythingBut : 'r. ('r -> 'r -> 'r) -> ('r * bool) genericQ -> 'r genericQ =
  fun (@) stop (module X) x ->
    match stop (module X) x with
    | v, true -> v
    | v, false -> List.fold_left (@) v (gmapQ (everythingBut (@) stop) (module X) x)


(** Summarise all nodes in top-down, left-to-right order, carrying some state
    down the tree during the computation, but not left-to-right to
    siblings. *)
(* [TODO]: everythingWithContext *)


(** Get a list of all entities that meet a predicate *)
let listify (module R:TYPEABLE) p =
  everything (@) (mkQ (module R) [] (fun x -> if p x then [x] else []))


(** Look up a subterm by means of a maybe-typed filter *)
(* [TODO]: something *)


(** Bottom-up synthesis of a data structure;
    1st argument z is the initial element for the synthesis;
    2nd argument o is for reduction of results from subterms;
    3rd argument f updates the synthesised data according to the given term
*)
let rec synthesize :
  's 't.'s -> ('t -> 's -> 's) -> ('s -> 't) genericQ -> 't genericQ =
  fun z o f (module X) x ->
   f (module X) x (List.fold_right o (gmapQ (synthesize z o f) (module X) x) z)


(** Compute size of an arbitrary data structure *)
let rec gsize : int genericQ =
  fun (module D) v -> 1 + sum (gmapQ gsize (module D) v)


(** Count the number of immediate subterms of the given term *)
let glength : int genericQ =
  fun (module T) x -> List.length (gmapQ (fun (module Z) _ ->  ()) (module T) x)


(** Determine depth of the given term *)
let rec gdepth : int genericQ =
  fun (module D) x -> succ (maximum (gmapQ gdepth (module D) x))

(** Determine the number of all suitable nodes in a given term *)
let gcount : bool genericQ -> int genericQ =
  fun p (module T) x ->
  everything (+)
    (fun (module X) x ->  if p (module X) x then 1 else 0 )
    (module T)
    x


(** Determine the number of all nodes in a given term *)
let gnodecount : int genericQ =
  fun (module X) x -> gcount (fun (module Y) _ ->  true ) (module X) x


(** Determine the number of nodes of a given type in a given term *)
let gtypecount : (module X:TYPEABLE) -> X.t -> int genericQ =
  fun (module X) x -> gcount (mkQ (module X) false  (fun _ -> true))


(** Find (unambiguously) an immediate subterm of a given type *)
let gfindtype : (module X:TYPEABLE) -> X.t option genericQ =
  fun (module X) (module D) x ->
   singleton (List.fold_left unjust []
               (gmapQ (mkQ (module X) None (fun c -> Some c)) (module D) x))


(** Generic show *)
let rec gshow : string genericQ =
  fun (module D) v ->
  "("^ string_of_constructor (constructor (module D) v)
  ^ String.concat " " (gmapQ gshow (module D) v)
  ^ ")"
