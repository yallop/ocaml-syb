open Syb_constructors
open Syb_classes

(* Various utilities *)
let unjust l o = match o with Some x -> x :: l | None -> l
let singleton = function [s] -> Some s | _ -> None
let sum l = List.fold_left (+) 0 l
let maximum = List.fold_left max 0

(** Apply a transformation everywhere in bottom-up manner *)
let rec everywhere : genericT -> genericT =
  fun f ->
    { t = 
        fun data x -> f.t data ((gmapT (everywhere f)).t data x) }


(** Apply a transformation everywhere in top-down manner *)
let rec everywhere' : genericT -> genericT =
  fun f ->
    { t =
        fun data x -> (gmapT (everywhere' f)).t data (f.t data x) }

(** Variation on everywhere with an extra stop condition *)
let rec everywhereBut : bool genericQ -> genericT -> genericT =
  fun stop f ->
    { t =
        fun data x ->
          if stop.q data x then x
          else f.t data ((gmapT (everywhereBut stop f)).t data x) }


(** Monadic variation on everywhere *)
(* [TODO]: everywhereM *)


(** Apply a monadic transformation at least somewhere *)
(* [TODO]: somewhere *)


(** Summarise all nodes in top-down, left-to-right order *)
let rec everything : 'r. ('r -> 'r -> 'r) -> 'r genericQ -> 'r genericQ =
  fun h g ->
    { q =
        fun data x ->
          let f = g.q data x in
          List.fold_left h f
            ((gmapQ (everything h g)).q data x) }


(** Variation of "everything" with an added stop condition *)
let rec everythingBut : 'r. ('r -> 'r -> 'r) -> ('r * bool) genericQ -> 'r genericQ =
  fun (@) (stop : _ genericQ) ->
    { q =
        fun data x ->
          match stop.q data x with
          | v, true -> v
          | v, false -> List.fold_left (@) v
                          ((gmapQ (everythingBut (@) stop)).q data x) }


(** Summarise all nodes in top-down, left-to-right order, carrying some state
    down the tree during the computation, but not left-to-right to
    siblings. *)
(* [TODO]: everythingWithContext *)


(** Get a list of all entities that meet a predicate *)
let listify (r : _ typeable) p =
  everything (@) (mkQ r [] (fun x ->  if p x then [x] else []))


(** Look up a subterm by means of a maybe-typed filter *)
(* [TODO]: something *)


(** Bottom-up synthesis of a data structure;
    1st argument z is the initial element for the synthesis;
    2nd argument o is for reduction of results from subterms;
    3rd argument f updates the synthesised data according to the given term
*)
let rec synthesize :
  's 't.'s -> ('t -> 's -> 's) -> ('s -> 't) genericQ -> 't genericQ =
  fun z o (f : _ genericQ) ->
    { q =
        fun data x ->
          f.q data x (List.fold_right o ((gmapQ (synthesize z o f)).q data x) z) }


(** Compute size of an arbitrary data structure *)
let rec gsize : int genericQ =
  { q = fun data v -> 1 + sum ((gmapQ gsize).q data v) }


(** Count the number of immediate subterms of the given term *)
let glength : int genericQ =
  { q =
      fun data x -> List.length ((gmapQ { q = fun data _ ->  () }).q data x) }


(** Determine depth of the given term *)
let rec gdepth =
  { q =
      fun data x -> succ (maximum ((gmapQ gdepth).q data x)) }


(** Determine the number of all suitable nodes in a given term *)
let gcount (p : bool genericQ) =
  { q =
      fun data x ->
        (everything (+)
           {q = fun data x ->  if p.q data x then 1 else 0 })
        .q data
          x }


(** Determine the number of all nodes in a given term *)
let gnodecount =
  { q =
      fun data x -> (gcount {q = fun data _ ->  true }).q data x }


(** Determine the number of nodes of a given type in a given term *)
let gtypecount typeable x = gcount (mkQ typeable false (fun _ -> true))


(** Find (unambiguously) an immediate subterm of a given type *)
let gfindtype typeable =
  { q =
      fun data x ->
        singleton (List.fold_left unjust []
                     ((gmapQ (mkQ typeable None (fun c -> Some c))).q data x)) }


(** Generic show *)
let rec gshow =
  { q =
      fun data v ->
        "("^ string_of_constructor (constructor.q data v)
        ^ String.concat " " ((gmapQ gshow).q data v)
        ^ ")" }
