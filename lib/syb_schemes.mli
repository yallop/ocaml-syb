open Syb_classes

(** Apply a transformation everywhere in bottom-up manner *)
val everywhere : genericT -> genericT

(** Apply a transformation everywhere in top-down manner *)
val everywhere' : genericT -> genericT

(** Variation on everywhere with an extra stop condition *)
val everywhereBut : bool genericQ -> genericT -> genericT

(** Monadic variation on everywhere *)
(* val everywhereM : (Data a, Monad m) => GenericM m -> a -> m a *)

(** Apply a monadic transformation at least somewhere *)
(* val somewhere : (MonadPlus m, Data a) => GenericM m -> a -> m a *)

(** Summarise all nodes in top-down, left-to-right order *)
val everything : ('r -> 'r -> 'r) -> 'r genericQ -> 'r genericQ

(** Variation of "everything" with an added stop condition *)
val everythingBut : ('r -> 'r -> 'r) -> ('r * bool) genericQ -> 'r genericQ

(** Summarise all nodes in top-down, left-to-right order, carrying some state
    down the tree during the computation, but not left-to-right to
    siblings. *)
(*
val everythingWithContext : {X: Data} -> 's -> ('r -> 'r -> 'r) ->
  ({Y:Data} -> Y.t code -> 's code -> ('r * 's) code) ->
  X.t code -> 'r code
*)

(** Get a list of all entities that meet a predicate *)
val listify : 'r typeable -> ('r -> bool) -> 'r list genericQ

(** Look up a subterm by means of a maybe-typed filter *)
(*
val something : 'u option genericQ -> 'u option genericQ
*)

(** Bottom-up synthesis of a data structure;
    1st argument z is the initial element for the synthesis;
    2nd argument o is for reduction of results from subterms;
    3rd argument f updates the synthesised data according to the given term
*)
val synthesize : 's -> ('t -> 's -> 's) -> ('s -> 't) genericQ -> 't genericQ

(** Compute size of an arbitrary data structure *)
val gsize : int genericQ

(** Count the number of immediate subterms of the given term *)
val glength : int genericQ

(** Determine depth of the given term *)
val gdepth : int genericQ

(** Determine the number of all suitable nodes in a given term *)
val gcount : bool genericQ -> int genericQ

(** Determine the number of all nodes in a given term *)
val gnodecount : int genericQ

(** Determine the number of nodes of a given type in a given term *)
val gtypecount : 'x typeable -> 'x -> int genericQ

(** Find (unambiguously) an immediate subterm of a given type *)
val gfindtype : 'x typeable -> 'x option genericQ

(** Generic show *)
val gshow : string genericQ
