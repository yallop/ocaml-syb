open Higher

(* Equality *)
type (_, _) eql = Refl : ('a, 'a) eql

(* Type representations *)
type _ type_rep = ..

(* Our analogue to the typeable class *)
module type TYPEABLE =
sig
  type t
  val type_rep : unit -> t type_rep
  val eqty : 's type_rep -> (t, 's) eql option
end

(* Equality test *)
val (=~~=) : {A:TYPEABLE} -> {B:TYPEABLE} -> (A.t, B.t) eql option

module rec R :
sig
  type    genericT = {T: R.DATA} -> T.t -> T.t
  type 'u genericQ = {T: R.DATA} -> T.t -> 'u
  type 'c genericFapp  =
    < g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app >
  type 'c genericFunit = < u: 'g. 'g -> ('g, 'c) app >
  module type DATA =
  sig
    type t
    module Typeable : TYPEABLE with type t = t
    val gmapT : genericT -> t -> t
    val gmapQ : 'u genericQ -> t -> 'u list
    (* val gfoldl : 'c genericFapp -> 'c genericFunit -> t -> (t, 'c) app *)
    val constructor: t -> Syb_constructors.constructor
  end
end
include module type of R

val gmapT : genericT -> genericT
val gmapQ : 'u genericQ -> 'u list genericQ
(* val gfoldl : 'c genericFapp -> 'c genericFunit -> *)
(*              {T: DATA} -> T.t -> (T.t, 'c) app *)
val constructor : Syb_constructors.constructor genericQ

val mkT : {T:TYPEABLE} -> (T.t -> T.t) -> genericT
val mkQ : {T:TYPEABLE} -> 'u -> (T.t -> 'u) -> 'u genericQ
