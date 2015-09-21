(* SYB-style equality, using extensible variants to avoid the unsafe cast. *)

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

let (=~~=) {A: TYPEABLE} {B: TYPEABLE} = A.eqty (B.type_rep ())

(* Implicit instances *)
module rec R :
sig
  type   genericT = {D:R.DATA} -> D.t -> D.t
  and 'u genericQ = {D:R.DATA} -> D.t -> 'u
  and 'c genericFapp  =
    < g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app >
  and 'c genericFunit = < u: 'g. 'g -> ('g, 'c) app >

  module type DATA =
  sig
    type t
    module Typeable : TYPEABLE with type t = t
    val gmapT : genericT -> t -> t
    val gmapQ : 'u genericQ -> t -> 'u list
    val gfoldl : 'c genericFapp -> 'c genericFunit -> t -> (t, 'c) app
    val constructor: t -> Syb_constructors.constructor
  end
end = R
include R

let gmapT f {D: DATA} = D.gmapT f
let gmapQ f {D: DATA} = D.gmapQ f
let gfoldl f u {D: DATA} = D.gfoldl f u
let constructor {D: DATA} = D.constructor

let app (type a) (type b) (module A : TYPEABLE with type t = a)
    (b : b type_rep) (g : b -> b) (x : a) : a =
    match A.eqty b with
  | Some Refl -> g x
  | _         -> x

let app' (type a) (type b) (type u) (module A : TYPEABLE with type t = a)
    (b : b type_rep) (u:  u) (g : b -> u) (x: a) : u =
    match A.eqty b with
  | Some Refl -> g x
  | _         -> u

let mkT : {T:TYPEABLE} -> (T.t -> T.t) -> genericT =
  fun {T:TYPEABLE} g {D: DATA} ->
    app (module D.Typeable) (T.type_rep ()) g

let mkQ : 'u. {T:TYPEABLE} -> 'u -> (T.t -> 'u) -> 'u genericQ =
  fun {T:TYPEABLE} u g {D: DATA} x ->
    app' (module D.Typeable) (T.type_rep ()) u g x
