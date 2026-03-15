(* SYB-style equality, using extensible variants to avoid the unsafe cast. *)

open Higher

(* Type representations *)
type _ type_rep = ..

(* Our analogue to the typeable class *)
module type TYPEABLE =
sig
  type t
  val type_rep : unit -> t type_rep
  val eqty : 's type_rep -> (t, 's) Type.eq option
end

let (=~~=) (module A: TYPEABLE) (module B: TYPEABLE) = A.eqty (B.type_rep ())

(* Implicit instances *)
module rec R :
sig
  type   genericT = (module D:R.DATA) -> D.t -> D.t
  and 'u genericQ = (module D:R.DATA) -> D.t -> 'u
  and 'c genericFapp  =
    < g: 'b. (module T: R.DATA) -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app >
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

let gmapT f (module D: DATA) = D.gmapT f
let gmapQ f (module D: DATA) = D.gmapQ f
let gfoldl f u (module D: DATA) = D.gfoldl f u
let constructor (module D: DATA) = D.constructor

let app (type b) (module A : TYPEABLE) (b : b type_rep) (g : b -> b) (x : A.t) : A.t =
  match A.eqty b with
  | Some Equal -> g x
  | _          -> x

let app' (type b u) (module A : TYPEABLE) (b : b type_rep) (u : u) (g : b -> u) (x: A.t) : u =
  match A.eqty b with
  | Some Equal -> g x
  | _          -> u

let mkT : (module T:TYPEABLE) -> (T.t -> T.t) -> genericT =
  fun (module T) g (module D) ->
    app (module D.Typeable) (T.type_rep ()) g

let mkQ : 'u. (module T:TYPEABLE) -> 'u -> (T.t -> 'u) -> 'u genericQ =
  fun (module T) u g (module D) x ->
    app' (module D.Typeable) (T.type_rep ()) u g x
