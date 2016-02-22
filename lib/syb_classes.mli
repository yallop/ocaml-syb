open Higher

(* Equality *)
type (_, _) eql = Refl : ('a, 'a) eql

(* Type representations *)
type _ type_rep = ..

(* Our analogue to the typeable class *)
type 't typeable = {
  type_rep : 't type_rep;
  eqty : 's. 's type_rep -> ('t, 's) eql option;
}

(* Equality test *)
val (=~~=) : 'a typeable -> 'b typeable -> ('a, 'b) eql option

type   genericT = { t: 't. 't data -> 't -> 't }
and 'u genericQ = { q: 't. 't data -> 't -> 'u }
and 't data = {
  typeable: 't typeable;
  gmapT : genericT -> 't -> 't;
  gmapQ : 'u. 'u genericQ -> 't -> 'u list;
  constructor: 't -> Syb_constructors.constructor
}

val gmapT : genericT -> genericT
val gmapQ : 'u genericQ -> 'u list genericQ
val constructor : Syb_constructors.constructor genericQ

val mkT : 't typeable -> ('t -> 't) -> genericT
val mkQ : 't typeable -> 'u -> ('t -> 'u) -> 'u genericQ
