(* SYB-style equality, using extensible variants to avoid the unsafe cast. *)

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

let (=~~=) a b = a.eqty b.type_rep

(* Implicit instances *)
type   genericT = { t: 't. 't data -> 't -> 't }
and 'u genericQ = { q: 't. 't data -> 't -> 'u }
and 't data = {
  typeable: 't typeable;
  gmapT : genericT -> 't -> 't;
  gmapQ : 'u. 'u genericQ -> 't -> 'u list;
  constructor: 't -> Syb_constructors.constructor
}

let gmapT f = { t = fun {gmapT} -> gmapT f }
let gmapQ f = { q = fun {gmapQ} -> gmapQ f }
let constructor = { q = fun {constructor} -> constructor }

let mkT : type t. t typeable -> (t -> t) -> genericT =
  fun t g ->
    { t = fun (type t') (d : t' data) (x : t') ->
          match d.typeable.eqty t.type_rep with
          | Some Refl -> (g x : t')
          | _         -> x }

let mkQ : type t u. t typeable -> u -> (t -> u) -> u genericQ =
  fun t u g ->
    { q = fun (type t') (d : t' data) (x : t') -> 
          match d.typeable.eqty t.type_rep with
          | Some Refl -> (g x : u)
          | _         -> u }
