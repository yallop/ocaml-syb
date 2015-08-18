open Syb_constructors
open Syb_classes

(* Some primitive typeable instances *)
type _ type_rep += List : 'a type_rep -> 'a list type_rep
type _ type_rep += Option : 'a type_rep -> 'a option type_rep
type _ type_rep += Pair : 'a type_rep * 'b type_rep -> ('a * 'b) type_rep

module Typeable0_make(T: sig type t end) =
struct
  type _ type_rep += T : T.t type_rep
  type t = T.t
  let eqty : type b. b type_rep -> (t, b) eql option =
    function T -> Some Refl | _ -> None
  let type_rep () = T
end

implicit module Typeable_int = Typeable0_make(struct type t = int end)
implicit module Typeable_bool = Typeable0_make(struct type t = bool end)
implicit module Typeable_float = Typeable0_make(struct type t = float end)
implicit module Typeable_string = Typeable0_make(struct type t = string end)

implicit module Typeable_pair{A: TYPEABLE} {B: TYPEABLE} =
struct
  type t = A.t * B.t
  let eqty : type c. c type_rep -> (A.t * B.t, c) eql option = function
      Pair (a, b) ->
      begin match A.eqty a, B.eqty b with
          Some Refl, Some Refl -> Some Refl
        | _ -> None
      end
    | _ -> None

  let type_rep () = Pair (A.type_rep (), B.type_rep ())
end

implicit module Typeable_list{A: TYPEABLE} =
struct
  type t = A.t list
  let eqty : type b. b type_rep -> (A.t list, b) eql option = function
    | List a ->
      begin match A.eqty a with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  let type_rep () = List (A.type_rep ())
end

implicit module Typeable_option{A: TYPEABLE} =
struct
  type t = A.t option
  let eqty : type b. b type_rep -> (A.t option, b) eql option = function
    | Option a ->
      begin match A.eqty a with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  let type_rep () = Option (A.type_rep ())
end

module Primitive(A:
                 sig
                   type t
                   module Typeable : TYPEABLE with type t = t
                   val constructor : t -> constructor
                 end) : DATA with type t = A.t =
struct
  include A
  let gmapT _ x = x
  let gmapQ _ _ = []
end

implicit module Data_int =
           Primitive (struct
             type t = int
             module Typeable = Typeable_int
             let constructor c = Syb_constructors.constructor (string_of_int c)
           end)

implicit module Data_bool =
           Primitive (struct
             type t = bool
             module Typeable = Typeable_bool
             let constructor b = Syb_constructors.constructor (string_of_bool b)
           end)

implicit module Data_float =
           Primitive (struct
             type t = float
             module Typeable = Typeable_float
             let constructor f = Syb_constructors.constructor (string_of_float f)
           end)

implicit module Data_string =
           Primitive (struct
             type t = string
             module Typeable = Typeable_string
             let constructor s = Syb_constructors.constructor (Printf.sprintf "%S" s)
           end)

implicit module Data_list {A: DATA} : DATA with type t = A.t list =
struct
  module rec R : DATA with type t = A.t list =
  struct
    type t = A.t list 
    module Typeable = Typeable_list(A.Typeable)
    let gmapT (f : genericT) (l : t) =
      match l with
        [] -> []
      | x :: xs -> f x :: f {R} xs

    let gmapQ (q : _ genericQ) (l : t) =
      match l with
        [] -> []
      | x :: xs -> [q x; q {R} xs]

    let constructor = function
        [] -> Syb_constructors.constructor "[]"
      | _::_ -> Syb_constructors.constructor "::"
  end
  include R
end

implicit module Data_pair {A: DATA} {B: DATA} : DATA with type t = A.t * B.t =
struct
  type t = A.t * B.t
  module Typeable = Typeable_pair(A.Typeable)(B.Typeable)
  let gmapT (f : genericT) ((x, y) : t) = (f x, f y)
  let gmapQ (q : _ genericQ) ((x, y) : t) = [q x; q y]
  let constructor _ = "(,)"
end

implicit module Data_option {A: DATA} : DATA with type t = A.t option =
struct
  type t = A.t option
  module Typeable = Typeable_option(A.Typeable)
  let gmapT (f : genericT) (o : t) =
    match o with None -> None | Some x -> Some (f x)
  let gmapQ (q : _ genericQ) (o : t) =
    match o with None -> [] | Some x -> [q x]
  let constructor = function
      None -> "None"
    | Some _ -> "Some"
end

implicit module Typeable_of_data{F: DATA} = F.Typeable
