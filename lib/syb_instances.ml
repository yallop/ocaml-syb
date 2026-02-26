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
  let eqty : type b. b type_rep -> (t, b) Type.eq option =
    function T -> Some Equal | _ -> None
  let type_rep () = T
end

module Typeable_int = Typeable0_make(struct type t = int end)
module Typeable_bool = Typeable0_make(struct type t = bool end)
module Typeable_float = Typeable0_make(struct type t = float end)
module Typeable_string = Typeable0_make(struct type t = string end)

module Typeable_pair (A: TYPEABLE) (B: TYPEABLE) =
struct
  type t = A.t * B.t
  let eqty : type c. c type_rep -> (A.t * B.t, c) Type.eq option = function
      Pair (a, b) ->
      begin match A.eqty a, B.eqty b with
          Some Equal, Some Equal -> Some Equal
        | _ -> None
      end
    | _ -> None

  let type_rep () = Pair (A.type_rep (), B.type_rep ())
end

module Typeable_list (A: TYPEABLE) =
struct
  type t = A.t list
  let eqty : type b. b type_rep -> (A.t list, b) Type.eq option = function
    | List a ->
      begin match A.eqty a with
          Some Equal -> Some Equal
        | None -> None
      end
    | _ -> None
  let type_rep () = List (A.type_rep ())
end

module Typeable_option (A: TYPEABLE) =
struct
  type t = A.t option
  let eqty : type b. b type_rep -> (A.t option, b) Type.eq option = function
    | Option a ->
      begin match A.eqty a with
          Some Equal -> Some Equal
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
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
end

module Data_int =
           Primitive (struct
             type t = int
             module Typeable = Typeable_int
             let constructor c = Syb_constructors.constructor (string_of_int c)
           end)

module Data_bool =
           Primitive (struct
             type t = bool
             module Typeable = Typeable_bool
             let constructor b = Syb_constructors.constructor (string_of_bool b)
           end)

module Data_float =
           Primitive (struct
             type t = float
             module Typeable = Typeable_float
             let constructor f = Syb_constructors.constructor (string_of_float f)
           end)

module Data_string =
           Primitive (struct
             type t = string
             module Typeable = Typeable_string
             let constructor s = Syb_constructors.constructor (Printf.sprintf "%S" s)
           end)

module Data_list  (A: DATA) : DATA with type t = A.t list =
struct
  module rec R : DATA with type t = A.t list =
  struct
    type t = A.t list 
    module Typeable = Typeable_list (A.Typeable)
    let gmapT (f : genericT) (l : t) =
      match l with
        [] -> []
      | x :: xs -> f (module A) x :: f (module R) xs

    let gmapQ (q : _ genericQ) (l : t) =
      match l with
        [] -> []
      | x :: xs -> [q (module A) x; q (module R) xs]

    let gfoldl (g : _ genericFapp) (u : _ genericFunit) l =
      match l with
        [] -> u#u l
      | x :: xs -> g#g (module R) (g#g (module A) (u#u (fun x xs -> x :: xs)) x) xs

    let constructor = function
        [] -> Syb_constructors.constructor "[]"
      | _::_ -> Syb_constructors.constructor "::"
  end
  include R
end

module Data_pair  (A: DATA)  (B: DATA) : DATA with type t = A.t * B.t =
struct
  type t = A.t * B.t
  module Typeable = Typeable_pair (A.Typeable) (B.Typeable)
  let gmapT (f : genericT) (x, y) = (f (module A) x, f (module B) y)
  let gmapQ (q : _ genericQ) (x, y) = [q (module A) x; q (module B) y]
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) (x, y) =
    g#g (module B) (g#g (module A) (u#u (fun x y -> (x,y))) x) y
  let constructor _ = "(,)"
end

module Data_option  (A: DATA) : DATA with type t = A.t option =
struct
  type t = A.t option
  module Typeable = Typeable_option (A.Typeable)
  let gmapT (f : genericT) (o : t) =
    match o with None -> None | Some x -> Some (f (module A) x)
  let gmapQ (q : _ genericQ) (o : t) =
    match o with None -> [] | Some x -> [q (module A) x]
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) = function
      None -> u#u None
    | Some x -> g#g (module A) (u#u (fun x -> Some x)) x
  let constructor = function
      None -> "None"
    | Some _ -> "Some"
end

module Typeable_of_data (F: DATA) = F.Typeable
