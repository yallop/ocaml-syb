open Syb_constructors
open Syb_classes

(* Some primitive typeable instances *)
type _ type_rep += List : 'a type_rep -> 'a list type_rep
type _ type_rep += Option : 'a type_rep -> 'a option type_rep
type _ type_rep += Pair : 'a type_rep * 'b type_rep -> ('a * 'b) type_rep

let typeable0_make : type a. unit -> a typeable =
  fun () ->
    let module M = struct
      type _ type_rep += T : a type_rep
      type t = a
      let eqty : type b. b type_rep -> (t, b) eql option =
        function T -> Some Refl | _ -> None
      let type_rep = T
    end in M.{ type_rep; eqty }

let typeable_int = typeable0_make ()
let typeable_bool = typeable0_make ()
let typeable_float = typeable0_make ()
let typeable_string = typeable0_make ()

let typeable_pair (type a) (type b) (a : a typeable) (b : b typeable) =
  let eqty : type c. c type_rep -> (a * b, c) eql option = function
      Pair (a', b') ->
      begin match a.eqty a', b.eqty b' with
          Some Refl, Some Refl -> Some Refl
        | _ -> None
      end
    | _ -> None

  and type_rep = Pair (a.type_rep, b.type_rep) in
  { eqty; type_rep }

let typeable_list (type a) (a : a typeable) =
  let eqty : type b. b type_rep -> (a list, b) eql option = function
    | List a' ->
      begin match a.eqty a' with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  and type_rep = List a.type_rep in
  { eqty; type_rep }

let typeable_option (type a) (a : a typeable) =
  let eqty : type b. b type_rep -> (a option, b) eql option = function
    | Option a' ->
      begin match a.eqty a' with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  and type_rep = Option a.type_rep in
  { eqty; type_rep }

let primitive typeable constructor =
  let gmapT _ x = x
  and gmapQ _ _ = [] in
  { typeable; constructor; gmapT; gmapQ }


let int = primitive typeable_int
    (fun c -> Syb_constructors.constructor (string_of_int c))

let bool = primitive typeable_bool
    (fun c -> Syb_constructors.constructor (string_of_bool c))

let float = primitive typeable_float
    (fun c -> Syb_constructors.constructor (string_of_float c))

let string = primitive typeable_string
    (fun c -> Syb_constructors.constructor (Printf.sprintf "%S" c))

let list a =
  let rec r =
    let typeable = typeable_list a.typeable
    and gmapT {t=f} l =
      match l with
        [] -> []
      | x :: xs -> f a x :: f r xs
                     
    and gmapQ {q} l =
      match l with
        [] -> []
      | x :: xs -> [q a x; q r xs]
                   
    and constructor = function
        [] -> Syb_constructors.constructor "[]"
      | _::_ -> Syb_constructors.constructor "::"
    in
    { gmapT; gmapQ; typeable; constructor }
  in r

let pair a b =
  let typeable = typeable_pair a.typeable b.typeable
  and gmapT {t=f} (x, y) = (f a x, f b y)
  and gmapQ {q} (x, y) = [q a x; q b y]
  and constructor _ = "(,)" in
    { gmapT; gmapQ; typeable; constructor }

let option a =
  let typeable = typeable_option a.typeable
  and gmapT {t=f} o =
    match o with None -> None | Some x -> Some (f a x)
  and gmapQ {q} o =
    match o with None -> [] | Some x -> [q a x]
  and constructor = function
      None -> "None"
    | Some _ -> "Some" in
  { gmapT; gmapQ; typeable; constructor }

let typeable_of_data f = f.typeable
