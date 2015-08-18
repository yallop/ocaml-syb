open Syb_classes

(* Data instances for built-in types *)
implicit module Data_int : DATA with type t = int
implicit module Data_bool : DATA with type t = bool
implicit module Data_float : DATA with type t = float
implicit module Data_string : DATA with type t = string
implicit module Data_list {A: DATA} : DATA with type t = A.t list
implicit module Data_pair {A: DATA} {B: DATA} : DATA with type t = A.t * B.t
implicit module Data_option {A: DATA} : DATA with type t = A.t option

implicit module Typeable_of_data {A: DATA} : TYPEABLE with type t = A.t
