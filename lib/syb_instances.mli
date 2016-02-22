open Syb_classes

(* Data instances for built-in types *)
val int : int data 
val bool : bool data 
val float : float data 
val string : string data 
val list : 'a data -> 'a list data 
val pair : 'a data -> 'b data -> ('a * 'b) data
val option : 'a data -> 'a option data

val typeable_of_data : 'a data -> 'a typeable
