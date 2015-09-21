open OUnit2
open Higher
open Syb_classes
open Syb_schemes
open Syb_instances

let test_gshow _ =
  assert_equal ~printer:(fun x -> x)
    "(::((,)(true) (1)) (::((,)(false) (2)) (::((,)(false) (3)) ([]))))"
    (gshow
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_gsize _ =
  assert_equal ~printer:string_of_int
    13
    (gsize
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_everywhere _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((everywhere (mkT not))
       [(true, 1);
        (false, 2);
        (false, 3)])
  

let test_everywhere' _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((everywhere' (mkT not))
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_everything _ =
  let ints_gt_0 = mkQ [] (fun x -> if x > 0 then [x] else []) in
  assert_equal
    [1; 2; 3; 20]
    ((everything (@) ints_gt_0)
    [(false, 1);
     (true, 2);
     (true, 3);
     (true, -10);
     (false, 20);
    ])

let test_instantiate_everywhere_without_function _ =
  assert_equal
    [(false, 2);
     (true,  3);
     (false, 4)]
    (everywhere (mkT succ)
       [(false, 1);
        (true,  2);
        (false, 3)])


let test_gfoldl_gmap _ =
  let module Definitions =
  struct
    module Id = Newtype1(struct type 'a t = 'a end)
    (* gmapT in terms of gfoldl *)
    let gmapT (f : genericT) : genericT =
      let f : _ genericFapp =
        object
          method g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app =
            fun {T: R.DATA} g x -> Id.inj (Id.prj g (f x))
        end
      and u : _ genericFunit =
        object
          method u: 'g. 'g -> ('g, 'c) app = Id.inj
        end in
      fun {D:DATA} (x: D.t) -> Id.prj (D.gfoldl f u x)

    let rec everywhere : genericT -> genericT =
      fun (f : genericT) {X:DATA} x -> f ((gmapT (everywhere f) : genericT) x)
  end
  in
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((Definitions.everywhere (mkT not))
       [(true, 1);
        (false, 2);
        (false, 3)])


let suite = "SYB tests" >:::
  ["gshow"
    >:: test_gshow;

    "gsize"
    >:: test_gsize;

    "everywhere"
    >:: test_everywhere;

    "everywhere'"
    >:: test_everywhere';

    "everything"
    >:: test_everything;

    "everything without function"
    >:: test_instantiate_everywhere_without_function;

    "everywhere using gfoldl"
    >:: test_gfoldl_gmap;
  ]


let _ =
  run_test_tt_main suite
