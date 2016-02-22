open OUnit2
open Higher
open Syb_classes
open Syb_schemes
open Syb_instances

let test_gshow _ =
  assert_equal ~printer:(fun x -> x)
    "(::((,)(true) (1)) (::((,)(false) (2)) (::((,)(false) (3)) ([]))))"
    (gshow.genericQ (list (pair bool int))
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_gsize _ =
  assert_equal ~printer:string_of_int
    13
    (gsize.genericQ (list (pair bool int))
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_everywhere _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((everywhere (mkT bool.typeable not)).genericT
       (list (pair bool int))
       [(true, 1);
        (false, 2);
        (false, 3)])
  

let test_everywhere' _ =
  assert_equal
    [(false, 1);
     (true, 2);
     (true, 3)]
    ((everywhere' (mkT bool.typeable not)).genericT
       (list (pair bool int))
       [(true, 1);
        (false, 2);
        (false, 3)])

let test_everything _ =
  let ints_gt_0 = mkQ int.typeable [] (fun x -> if x > 0 then [x] else []) in
  assert_equal
    [1; 2; 3; 20]
    ((everything (@) ints_gt_0).genericQ
       (list (pair bool int))
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
    ((everywhere (mkT int.typeable succ)).genericT
       (list (pair bool int))
       [(false, 1);
        (true,  2);
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
  ]


let _ =
  run_test_tt_main suite
