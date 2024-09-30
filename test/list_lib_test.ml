open OUnit2
open List_lib

let test_generate_list_rec _ =
  assert_equal [] (generate_list_rec 0);
  assert_equal [1] (generate_list_rec 1);
  assert_equal [2; 1] (generate_list_rec 2);
  assert_equal [3; 2; 1] (generate_list_rec 3);
  assert_equal [5; 4; 3; 2; 1] (generate_list_rec 5)
  
let test_generate_list_tail_rec _ =
  assert_equal [] (generate_list_tail_rec 0 []);
  assert_equal [1] (generate_list_tail_rec 1 []);
  assert_equal [1; 2] (generate_list_tail_rec 2 []);
  assert_equal [1; 2; 3;] (generate_list_tail_rec 3 []);
  assert_equal [1; 2; 3; 4; 5] (generate_list_tail_rec 5 [])
  
let test_argmax _ =
  assert_equal 0 (argmax [42]);
  assert_equal 1 (argmax [1; 3; 2]);
  assert_equal 0 (argmax [3; 2; 1]);
  assert_equal 2 (argmax [1; 2; 3]);
  
  (* Testing with duplicate maximum values *)
  assert_equal 0 (argmax [5; 3; 5]);
  assert_equal 1 (argmax [3; 5; 5]);
  
  (* Testing with a list of negative values *)
  assert_equal 0 (argmax [-1; -2; -3; -4; -5]);

  (* Expecting failure on empty list *)
  assert_raises (Failure "Empty list") (fun () -> argmax [])

let suite =
  "ListLibTests" >:::
  [
    "test_generate_list_rec" >:: test_generate_list_rec;
    "test_generate_list_tail_rec" >:: test_generate_list_tail_rec;
    "test_argmax" >:: test_argmax;
  ]
