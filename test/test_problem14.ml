open OUnit2
open Base
open Problem14

let test_collatz_recursive _ =
  (* Test known sequences *)
  assert_equal [1] (collatz_recursive 1);
  assert_equal [2; 1] (collatz_recursive 2);
  assert_equal [3; 10; 5; 16; 8; 4; 2; 1] (collatz_recursive 3);
  assert_equal [4; 2; 1] (collatz_recursive 4);
  assert_equal [5; 16; 8; 4; 2; 1] (collatz_recursive 5);
  assert_equal [6; 3; 10; 5; 16; 8; 4; 2; 1] (collatz_recursive 6);
  assert_equal [7; 22; 11; 34; 17; 52; 26; 13; 40; 20; 10; 5; 16; 8; 4; 2; 1] (collatz_recursive 7);
  
  (* Test exception for invalid input *)
  let exception_test () = 
    ignore (collatz_recursive 0);
    ignore (collatz_recursive (-1))
  in
  assert_raises (Failure "Input must be a positive integer") exception_test


let test_collatz_recursive_tail _ =
  (* Test known sequences with the tail-recursive version *)
  assert_equal [1] (collatz_recursive_tail 1 []);
  assert_equal [2; 1] (collatz_recursive_tail 2 []);
  assert_equal [3; 10; 5; 16; 8; 4; 2; 1] (collatz_recursive_tail 3 []);
  assert_equal [4; 2; 1] (collatz_recursive_tail 4 []);
  assert_equal [5; 16; 8; 4; 2; 1] (collatz_recursive_tail 5 []);
  assert_equal [6; 3; 10; 5; 16; 8; 4; 2; 1] (collatz_recursive_tail 6 []);
  assert_equal [7; 22; 11; 34; 17; 52; 26; 13; 40; 20; 10; 5; 16; 8; 4; 2; 1] (collatz_recursive_tail 7 []);
  
  (* Test exception for invalid input *)
  let exception_test () = 
    ignore (collatz_recursive_tail 0 []);
    ignore (collatz_recursive_tail (-1) [])
  in
  assert_raises (Failure "Input must be a positive integer") exception_test  


let test_collatz_length _ =
  (* Test the length of known sequences *)
  assert_equal 1 (collatz_length 1);
  assert_equal 2 (collatz_length 2);
  assert_equal 8 (collatz_length 3);
  assert_equal 3 (collatz_length 4);
  assert_equal 6 (collatz_length 5);
  assert_equal 9 (collatz_length 6);
  assert_equal 17 (collatz_length 7); 
  
  (* Test exception for invalid input *)
  let exception_test () = 
    ignore (collatz_length 0);
    ignore (collatz_length (-1))
  in
  assert_raises (Failure "Input must be a positive integer") exception_test


(* helper function *)
let seq_to_list seq =
  let rec aux acc seq =
    match seq () with
    | Stdlib.Seq.Nil -> List.rev acc
    | Stdlib.Seq.Cons (x, xs) -> aux (x :: acc) xs
  in
  aux [] seq


let test_collatz_infinite_seq _ =
  (* Test known infinite sequences *)
  let seq1 = collatz_infinite_seq 1 in
  assert_equal [1] (seq_to_list seq1);
  
  let seq2 = collatz_infinite_seq 2 in
  assert_equal [2; 1] (seq_to_list seq2);
  
  let seq3 = collatz_infinite_seq 3 in
  assert_equal [3; 10; 5; 16; 8; 4; 2; 1] (seq_to_list seq3);
  
  let seq4 = collatz_infinite_seq 4 in
  assert_equal [4; 2; 1] (seq_to_list seq4);
  
  let seq5 = collatz_infinite_seq 5 in
  assert_equal [5; 16; 8; 4; 2; 1] (seq_to_list seq5);

  let seq6 = collatz_infinite_seq 6 in
  assert_equal [6; 3; 10; 5; 16; 8; 4; 2; 1] (seq_to_list seq6);

  let seq7 = collatz_infinite_seq 7 in
  assert_equal [7; 22; 11; 34; 17; 52; 26; 13; 40; 20; 10; 5; 16; 8; 4; 2; 1] (seq_to_list seq7)
  

let test_collatz_length_sequence _ =
  (* Test lengths of known sequences *)
  assert_equal 1 (collatz_length_sequence 1);
  assert_equal 2 (collatz_length_sequence 2);
  assert_equal 8 (collatz_length_sequence 3);
  assert_equal 3 (collatz_length_sequence 4);
  assert_equal 6 (collatz_length_sequence 5);
  assert_equal 9 (collatz_length_sequence 6);
  assert_equal 17 (collatz_length_sequence 7)


let test_generate_collatz_lengths _ =
  (* Test generating lengths of Collatz sequences *)
  assert_equal [1] (generate_collatz_lengths 1);
  assert_equal [1; 2] (generate_collatz_lengths 2);
  assert_equal [1; 2; 8] (generate_collatz_lengths 3);
  assert_equal [1; 2; 8; 3] (generate_collatz_lengths 4);
  assert_equal [1; 2; 8; 3; 6] (generate_collatz_lengths 5);
  assert_equal [1; 2; 8; 3; 6; 9] (generate_collatz_lengths 6);
  assert_equal [1; 2; 8; 3; 6; 9; 17] (generate_collatz_lengths 7)


let suite =
  "TestMyModule" >:::
  [
    "test_collatz_recursive" >:: test_collatz_recursive;
    "test_collatz_recursive_tail" >:: test_collatz_recursive_tail;
    "test_collatz_length" >:: test_collatz_length;
    "test_collatz_infinite_seq" >:: test_collatz_infinite_seq;
    "test_collatz_length_sequence" >:: test_collatz_length_sequence;
    "test_generate_collatz_lengths" >:: test_generate_collatz_lengths;
  ]


