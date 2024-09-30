open OUnit2
open Problem17

let test_to_words _ =
  assert_equal "one" (to_words 1);
  assert_equal "two" (to_words 2);
  assert_equal "ten" (to_words 10);
  assert_equal "eleven" (to_words 11);
  assert_equal "twenty" (to_words 20);
  assert_equal "twenty-one" (to_words 21);
  assert_equal "forty-two" (to_words 42);
  assert_equal "ninety-nine" (to_words 99);
  assert_equal "one hundred" (to_words 100);
  assert_equal "one hundred and twenty-three" (to_words 123);
  assert_equal "one thousand" (to_words 1000);
  
  (* Testing edge cases *)
  assert_equal "" (to_words 0);
  assert_equal "one hundred" (to_words 100);
  assert_equal "nine hundred and ninety-nine" (to_words 999)

let test_count_letters _ =
  assert_equal 3 (count_letters "one");
  assert_equal 5 (count_letters "three");
  assert_equal 6 (count_letters "eleven");
  assert_equal 8 (count_letters "forty-two");
  assert_equal 10 (count_letters "one hundred")

let test_solve _ =
  assert_equal 21124 solve

let test_solve_loop _ =
  assert_equal 21124 solve_loop

let suite =
  "NumberToWordsTests" >:::
  [
    "test_to_words" >:: test_to_words;
    "test_count_letters" >:: test_count_letters;
    "test_solve" >:: test_solve;
    "test_solve_loop" >:: test_solve_loop;
  ]