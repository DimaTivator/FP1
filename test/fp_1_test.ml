open OUnit2


let () =
  let suite = 
    "MainTestSuite" >:::
    [
      Test_problem14.suite;  
      Test_problem17.suite;  
      List_lib_test.suite;
    ]
  in
  run_test_tt_main suite
