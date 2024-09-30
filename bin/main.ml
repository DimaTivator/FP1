open Stdio


let () = 
  let problem14_result = Problem14.solve_seq 1_000_000 in 
  let problem17_result = Problem17.solve in 
  printf "Problem 17: %d\nProblem 14: %d\n" problem17_result problem14_result
  