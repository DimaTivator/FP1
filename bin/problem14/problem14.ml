open Base

(* 1.1 *)
let rec collatz_recursive (n: int) : int list =
  match n with
  | n when n <= 0 -> failwith "Input must be a positive integer"
  | 1 -> [1]
  | n when n % 2 = 0 -> n :: collatz_recursive (n / 2)
  | _ -> n :: collatz_recursive (3 * n + 1)


(* 1.2 *)
let rec collatz_recursive_tail (n: int) (sequence: int list) : int list =
  match n with 
  | n when n <= 0 -> failwith "Input must be a positive integer"
  | 1 -> List.rev (1 :: sequence)
  | n when n % 2 = 0 -> collatz_recursive_tail (n / 2) (n :: sequence)
  | _ -> collatz_recursive_tail (3 * n + 1) (n :: sequence)


(* length of collatz sequence using lists *)
let collatz_length (n: int) : int = 
  let collatz_sequence = collatz_recursive_tail n [] in
  (* tail recursion list length  *)
  let rec list_length (lst: int list) (acc: int) : int = 
    match lst with
    | [] -> acc
    | _ :: tail -> list_length (tail) (acc + 1)
  in 
  list_length collatz_sequence 0


let solve (n: int) : int = 
  let lst = List_lib.generate_list_tail_rec n [] in 
  let collatz_lengths = List.map ~f:collatz_length lst in
  (List_lib.argmax collatz_lengths) + 1


(* ############################################################################## *)


(* 5 *)
let collatz_infinite_seq (n: int) : int Stdlib.Seq.t =
  let rec next_collatz x () =
    match x with 
    | 1 -> Stdlib.Seq.Cons (1, Stdlib.Seq.empty)
    | x when x % 2 = 0 -> Stdlib.Seq.Cons (x, next_collatz (x / 2))
    | _ -> Stdlib.Seq.Cons (x, next_collatz (3 * x + 1))
  in
  next_collatz n

let collatz_length_sequence (n: int) : int = 
  let collatz_seq = collatz_infinite_seq n in
  Stdlib.Seq.fold_left (fun acc _ -> acc + 1) 0 collatz_seq

let generate_collatz_lengths (n: int) : int list = 
  List.init n ~f:(fun i -> collatz_length (i + 1))


(* sequence implementation *)
let solve_seq (n: int) : int = 
  let lengths = generate_collatz_lengths n in 
  (List_lib.argmax lengths) + 1
  

(* ############################################################################## *)


(* loop implementation *)
let solve_loop (n: int) = 
  let current_max = ref 0 in 
  let current_index = ref 0 in 
  for i = 1 to n do
    let length = collatz_length i in
    if length > !current_max then begin
      current_max := length;
      current_index := i;
    end;
  done;
  !current_index
