let rec generate_list_rec (n: int) : int list = 
  match n with
  | 0 -> []
  | _ -> n :: generate_list_rec(n - 1)

let rec generate_list_tail_rec (n: int) (seq: int list) = 
  match n with 
  | 0 -> seq 
  | _ -> generate_list_tail_rec (n - 1) (n :: seq)

let argmax lst =
  let rec aux max_val max_idx current_idx = function
    | [] -> max_idx 
    | x :: xs ->
      match x > max_val with 
      | true -> aux x current_idx (current_idx + 1) xs  (* Update max value and index *)
      | false -> aux max_val max_idx (current_idx + 1) xs  (* Keep current max value and index *)
  in
  match lst with
  | [] -> failwith "Empty list" 
  | x :: xs -> aux x 0 1 xs  
