open Base


(* Function to convert a number to its English words representation *)
let to_words (n: int) : string =
  let ones = [|
    ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine";
    "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"
  |] in
  let tens = [|
    ""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety"
  |] in
  let hundred = "hundred" in

  let rec convert n word = 
    match n with 
    | 1000 -> "one thousand"
    | _ when n >= 100 -> 
      let hundreds_part = ones.(n / 100) ^ " " ^ hundred in
      let remainder = n % 100 in
      let conjunction = if remainder <> 0 then " and " else "" in
      convert remainder (word ^ hundreds_part ^ conjunction)
      
    | _ when n < 20 -> word ^ ones.(n)
    | _ ->
        let tens_part = tens.(n / 10) in
        let remainder = n % 10 in
        let ones_part = if remainder <> 0 then "-" ^ ones.(remainder) else "" in
        word ^ tens_part ^ ones_part
  in
  convert n ""


let count_letters str =
  let count = ref 0 in
  String.iter ~f:(fun (c: char) ->
    match c with 
    | ' ' | '-' -> ()
    | _ -> count := !count + 1
  ) str;
  !count


let solve : int = 
  let seq = List_lib.generate_list_tail_rec 1000 [] in 
  let words_list = List.map ~f:to_words seq in
  let length_list = List.map ~f:count_letters words_list in
  List.fold_left ~f:(+) ~init:0 length_list


(* loop implementation  *)
let solve_loop : int = 
  let sum = ref 0 in 
  for i = 1 to 1000 do
    let words = to_words i in
    let length = count_letters words in 
    sum := !sum + length
  done;
  !sum

  
