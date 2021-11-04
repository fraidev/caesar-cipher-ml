let modulus a b = ((a mod b) + b) mod b

let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t ->
    if x = h then
      0
    else
      1 + find x t

let findAlphabetIndex input = find input Alphabet.alphabet

type typeEncrypt =
  | Sum
  | Subtract

let shift input shiftCount encrypt =
  let operation =
    match encrypt with
    | Sum -> (
      function
      | x -> x + shiftCount)
    | Subtract -> (
      function
      | x -> x - shiftCount)
  in
  String.lowercase_ascii input
  |> String.map (function letter ->
         List.nth Alphabet.alphabet (modulus (operation (findAlphabetIndex letter)) 26))

let encrypt input = shift input 3 Sum

let decrypt input =   shift input 3 Subtract;;

Printf.printf "%s \n" (encrypt "ABC");;

Printf.printf "%s \n" (decrypt "ABC")
