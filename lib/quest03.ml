open! Base
open! Stdio
open! Angstrom

type position =
  { x : int
  ; y : int
  ; diagonal : int
  }
[@@deriving show]

let line_parser =
  string "x="
  *> let+ x =
       take_while1 (function
         | '0' .. '9' | '-' -> true
         | _ -> false)
       >>| Int.of_string
     and+ y =
       skip_while (fun c -> Char.equal c ' ')
       *> string "y="
       *> (take_while1 (function
             | '0' .. '9' | '-' -> true
             | _ -> false)
           >>| Int.of_string)
       <* skip_while (fun c -> Char.equal c ' ')
     in
     { x; y; diagonal = y + (x - 1) }
;;

let calculate_final_pos initial_pos steps =
  let final_y = Util.wrap_index (initial_pos.y - steps - 1) initial_pos.diagonal + 1 in
  let final_x = initial_pos.diagonal - final_y + 1 in
  { initial_pos with x = final_x; y = final_y }
;;

let calculate_score pos = pos.x + (100 * pos.y)

let rec crt = function
  | [] -> 0
  | [ a ] -> a.y - 1
  | a :: b :: tl ->
    let m1 = Z.of_int a.diagonal in
    let m2 = Z.of_int b.diagonal in
    let r1 = Z.of_int (a.y - 1) in
    let r2 = Z.of_int (b.y - 1) in
    let m_combined = Z.(m1 * m2) in
    let term1 = Z.(r1 * invert m2 m1 * m2) in
    let term2 = Z.(r2 * invert m1 m2 * m1) in
    let remainder = Z.((term1 + term2) mod m_combined) in
    crt ({ x = 0; y = Z.to_int remainder + 1; diagonal = Z.to_int m_combined } :: tl)
;;

let parse_line line =
  match parse_string ~consume:All line_parser line with
  | Ok result -> result
  | Error msg -> failwith msg
;;

let part1 () =
  let input = Util.read_input_lines 3 1 in
  let positions = List.map input ~f:parse_line in
  let final_positions = List.map positions ~f:(fun pos -> calculate_final_pos pos 100) in
  let scores = List.map final_positions ~f:calculate_score in
  let res = List.fold scores ~init:0 ~f:( + ) in
  Int.to_string res
;;

let part2 () =
  let input = Util.read_input_lines 3 2 in
  let positions = List.map input ~f:parse_line in
  let res = crt positions in
  Int.to_string res
;;

let part3 () =
  let input = Util.read_input_lines 3 3 in
  let positions = List.map input ~f:parse_line in
  let res = crt positions in
  Int.to_string res
;;

let solve part =
  match part with
  | 1 -> part1 ()
  | 2 -> part2 ()
  | 3 -> part3 ()
  | _ -> failwith "Invalid part number"
;;
