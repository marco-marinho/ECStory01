open! Base
open! Stdio
open Angstrom

type node =
  { id : int
  ; left_val : int
  ; left_tag : string
  ; right_val : int
  ; right_tag : string
  }
[@@deriving show]

let line_parser =
  string "ADD"
  *> skip_while (fun c -> Char.equal c ' ')
  *> string "id="
  *> let+ id =
       take_while1 (function
         | '0' .. '9' | '-' -> true
         | _ -> false)
       >>| Int.of_string
     and+ left_val =
       skip_while (fun c -> Char.equal c ' ')
       *> string "left=["
       *> take_while1 (function
         | '0' .. '9' | '-' -> true
         | _ -> false)
       >>| Int.of_string
     and+ left_tag = char ',' *> take_while (fun c -> Char.( <> ) c ']') <* char ']'
     and+ right_val =
       skip_while (fun c -> Char.equal c ' ')
       *> string "right=["
       *> take_while1 (function
         | '0' .. '9' | '-' -> true
         | _ -> false)
       >>| Int.of_string
     and+ right_tag =
       char ',' *> take_while (fun c -> Char.( <> ) c ']')
       <* char ']'
       <* skip_while (fun c -> Char.equal c ' ')
     in
     { id; left_val; left_tag; right_val; right_tag }
;;

let parse_line line =
  match parse_string ~consume:All line_parser line with
  | Ok result -> result
  | Error msg -> failwith msg
;;

let part1 () =
  let input = Util.read_input_lines 2 1 in
  let nodes = List.map input ~f:parse_line in
  let () = List.iter nodes ~f:(fun node -> Stdio.print_endline (show_node node)) in
  ""
;;

let part2 () = ""
let part3 () = ""

let solve part =
  match part with
  | 1 -> part1 ()
  | 2 -> part2 ()
  | 3 -> part3 ()
  | _ -> failwith "Invalid part number"
;;
