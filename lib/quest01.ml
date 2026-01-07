open! Base
open! Stdio

type triplet =
  { a : int
  ; b : int
  ; c : int
  ; x : int
  ; y : int
  ; z : int
  ; m : int
  }
[@@deriving show]

let parse_line line =
  Stdlib.Scanf.sscanf line "A=%d B=%d C=%d X=%d Y=%d Z=%d M=%d" (fun a b c x y z m ->
    { a; b; c; x; y; z; m })
;;

let eni n exp imod =
  let rec aux ixp prev_score acc =
    match ixp with
    | 0 -> acc |> Util.digits_to_int
    | x ->
      let new_score = prev_score * n % imod in
      aux (x - 1) new_score (new_score :: acc)
  in
  aux exp 1 []
;;

let calc_enis triplet =
  let enia = eni triplet.a triplet.x triplet.m in
  let enib = eni triplet.b triplet.y triplet.m in
  let enic = eni triplet.c triplet.z triplet.m in
  enia + enib + enic
;;

let eni_cycle n exp imod =
  let seen = Hashtbl.create (module Int) in
  let rec aux i prev_score acc =
    let new_score = prev_score * n % imod in
    match Hashtbl.find seen new_score with
    | Some idx ->
      if exp < List.length acc
      then List.take (List.rev acc) exp |> Array.of_list, -1
      else acc |> List.rev |> Array.of_list, idx
    | None ->
      let () = Hashtbl.set seen ~key:new_score ~data:i in
      aux (i + 1) new_score (new_score :: acc)
  in
  aux 0 1 []
;;

let calc_enis_last_5 triplet =
  let enia = eni_cycle triplet.a triplet.x triplet.m in
  (* let enib = eni_cycle triplet.b triplet.y triplet.m in
  let enic = eni_cycle triplet.c triplet.z triplet.m in *)
  let () = Util.print_array ~f:Int.to_string (fst enia) in
  let () = Stdio.print_endline (Int.to_string (snd enia)) in
  0
;;

let part1 () =
  let input = Util.read_input_lines 1 1 in
  let triplets = List.map ~f:parse_line input in
  let enis = List.map ~f:calc_enis triplets in
  let result = Util.list_max enis in
  result |> Int.to_string
;;

let part2 () =
  let input = Util.read_input_lines 1 2 in
  let triplets = List.map ~f:parse_line input in
  let enis = List.map ~f:calc_enis_last_5 triplets in
  let result = Util.list_max enis in
  result |> Int.to_string
;;

let part3 () = ""

let solve part =
  match part with
  | 1 -> part1 ()
  | 2 -> part2 ()
  | 3 -> part3 ()
  | _ -> failwith "Invalid part number"
;;
