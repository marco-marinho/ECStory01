open! Base
open! Stdio
open Angstrom

type node_info =
  { id : int
  ; left_val : int
  ; left_tag : string
  ; right_val : int
  ; right_tag : string
  }
[@@deriving show]

type node =
  | Empty
  | Leaf of
      { id : int
      ; value : int
      ; tag : string
      ; left : node
      ; right : node
      ; last_swap : int
      }
[@@deriving show]

type command =
  | Add of node_info
  | Swap of int
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

let swap_parser =
  string "SWAP"
  *> skip_while (fun c -> Char.equal c ' ')
  *>
  let a =
    take_while1 (function
      | '0' .. '9' | '-' -> true
      | _ -> false)
    >>| Int.of_string
    <* skip_while (fun c -> Char.equal c ' ')
  in
  a
;;

let extract_node_info command =
  match command with
  | Add info -> info
  | Swap _ -> failwith "Not an Add command"
;;

let rec insert_node tree tree_node =
  match tree_node with
  | Empty -> tree
  | Leaf inode ->
    (match tree with
     | Empty -> tree_node
     | Leaf l ->
       if inode.value < l.value
       then Leaf { l with left = insert_node l.left tree_node }
       else Leaf { l with right = insert_node l.right tree_node })
;;

let rec replace_node_value tree node_id new_value =
  match tree with
  | Empty -> Empty
  | Leaf current ->
    let nval, ntag = new_value in
    if current.id = node_id
    then Leaf { current with value = nval; tag = ntag }
    else
      Leaf
        { current with
          left = replace_node_value current.left node_id new_value
        ; right = replace_node_value current.right node_id new_value
        }
;;

let rec replace_node node_replacement tree =
  match tree with
  | Empty -> Empty
  | Leaf current ->
    let tid, ttag, tlast_swap, tnode = node_replacement in
    if
      current.id = tid
      && (not (String.equal current.tag ttag))
      && not (current.last_swap = tlast_swap)
    then tnode
    else
      Leaf
        { current with
          left = replace_node node_replacement current.left
        ; right = replace_node node_replacement current.right
        }
;;

let rec find_node tree node_id =
  match tree with
  | Empty -> []
  | Leaf { id; value = _; tag = _; left; right; last_swap = _ } ->
    if id = node_id
    then [ tree ] @ find_node left node_id @ find_node right node_id
    else find_node left node_id @ find_node right node_id
;;

let parse_line line =
  match String.is_substring_at line ~substring:"SWAP" ~pos:0 with
  | true ->
    let swap_id =
      match parse_string ~consume:All swap_parser line with
      | Ok result -> result
      | Error msg -> failwith msg
    in
    Swap swap_id
  | false ->
    (match parse_string ~consume:All line_parser line with
     | Ok result -> Add result
     | Error msg -> failwith msg)
;;

let node_info_to_node { id; left_val; left_tag; right_val; right_tag } =
  let left_node =
    Leaf
      { id; value = left_val; tag = left_tag; left = Empty; right = Empty; last_swap = 0 }
  in
  let right_node =
    Leaf
      { id
      ; value = right_val
      ; tag = right_tag
      ; left = Empty
      ; right = Empty
      ; last_swap = 0
      }
  in
  left_node, right_node
;;

let get_node_tag node =
  match node with
  | Empty -> ""
  | Leaf l -> l.tag
;;

let get_node_children node =
  match node with
  | Empty -> Empty, Empty
  | Leaf l -> l.left, l.right
;;

let get_tree_levels tree =
  let rec aux acc current_level =
    match current_level with
    | [] -> List.rev acc
    | _ ->
      let next_level =
        List.fold current_level ~init:[] ~f:(fun acc' node ->
          match get_node_children node with
          | Empty, Empty -> acc'
          | Empty, right -> right :: acc'
          | left, Empty -> left :: acc'
          | left, right -> right :: left :: acc')
      in
      aux (current_level :: acc) (List.rev next_level)
  in
  aux [] [ tree ]
;;

let biggest_level_string levels =
  List.max_elt levels ~compare:(fun a b -> Int.compare (List.length a) (List.length b))
  |> Option.value_exn
  |> List.map ~f:(fun node ->
    match node with
    | Empty -> ""
    | Leaf l -> l.tag)
  |> String.concat ~sep:""
;;

let node_to_replace_args node i =
  match node with
  | Leaf n -> n.id, n.tag, i, Leaf { n with last_swap = i }
  | _ -> failwith "Invalid node for replacement"
;;

let node_list_to_replace_args node_list i =
  match node_list with
  | [ an; bn ] -> node_to_replace_args an i, node_to_replace_args bn i
  | _ -> failwith "Invalid node list for replacement"
;;

let part1 () =
  let input = Util.read_input_lines 2 1 in
  let nodes = List.map input ~f:parse_line in
  let roots =
    List.hd nodes |> Option.value_exn |> extract_node_info |> node_info_to_node
  in
  let left_tree, right_tree =
    List.fold (List.drop nodes 1) ~init:roots ~f:(fun (l_tree, r_tree) command ->
      match command with
      | Swap _ -> l_tree, r_tree
      | Add node_info ->
        let l_node, r_node = node_info_to_node node_info in
        insert_node l_tree l_node, insert_node r_tree r_node)
  in
  let left_levels = get_tree_levels left_tree in
  let right_levels = get_tree_levels right_tree in
  let left_string = biggest_level_string left_levels in
  let right_string = biggest_level_string right_levels in
  left_string ^ right_string
;;

let part2 () =
  let input = Util.read_input_lines 2 2 in
  let nodes = List.map input ~f:parse_line in
  let left_values = Array.create ~len:101 (0, "") in
  let right_values = Array.create ~len:101 (0, "") in
  let roots =
    List.hd nodes |> Option.value_exn |> extract_node_info |> node_info_to_node
  in
  let left_tree, right_tree =
    List.fold (List.drop nodes 1) ~init:roots ~f:(fun (l_tree, r_tree) command ->
      match command with
      | Swap tid ->
        let l_value = left_values.(tid) in
        let r_value = right_values.(tid) in
        let new_left_tree = replace_node_value l_tree tid r_value in
        let new_right_tree = replace_node_value r_tree tid l_value in
        let () = left_values.(tid) <- r_value in
        let () = right_values.(tid) <- l_value in
        new_left_tree, new_right_tree
      | Add node_info ->
        let () = left_values.(node_info.id) <- node_info.left_val, node_info.left_tag in
        let () =
          right_values.(node_info.id) <- node_info.right_val, node_info.right_tag
        in
        let l_node, r_node = node_info_to_node node_info in
        insert_node l_tree l_node, insert_node r_tree r_node)
  in
  let left_levels = get_tree_levels left_tree in
  let right_levels = get_tree_levels right_tree in
  let left_string = biggest_level_string left_levels in
  let right_string = biggest_level_string right_levels in
  left_string ^ right_string
;;

let part3 () =
  let input = Util.read_input_lines 2 3 in
  let nodes = List.map input ~f:parse_line in
  let roots =
    List.hd nodes |> Option.value_exn |> extract_node_info |> node_info_to_node
  in
  let left_tree, right_tree =
    List.foldi (List.drop nodes 1) ~init:roots ~f:(fun idx (l_tree, r_tree) command ->
      match command with
      | Swap tid ->
        let to_swap = find_node l_tree tid @ find_node r_tree tid in
        let a_replace, b_replace = node_list_to_replace_args to_swap idx in
        let new_left_tree = replace_node a_replace l_tree |> replace_node b_replace in
        let new_right_tree = replace_node a_replace r_tree |> replace_node b_replace in
        new_left_tree, new_right_tree
      | Add node_info ->
        let l_node, r_node = node_info_to_node node_info in
        insert_node l_tree l_node, insert_node r_tree r_node)
  in
  let left_levels = get_tree_levels left_tree in
  let right_levels = get_tree_levels right_tree in
  let left_string = biggest_level_string left_levels in
  let right_string = biggest_level_string right_levels in
  left_string ^ right_string
;;

let solve part =
  match part with
  | 1 -> part1 ()
  | 2 -> part2 ()
  | 3 -> part3 ()
  | _ -> failwith "Invalid part number"
;;
