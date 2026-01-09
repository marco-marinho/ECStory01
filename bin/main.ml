open! Base
open! Stdio

let () =
  let args = Sys.get_argv () in
  let quest = Int.of_string args.(1) in
  let part = Int.of_string args.(2) in
  let solution =
    match quest with
    | 1 -> ECStory01.Quest01.solve part
    | 2 -> ECStory01.Quest02.solve part
    | 3 -> ECStory01.Quest03.solve part
    | _ -> failwith "Quest not implemented"
  in
  print_endline solution
;;
