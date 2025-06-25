open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let node_to_string node =
  let open Soup in
  texts node |> String.concat ~sep:"" |> String.strip
;;

let get_credits contents : string list =
  let open Soup in
  let class_name_for_known_for_section = ".sc-c3958617-0" in
  parse contents
  $ class_name_for_known_for_section
  $$ "a"
  |> to_list
  |> List.map ~f:node_to_string
  |> List.filter ~f:(fun a_strings -> String.equal "" a_strings |> not)
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
