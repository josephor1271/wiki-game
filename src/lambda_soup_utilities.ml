open! Core

(* Gets the "title" node of an HTML page. *)
let get_title contents : string =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

let%expect_test "get_title" =
  (* This test specifies the HTML content directly in the file. *)
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  print_endline (get_title contents);
  [%expect {| My Blog |}]
;;

(* Gets all of the list items contained in an HTML page. *)
let get_list_items contents : string list =
  let open Soup in
  parse contents
  $$ "li"
  |> to_list
  |> List.map ~f:(fun li ->
    texts li |> String.concat ~sep:"" |> String.strip)
;;

let%expect_test "get_list_items" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter (get_list_items contents) ~f:print_endline;
  [%expect
    {|
    All feliforms, such as domestic cats, big cats, hyenas, mongooses, civets
    Almost all caniforms, such as the dogs, wolves, foxes, ferrets, seals and walruses
    All cetaceans, such as dolphins, whales and porpoises
    All bats except fruitbats
    The carnivorous marsupials, such as the Tasmania devil
    All birds of prey, such as hawks, eagles, falcons and owls
    All vultures, both old world and new
    Most waterfowl, such as gulls, penguins, pelicans, storks, and herons
    |}]
;;

let node_to_string node =
  let open Soup in
  texts node |> String.concat ~sep:"" |> String.strip
;;

(* Gets the first item of all unordered lists contained in an HTML page. *)
let get_first_item_of_all_unordered_lists contents : string list =
  let open Soup in
  (*get list of all ul nodes*)
  parse contents
  $$ "ul"
  |> to_list
  (* Mapping list of uls to only the first element of each ul as text*)
  (*First take a list of all li elements within ech ul*)
  |> List.map ~f:(fun ul ->
    select "li" ul
    |> to_list
    (*Take first node of every list of li and get text*)
    |> List.hd_exn
    |> node_to_string)
;;

let%expect_test "get_first_ul_item" =
  (* This test specifies the HTML content directly in the file. *)
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
    <ul>
  <li>Coffee</li>
  <li>Tea</li>
  <li>Milk</li>
</ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  let first_items = get_first_item_of_all_unordered_lists contents in
  print_s [%message (first_items : string list)];
  [%expect {| (first_items (Coffee)) |}]
;;

(* Gets the first item of the second unordered list in an HTML page. *)
let get_first_item_of_second_unordered_list contents : string =
  let open Soup in
  let ul_list = parse contents $$ "ul" |> to_list in
  (*get second ul*)
  List.nth_exn ul_list 1
  (*get first li node from our ul*)
  |> R.select_one "li"
  (*join all text in our li element*)
  |> node_to_string
;;

let%expect_test "get_first_ul_item_of_second_ul" =
  (* This test specifies the HTML content directly in the file. *)
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
    <ul>
  <li>Coffee</li>
  <li>Tea</li>
  <li>Milk</li>
</ul>
<ul>
  <li>Bagel</li>
  <li>Tea</li>
  <li>Milk</li>
</ul>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  let second_ul_first_item =
    get_first_item_of_second_unordered_list contents
  in
  print_s [%message (second_ul_first_item : string)];
  [%expect {| (second_ul_first_item Bagel) |}]
;;

(* Gets all bolded text from an HTML page. *)
let get_bolded_text contents : string list =
  let open Soup in
  (*get list of all bold nodes in html file*)
  parse contents
  $$ "b"
  |> to_list
  (*replace bold nodes with stripped text of each bold node*)
  |> List.map ~f:(fun bold_node -> node_to_string bold_node)
;;

let%expect_test "get_bolded_text" =
  (* This test specifies the HTML content directly in the file. *)
  let contents =
    {|<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta http-equiv="X-UA-Compatible" content="ie=edge">
        <title>My Blog</title>
        <link rel="stylesheet" href="style.css">
    </head>
    <body>
    <ul>
  <li><b>Coffee</b></li>
  <li>Tea</li>
  <li>Milk</li>
</ul>
<ul>
  <li>Bagel</li>
  <li>Tea</li>
  <li>Milk</li>
</ul>
<p> blahblahal sjhdjdjs <b> this should be bold </b>
    </body>
    <script src="index.js"></script>
</html>
|}
  in
  let bolded_text = get_bolded_text contents in
  print_s [%message (bolded_text : string list)];
  [%expect {| (bolded_text (Coffee "this should be bold")) |}]
;;

(* [make_command ~summary ~f] is a helper function that builds a simple HTML parsing
   command. It takes in a [summary] for the command, as well as a function [f] that
   transforms a string (the HTML contents of a page) into a list of strings (the parsed
   results from that HTML page). *)
let make_command ~summary ~f =
  let open Command.Let_syntax in
  Command.basic
    ~summary
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (f contents) ~f:print_endline]
;;

let print_title_command =
  make_command
    ~summary:"print the title from an HTML page"
    ~f:(fun contents -> [ get_title contents ])
;;

let print_list_items_command =
  make_command
    ~summary:"print all list items from an HTML page"
    ~f:get_list_items
;;

let print_first_item_of_all_unordered_lists_command =
  make_command
    ~summary:"print the first item of each unordered list from an HTML page"
    ~f:get_first_item_of_all_unordered_lists
;;

let print_first_item_of_second_unordered_list_command =
  make_command
    ~summary:"print first item of the second unordered list of an HTML page"
    ~f:(fun contents -> [ get_first_item_of_second_unordered_list contents ])
;;

let print_bolded_text_command =
  make_command
    ~summary:"print all bolded text in an HTML page"
    ~f:get_bolded_text
;;

let command =
  Command.group
    ~summary:"lambda soup demo"
    [ "print-title", print_title_command
    ; "print-list-items", print_list_items_command
    ; ( "print-first-item-of-all-unordered-lists"
      , print_first_item_of_all_unordered_lists_command )
    ; ( "print-first-item-of-second-unordered-list"
      , print_first_item_of_second_unordered_list_command )
    ; "print-bolded-text", print_bolded_text_command
    ]
;;
