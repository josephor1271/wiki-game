open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)
let remove_non_wiki href_attribute_list : string list =
  List.filter ~f:(String.is_prefix ~prefix:"/wiki/") href_attribute_list
;;

let remove_namespaces_and_duplicates link_list : string list =
  (*remove namespaces*)
  List.map link_list ~f:(fun link ->
    match Wikipedia_namespace.namespace link with None -> link | _ -> "")
  |> List.filter ~f:(fun href_attribute ->
    String.equal href_attribute "" |> not)
  (*removing duplicates*)
  |> List.dedup_and_sort ~compare:String.compare
;;

let get_linked_articles contents : string list =
  let open Soup in
  let link_node_list = parse contents $$ "a[href]" |> to_list in
  let href_attribute_list =
    List.map link_node_list ~f:(fun link_node ->
      R.attribute "href" link_node |> String.strip)
  in
  remove_non_wiki href_attribute_list |> remove_namespaces_and_duplicates
;;

let%expect_test "get_linked_articles" =
  (* This test specifies the HTML content directly in the file. *)
  let contents =
    {|<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>Carnivora - Wikipedia</title>
  </head> 
  <body>
    <p>The <a href="/wiki/Order_(biology)">order</a> Carnivora is a
    group of <a href="/wiki/Mammal">mammals</a>. The group is divided
    into the "cat-like" <a href="/wiki/Feliformia">Feliformia</a> and
    the "dog-like" <a href="/wiki/Caniformia">Caniformia</a>.</p>  

    <p><a href="/wiki/Talk:Order_(biology)">Talk</a></p>
  </body>
</html>
|}
  in
  let linked_articles = get_linked_articles contents in
  print_s [%message (linked_articles : string list)];
  [%expect
    {|
      (linked_articles
       ("/wiki/Order_(biology)" /wiki/Mammal /wiki/Feliformia /wiki/Caniformia)) |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
module Article = struct
  type t = string [@@deriving compare, sexp, hash, equal]
end

module Level = struct
  include Int

  (*let default = 0*)
end

module Article_network = struct
  module Article_connection = struct
    module T = struct
      type t = Article.t * Article.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)
  end

  type t = Article_connection.Set.t [@@deriving sexp_of]
end

let get_contents_from_string_link string_link ~how_to_fetch : string =
  match how_to_fetch with
  | File_fetcher.How_to_fetch.Remote ->
    File_fetcher.fetch_exn Remote ~resource:"https://en.wikipedia.org"
    ^ string_link
  | x -> File_fetcher.fetch_exn x ~resource:string_link
;;

let remove_visited_from_queue visited queue =
  List.filter queue ~f:(fun queue_member ->
    List.mem visited queue_member ~equal:String.equal |> not)
;;

let update_queue ~queue ~source_neighbors ~visited ~max_depth ~current_depth =
  match current_depth < max_depth with
  | true ->
    List.append queue source_neighbors
    |> List.dedup_and_sort ~compare:String.compare
    |> remove_visited_from_queue visited
  | false -> []
;;

let get_neighbors_from_source source ~how_to_fetch : string list =
  get_contents_from_string_link source ~how_to_fetch |> get_linked_articles
;;

let update_visited ~visited ~new_neighbors =
  List.append visited new_neighbors
  |> List.dedup_and_sort ~compare:String.compare
;;

let create_new_connections_from_source source neighbors =
  List.map neighbors ~f:(fun neighbor -> source, neighbor)
;;

let rec get_connections_with_bfs
          ~source
          ~max_depth
          ~how_to_fetch
          ~queue
          ~visited
          ~current_depth
  =
  let source_neighbors = get_neighbors_from_source source ~how_to_fetch in
  let updated_queue =
    update_queue ~queue ~source_neighbors ~visited ~current_depth ~max_depth
  in
  let new_visited =
    update_visited ~visited ~new_neighbors:source_neighbors
  in
  let new_depth = current_depth + 1 in
  let new_connections =
    create_new_connections_from_source source source_neighbors
  in
  match updated_queue with
  | [] -> new_connections
  | new_source :: rest_of_queue ->
    new_connections
    @ get_connections_with_bfs
        ~source:new_source
        ~max_depth
        ~how_to_fetch
        ~queue:rest_of_queue
        ~visited:new_visited
        ~current_depth:new_depth
;;

module G = Graph.Imperative.Graph.Concrete (Article)

module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `Forward ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = Printf.sprintf "\"%s\"" v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  (*let contents = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
    get_linked_articles contents*)
  let list_of_connections =
    get_connections_with_bfs
      ~source:origin
      ~max_depth
      ~how_to_fetch
      ~queue:[]
      ~visited:[]
      ~current_depth:0
  in
  let graph = G.create () in
  List.iter list_of_connections ~f:(fun (article1, article2) ->
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
       they don't already exist. *)
    G.add_edge graph article1 article2);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph;
  ()
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
