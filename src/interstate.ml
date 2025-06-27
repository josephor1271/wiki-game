open! Core

module City = struct
  type t = string [@@deriving compare, sexp, hash, equal]
end

module Interstate_Name = struct
  include String

  let default = ""
end

module Interstate_network = struct
  module Interstate = struct
    module T = struct
      type t = Interstate_Name.t * (City.t * City.t)
      [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)

    let is_reflexive_not_bool (city_1, city_2) : bool =
      String.equal city_1 city_2 |> not
    ;;

    let replace_dots_and_spaces cities : string list =
      List.map cities ~f:(fun city ->
        String.substr_replace_all city ~pattern:"." ~with_:""
        |> String.substr_replace_all ~pattern:" " ~with_:"_")
    ;;

    let get_edges_from_city_list city_list (name : string)
      : (Interstate_Name.t * (City.t * City.t)) list
      =
      List.cartesian_product city_list city_list
      |> List.filter ~f:is_reflexive_not_bool
      |> List.map ~f:(fun road -> name, road)
    ;;

    (*takes in a line from txt file and outputs list of (name, (city, city))*)
    let of_string s =
      match String.split s ~on:',' |> replace_dots_and_spaces with
      | interstate_name :: city_list ->
        get_edges_from_city_list city_list interstate_name
      | [] -> raise_s [%message "Got empty line!"]
    ;;
  end

  type t = Interstate.Set.t [@@deriving sexp_of]

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:Interstate.of_string
    in
    Interstate.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Interstate_network.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values (whose types
           have [sexp_of_t] implemented). *)
        printf !"%{sexp: Interstate_network.t}\n" network]
;;

module G = Graph.Imperative.Graph.ConcreteLabeled (City) (Interstate_Name)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes edge = [ `Dir `None; `Label (E.label edge) ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let interstate_network = Interstate_network.of_file input_file in
        let graph = G.create () in
        Set.iter interstate_network ~f:(fun (name, (city_1, city_2)) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          G.add_edge_e graph (city_1, name, city_2));
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
