open! Core

(* module Game_state = struct type t = Game_continues of Island.t | Game_over
   of Player.t end *)

module Level = struct
  module T = struct
    type t =
      | Easy
      | Medium
      | Hard
    [@@deriving enumerate, sexp]

    let to_string t = Sexp.to_string (sexp_of_t t)
  end

  include T

  let arg : t Command.Arg_type.t = Command.Arg_type.enumerated (module T)
end

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
    include G

    let edge_attributes _ = [ `Dir `Both ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

type t =
  { (*player_one : Player.t ; player_two : Player.t ; game_state :
      Game_state.t *)
    difficulty : Level.t
  }

module My_components = Graph.Components.Make (G)

let create_graph ~graph ~nodes ~(distance : float) =
  List.iter nodes ~f:(fun (node_1, x1, y1) ->
    List.iter nodes ~f:(fun (node_2, x2, y2) ->
      if String.equal node_1 node_2
      then ()
      else (
        let pythagoreum =
          Float.sqrt
            (Int.pow (x2 - x1) 2 + Int.pow (y2 - y1) 2 |> Float.of_int)
        in
        if Float.( < ) pythagoreum distance
        then G.add_edge graph node_1 node_2)));
  let islands = My_components.scc_list graph in
  let island_count = List.length islands in
  if island_count > 1
  then
    List.range 0 (island_count - 2)
    |> List.iter ~f:(fun (island_number : int) ->
      G.add_edge
        graph
        (List.random_element_exn (List.nth_exn islands island_number))
        (List.random_element_exn (List.nth_exn islands (island_number + 1))))
  else ()
;;

(*Initialized a game w/ the islands and outputs a graph as the*)
let create game =
  let graph = G.create () in
  let size, bound =
    match game.difficulty with
    | Level.Easy -> 10, 9
    | Level.Medium -> 15, 10
    | Level.Hard -> 20, 11
  in
  let solar_system =
    [ "Neptune"
    ; "Pluto"
    ; "Uranus"
    ; "Mercury"
    ; "Venus"
    ; "Titan"
    ; "Mars"
    ; "Jupiter"
    ; "Sun"
    ; "Moon"
    ; "Ganymede"
    ; "Callisto"
    ; "Io"
    ; "Europa"
    ; "Triton"
    ; "Eris"
    ; "Titania"
    ; "Rhea"
    ; "Iapetus"
    ; "Oberon"
    ]
  in
  List.iter (List.range 0 size) ~f:(fun idx ->
    let planet = List.nth_exn solar_system idx in
    G.add_vertex graph planet);
  let nodes =
    List.map solar_system ~f:(fun planet ->
      let x =
        Int63.random (Int63.of_int bound) |> Int63.to_int |> Option.value_exn
      in
      let y =
        Int63.random (Int63.of_int bound) |> Int63.to_int |> Option.value_exn
      in
      planet, x, y)
  in
  create_graph ~graph ~nodes ~distance:1.0;
  Dot.output_graph (Out_channel.create "map.dot") graph
;;

(* Functions needed: - When player answers question, check answer, update
   score, and move players forward in map - Island color update *)

let game_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Run game"
    [%map_open
      let level =
        flag "level" (required Level.arg) ~doc:"how hard game is"
      in
      fun () ->
        let game =
          { (* player_one ; player_two ( ; game_state =
               Game_state.Game_continues island *)
            difficulty = level
          }
        in
        create game]
;;

(* let%expect_test "graph" = let game = { player_one ; player_two ;
   game_state = Game_state.Game_continues island ; difficulty = level } in
   let graph = create game in G.iter_vertex (fun vertex -> print_s [%sexp
   (G.out_degree graph vertex > 0 : bool)]) graph; [%expect {| true true true
   true |}] *)
