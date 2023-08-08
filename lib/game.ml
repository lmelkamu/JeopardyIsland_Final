open! Core
open Async

let pointer = ref (-1)

module Level = struct
  module T = struct
    type t =
      | Easy
      | Medium
      | Hard
    [@@deriving enumerate, sexp]

    (* let to_string t = Sexp.to_string (sexp_of_t t) *)
  end

  include T

  (* let arg : t Command.Arg_type.t = Command.Arg_type.enumerated (module
     T) *)
end

module Game_state = struct
  type t =
    | Start
    | Game_over
    | Answering of Player.t
    | Buzzing
    | Selecting of Player.t
  [@@deriving sexp]

  let to_string t =
    match t with
    | Start -> "Jeopardy Island!!"
    | Game_over -> "Game over"
    | Answering player -> String.append player.name " is answering"
    | Buzzing -> "Buzz in to answer - 'q' for Player 1, 'p' for Player 2"
    | Selecting player ->
      String.append
        player.name
        " is selecting the next island - press 't' to toggle and 'y' to \
         select"
  ;;
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
  { player_one : Player.t
  ; player_two : Player.t
  ; mutable curr_player : Player.t
  ; mutable game_state : Game_state.t
  ; mutable islands : Island.t list
  ; map : (Island.t, Island.Set.t) Hashtbl.t
  ; mutable questions : Question.Question.t list
  ; mutable selected_island : Island.t option
  }

let swap_player (player : Player.t) (game : t) =
  if Player.equal player game.player_one
  then game.player_two
  else game.player_one
;;

let update_start game key =
  match key with
  | ' ' -> game.game_state <- Game_state.Buzzing
  (* need to add delay before buzzing is allowed, but we still need to
     visualize the difference *)
  | _ -> ()
;;

let update_buzzing (game : t) key =
  match key with
  | 'p' ->
    game.curr_player <- game.player_two;
    game.game_state <- Game_state.Answering game.curr_player
  | 'q' ->
    game.curr_player <- game.player_one;
    game.game_state <- Game_state.Answering game.curr_player
  | _ -> ()
;;

(* need to add point subtraction *)
let update_answer (game : t) key =
  match key with
  | 'a' | 'b' | 'c' | 'd' ->
    if Question.is_correct (List.hd_exn game.questions) key
    then (
      game.game_state <- Game_state.Selecting game.curr_player;
      game.curr_player.points <- game.curr_player.points + 3)
    else (
      game.curr_player.points <- game.curr_player.points - 3;
      game.curr_player <- swap_player game.curr_player game;
      game.game_state <- Game_state.Selecting game.curr_player)
  | _ -> ()
;;

(* at this point, we need to remove all copies of current island from
   hashtbl *)
let update_selecting (game : t) key =
  match key with
  | 't' ->
    (match Hashtbl.find game.map game.curr_player.curr_island with
     | None -> game.game_state <- Game_state.Game_over
     | Some neighbors ->
       pointer := (!pointer + 1) % Set.length neighbors;
       game.selected_island
         <- Some (Set.nth neighbors !pointer |> Option.value_exn);
       game.game_state <- Game_state.Selecting game.curr_player)
  | 'y' ->
    Hashtbl.remove game.map game.curr_player.curr_island;
    let map_copy = Hashtbl.copy game.map in
    Hashtbl.iter_keys map_copy ~f:(fun island ->
      Hashtbl.update game.map island ~f:(fun neighbors ->
        Set.remove (Option.value_exn neighbors) game.curr_player.curr_island));
    game.curr_player.curr_island <- Option.value_exn game.selected_island;
    game.selected_island <- None;
    pointer := -1;
    game.questions <- List.tl_exn game.questions;
    game.game_state <- Game_state.Buzzing
  | _ -> ()
;;

module My_components = Graph.Components.Make (G)

let create_graph ~graph ~(distance : float) ~(game : t) =
  List.iter game.islands ~f:(fun island_1 ->
    List.iter game.islands ~f:(fun island_2 ->
      if String.equal island_1.name island_2.name
      then ()
      else (
        let x1, y1 = island_1.position in
        let x2, y2 = island_2.position in
        let pythagoreum =
          Float.sqrt
            (Int.pow (x2 - x1) 2 + Int.pow (y2 - y1) 2 |> Float.of_int)
        in
        if Float.( < ) pythagoreum distance
        then (
          G.add_edge graph island_1.name island_2.name;
          Hashtbl.update game.map island_1 ~f:(fun set ->
            match set with
            | None -> Island.Set.singleton island_2
            | Some nodes -> Set.add nodes island_2)))));
  let distinct_islands = My_components.scc_list graph in
  let island_count = List.length distinct_islands in
  if island_count > 1
  then
    List.range 0 (island_count - 1)
    |> List.iter ~f:(fun (island_number : int) ->
      let island_1_name =
        List.random_element_exn (List.nth_exn distinct_islands island_number)
      in
      let island_2_name =
        List.random_element_exn
          (List.nth_exn distinct_islands (island_number + 1))
      in
      let random_island_1 =
        List.find_exn game.islands ~f:(fun island ->
          String.equal island_1_name island.name)
      in
      let random_island_2 =
        List.find_exn game.islands ~f:(fun island ->
          String.equal island_2_name island.name)
      in
      G.add_edge graph island_1_name island_2_name;
      Hashtbl.update game.map random_island_1 ~f:(fun set ->
        match set with
        | None -> Island.Set.singleton random_island_2
        | Some nodes -> Set.add nodes random_island_2);
      Hashtbl.update game.map random_island_2 ~f:(fun set ->
        match set with
        | None -> Island.Set.singleton random_island_1
        | Some nodes -> Set.add nodes random_island_1))
  else ()
;;

(*Initialized the islands and outputs a graph*)
let create_islands difficulty =
  let x_scale = 10 in
  let y_scale = 9 in
  let graph = G.create () in
  let bound = 100 in
  let right_left_margin = 5 in
  let up_down_margin = 15 in
  let size =
    match difficulty with
    | Level.Easy -> 25
    | Level.Medium -> 30
    | Level.Hard -> 40
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
    ; "Dimidium"
    ; "Hoth"
    ; "Astronova"
    ; "Veridian Prime"
    ; "Zephyrion"
    ; "Lunaris"
    ; "Thalassia"
    ; "Nova-9"
    ; "Epsilon Eridani"
    ; "Caelum"
    ; "Solara"
    ; "Nebuloria"
    ; "Aurelia"
    ; "Orionis"
    ; "Pandora"
    ; "Vulcanus"
    ; "Galactica"
    ; "Lyra Minor"
    ; "Xenoth"
    ; "Terra Nova"
    ; "Aquaar"
    ; "Magmara"
    ; "Celestis"
    ; "Stellara"
    ; "Chronos"
    ; "Iridia"
    ; "Zephyria"
    ]
  in
  let rec find_valid (current_nodes : (int * int) list) (max_islands : int)
    : (int * int) list
    =
    if List.length current_nodes = max_islands
    then current_nodes
    else (
      let rand_coordinate margin scale =
        let rand_int =
          Int63.random (Int63.of_int (bound - (2 * margin)))
          |> Int63.to_int_exn
        in
        scale * (rand_int + margin)
      in
      let rand_x = rand_coordinate right_left_margin x_scale in
      let rand_y = rand_coordinate up_down_margin y_scale in
      let squared_dist (x, y) =
        Int.pow (x - rand_x) 2 + Int.pow (y - rand_y) 2
      in
      let is_too_close =
        List.exists current_nodes ~f:(fun (x, y) ->
          squared_dist (x, y) < 4000)
      in
      if is_too_close
      then find_valid current_nodes max_islands
      else find_valid (current_nodes @ [ rand_x, rand_y ]) max_islands)
  in
  let positions = find_valid [] size in
  let islands =
    List.map (List.range 0 size) ~f:(fun idx ->
      let planet = List.nth_exn solar_system idx in
      G.add_vertex graph planet;
      let x, y = List.nth_exn positions idx in
      Island.create ~name:planet ~position:(x, y))
  in
  let leftmost, rightmost =
    List.fold
      islands
      ~init:
        ( Island.create ~name:"small" ~position:(Int.max_value, Int.max_value)
        , Island.create ~name:"big" ~position:(0, 0) )
      ~f:(fun (small, big) island ->
        let min_x, _ = small.position in
        let max_x, _ = big.position in
        let x, _ = island.position in
        let new_min = if x < min_x then island else small in
        let new_max = if x > max_x then island else big in
        new_min, new_max)
  in
  let%map questions = Question.get_questions size in
  (* create_graph ~graph ~nodes ~distance:10.0; *)
  Dot.output_graph (Out_channel.create "map.dot") graph;
  islands, questions, graph, leftmost, rightmost
;;

let create
  (difficulty : Level.t)
  (player_1_name : string)
  (player_2_name : string)
  =
  let%map ( (islands : Island.t list)
          , (questions : Question.t)
          , (graph : G.t)
          , (leftmost : Island.t)
          , (rightmost : Island.t) )
    =
    create_islands difficulty
  in
  let player_one = Player.create ~name:player_1_name ~island:leftmost in
  let player_two = Player.create ~name:player_2_name ~island:rightmost in
  let game =
    { player_one
    ; player_two
    ; curr_player = player_one
    ; game_state = Game_state.Start
    ; islands
    ; map = Island.Table.create ()
    ; questions
    ; selected_island = None
    }
  in
  create_graph ~graph ~distance:150.0 ~game;
  game
;;

let handle_key (game : t) key =
  match game.game_state with
  | Start -> update_start game key
  | Answering _ -> update_answer game key
  | Buzzing -> update_buzzing game key
  | Selecting _ -> update_selecting game key
  | _ -> ()
;;
