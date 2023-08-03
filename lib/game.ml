open! Core
open Async

let pointer = ref (-1)
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

module Game_state = struct
  type t = 
  |Start 
  |Game_over 
  |Answering of Player.t
  |Buzzing
  |Selecting of Player.t

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
  { player_one : Player.t ; 
    player_two : Player.t ; 
    mutable curr_player: Player.t;
    game_state : Game_state.t ;
    difficulty : Level.t ; 
    mutable islands : Island.t list ; 
    map : (Island.t, Island.t list) Hashtbl.t;
    mutable questions: Question.Question.t list;
    mutable selected_island: Island.t option
  }

  let swap_player (player:Player.t) (game:t) = 
    if Player.equal player game.player_one then game.player_two else game.player_one

  let update_start key = 
    match key with 
    |' ' -> Some Game_state.Buzzing
    (* need to add delay before buzzing is allowed, but we still need to visualize the difference *)
    |_ -> None
  ;;
  let update_buzzing (game:t) key = 
    match key with 
    |'p' -> (game.curr_player <- game.player_one; 
    Some (Game_state.Answering game.curr_player))
    |'q' ->(game.curr_player <- game.player_two; 
     Some (Game_state.Answering game.curr_player))
    | _ -> None;;

  (* need to add point subtraction *)
  let update_answer (game:t) key = 
    (match key with 
    |'a'  
    |'b'
    |'c'
    |'d' -> if (Question.is_correct (List.hd_exn game.questions) key) 
      then 
      (Some (Game_state.Selecting game.curr_player))
      else (
        game.curr_player <- (swap_player game.curr_player game);
        Some (Game_state.Selecting game.curr_player ))
    |_ -> None);;
  
    (* - counter for each time t is pressed
       - get adjacent islands from current uslands
       -  modulo counter over num of islands 
       - pass the island being selected to graphics
       - once y is selected, remove island from map mark it as visited and return buzzing*)

  (* at this point, we need to remove all copies of current island from hashtbl *)
  let update_selecting (game:t) key = 
    match key with 
    |'t' -> (let neighbors = Hashtbl.find_exn game.map game.curr_player.curr_island in 
    pointer:= (!pointer + 1)%(List.length neighbors);
    game.selected_island <- Some (List.nth_exn neighbors !pointer);
    Some (Game_state.Selecting game.curr_player)) 
    |'y' -> (
    Hashtbl.iter game.map ~f:(fun neighbors -> if List.mem neighbors (game.curr_player.curr_island ~equal:Island.equal) then )
    game.curr_player.curr_island <- Option.value_exn game.selected_island;
    game.selected_island <- None;
    pointer:= -1;
    
    Some Game_state.Buzzing
    )
    |_ -> None;;

module My_components = Graph.Components.Make (G)



let create_graph ~graph ~nodes ~(distance : float) ~(game:t)=
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
        then (G.add_edge graph node_1 node_2;
        let island_1 = List.find_exn game.islands ~f:(fun island -> String.equal node_1 island.name) in 
        let island_2 = List.find_exn game.islands ~f:(fun island -> String.equal node_2 island.name) in 

        Hashtbl.update game.map island_1 ~f:(fun list -> match list with |None -> [island_2] |Some nodes -> nodes @ [island_2])))));
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

module Coordinate = struct
  type t =  {
    name : string;
    x : int;
    y : int
  }
  [@@deriving sexp, compare, hash]
end

(*Initialized a game w/ the islands and outputs a graph*)
let create_islands difficulty =
  let x_scale = 10 in
  let y_scale = 8 in 
  let graph = G.create () in
  let bound = 100 in 
  let size =
    match difficulty with
    | Level.Easy -> 10
    | Level.Medium -> 15
    | Level.Hard -> 20
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

  let rec find_valid (current_nodes : Coordinate.t list ) : int * int = 
    let rand_x =
      Int63.random (Int63.of_int bound) |> Int63.to_int |> Option.value_exn |> Int.( * ) x_scale 
    in
    let rand_y =
      Int63.random (Int63.of_int bound) |> Int63.to_int |> Option.value_exn |> Int.( * ) y_scale
    in
    let closest_coordinate = List.min_elt (current_nodes) ~compare:(fun point_one point_two -> let distance_one = (Float.sqrt
    (Int.pow (point_one.x - rand_x) 2 + Int.pow (point_one.y - rand_y) 2 |> Float.of_int)) in let distance_two = 
    (Float.sqrt (Int.pow (point_two.x - rand_x) 2 + Int.pow (point_two.y - rand_y) 2 |> Float.of_int)) in Int.of_float (distance_two -. distance_one)) in 
    if (is_none closest_coordinate) then (rand_x,rand_y) else let coord = Option.value_exn closest_coordinate in 
    if (Float.( < )
    (Float.sqrt (Int.pow (coord.x - rand_x) 2 + Int.pow (coord.y - rand_y) 2 |> Float.of_int)) (Float.sqrt (Float.of_int 5))) then (find_valid current_nodes) else (rand_x, rand_y)
  ; in

  let (start : Coordinate.t list) = [] in 
  let nodes = 
    List.map (List.range 0 size) ~f:(fun idx ->
      let planet = List.nth_exn solar_system idx in
      G.add_vertex graph planet; let (x,y) = find_valid start in 
      planet, x, y)

  in
  let%map questions = Question.get_questions size in
  let islands =
    List.mapi nodes ~f:(fun idx (planet, x, y) ->
      { Island.name = planet
      ; position = x, y
      ; question = List.nth_exn questions idx
      })
  in
  (* create_graph ~graph ~nodes ~distance:10.0; *)
  Dot.output_graph (Out_channel.create "map.dot") graph;
  islands
;;

(* updates game state when player answer a question
   - checks answer, updates score
   - moves player to next island
   - updates island as visited *)

let handle_key (game:t) key  =
  match game.game_state with 
  |Start -> update_start key
  |Answering _ -> update_answer game key
  |Buzzing -> update_buzzing game key
  |Selecting _ -> update_selecting game key
  |_ -> None;;


let create (difficulty: Level.t)  = 
  let islands = create_graph Level.Easy in  
  let game =
    { 
    player_one = {
      name = "Player_one";
      points = 0;
      curr_island = List.nth_exn islands 0
    }; 
    player_two = {
      name = "Player_two";
      points = 0;
      curr_island = List.nth_exn islands 1;

    }; 
    curr_player = game.player_one;
    game_state = Game_state.Game_continues island ;
    difficulty = level ; 
    islands = islands;
    map = Hashtbl.create ();
    questions = Question.get_questions (match difficulty with |Easy -> 10 |Medium -> 15 |Hard -> 20);
    selected_island = None} in game
;;

let game_command =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Run game"
    [%map_open
      let level =
        flag "level" (required Level.arg) ~doc:"how hard game is"
      in
      fun () -> 
        create level]
;; 

(* let%expect_test "graph" = let game = { player_one ; player_two ;
   game_state = Game_state.Game_continues island ; difficulty = level } in
   let graph = create game in G.iter_vertex (fun vertex -> print_s [%sexp
   (G.out_degree graph vertex > 0 : bool)]) graph; [%expect {| true true true
   true |}] *)
