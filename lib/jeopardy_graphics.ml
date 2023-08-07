open! Core
open! Async
open! Graphics

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let head_color = Graphics.rgb 100 100 125
  let red = Graphics.rgb 255 000 000
  let gold = Graphics.rgb 255 223 0
  let blue = Graphics.rgb 0 0 255
  let purple = Graphics.rgb 100 0 100
  (* let game_in_progress = Graphics.rgb 100 100 200 let game_lost =
     Graphics.rgb 200 100 100 let game_won = Graphics.rgb 100 200 100 *)
end

(* These constants are optimized for running on a low-resolution screen. Feel
   free to increase the scaling factor to tweak! *)
module Constants = struct
  let scaling_factor = 1.
  let play_area_height = 800. *. scaling_factor |> Float.iround_down_exn
  let header_height = 100. *. scaling_factor |> Float.iround_down_exn
  let play_area_width = 1000. *. scaling_factor |> Float.iround_down_exn
  let circle_size = 25. *. scaling_factor |> Float.iround_down_exn
end

(* let draw_header ~game_state score = let open Constants in let header_color
   = match (game_state : Game_state.t) with | In_progress ->
   Colors.game_in_progress | Game_over _ -> Colors.game_lost | Win ->
   Colors.game_won in Graphics.set_color header_color; Graphics.fill_rect 0
   play_area_height play_area_width header_height; let header_text =
   Game_state.to_string game_state in Graphics.set_color Colors.black;
   Graphics.set_text_size 20; Graphics.moveto 0 (play_area_height + 25);
   Graphics.draw_string (Printf.sprintf " %s" header_text); Graphics.moveto
   (play_area_width - 75) (play_area_height + 25); Graphics.draw_string
   (Printf.sprintf "Score: %d" score) ;; *)

let only_one : bool ref = ref false

let init_exn level_num player_one player_two =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       play_area_width
       (play_area_height + header_height));
  let level =
    match level_num with
    | 1 -> Game.Level.T.Easy
    | 2 -> Game.Level.T.Medium
    | 3 -> Game.Level.T.Hard
    | _ -> Game.Level.T.Easy
  in
  Game.create level player_one player_two
;;

let draw_circle (x : int) (y : int) ~color =
  let open Constants in
  Graphics.set_color color;
  Graphics.fill_circle x y circle_size
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

let draw_islands (game : Game.t) =
  let (map : (Island.t, Island.Set.t) Hashtbl.t) = game.map in
  Hashtbl.iter_keys map ~f:(fun island_1 ->
    let x, y = island_1.position in
    draw_circle x y ~color:Colors.green;
    Graphics.set_color Colors.red;
    Set.iter (Hashtbl.find_exn map island_1) ~f:(fun island_2 ->
      let x_2, y_2 = island_2.position in
      Graphics.moveto x y;
      Graphics.set_line_width 3;
      Graphics.lineto x_2 y_2));
  let player_one_island = game.player_one.curr_island in
  let p_1_x, p_1_y = player_one_island.position in
  draw_circle p_1_x p_1_y ~color:Colors.purple;
  let player_two_island = game.player_two.curr_island in
  let p_2_x, p_2_y = player_two_island.position in
  draw_circle p_2_x p_2_y ~color:Colors.blue
;;

(* let draw_apple apple = let apple_position = Apple.position apple in
   draw_block apple_position ~color:(Colors.apple_color apple) ;; *)

(* let draw_snake snake_head snake_tail = List.iter snake_tail ~f:(draw_block
   ~color:Colors.green); (* Snake head is a different color *) draw_block
   ~color:Colors.head_color snake_head ;; *)

(* let render game = (* We want double-buffering. See
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Graphics.html for more
   info!

   So, we set [display_mode] to false, draw to the background buffer, set
   [display_mode] to true and then synchronize. This guarantees that there
   won't be flickering! *) Graphics.display_mode false; let snake =
   Game.snake game in let snake_two = Game.snake_two game in let apple =
   Game.apple game in let game_state = Game.game_state game in let score =
   Game.score game in draw_header ~game_state score; draw_play_area ();
   draw_apple apple; draw_snake (Snake.head snake) (Snake.tail snake);
   draw_snake (Snake.head snake_two) (Snake.tail snake_two);
   Graphics.display_mode true; Graphics.synchronize () ;; *)

(* take the string, split based on the character number, and then return the
   split list*)
let split_string (words : string) (number_of_chars : int) : string list =
  let word_split = String.split words ~on:' ' in
  let _, last_section, rev_words_list =
    List.fold
      word_split
      ~init:(0, [], [])
      ~f:(fun (current_count, current_words, whole_list) word ->
        let word_length = String.length word in
        if current_count + word_length > number_of_chars
        then word_length, [ word ], List.rev current_words :: whole_list
        else current_count + word_length, word :: current_words, whole_list)
  in
  let rev_words_list = List.rev last_section :: rev_words_list in
  let words_list = List.rev rev_words_list in
  List.map words_list ~f:(String.concat ~sep:" ")
;;

let%expect_test _ =
  let words =
    "Hi my name is Roman, I want to know how this function works"
  in
  let words_list = split_string words 15 in
  print_s [%message (words_list : string list)];
  return
    [%expect
      {|
    (words_list
     ("Hi my name is" "Roman, I want to" "know how this" "function works")) |}]
;;

let draw_question_and_answers (game : Game.t) =
  let open Constants in
  let choices = [ "A:"; "B:"; "C:"; "D:" ] in
  let rect_width = 600 in
  let rect_height = 300 in
  let questions = game.questions in
  let question = List.hd_exn questions in
  let question_string = question.question in
  (* let word_split = String.split words ~on:' ' in List.split_n word_split
     number_of_words in *)
  let word_separations = split_string question_string 20 in
  (* print_s [%message (word_separations : string list * string list)]; *)
  Graphics.fill_rect
    ((play_area_width - rect_width) / 2)
    ((play_area_height - rect_height) / 2)
    rect_width
    rect_height;
  Graphics.set_color Colors.red;
  List.iteri word_separations ~f:(fun line_number line ->
    Graphics.moveto
      ((play_area_width / 2) - (2 * String.length line))
      ((play_area_height / 2) - (20 * line_number));
    Graphics.draw_string line);
  List.iteri
    (question.answers : string list)
    ~f:(fun idx answer ->
      let answer_choice = List.nth_exn choices idx in
      Graphics.moveto (20 + (play_area_width * idx / 4)) 20;
      Graphics.draw_string (String.append answer_choice answer))
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;

let handle_game_states_visually (game : Game.t) =
  let open Constants in
  let state = game.game_state in
  match state with
  | Start ->
    Graphics.draw_rect 0 0 play_area_width play_area_height;
    Graphics.moveto ((play_area_width / 2) - 70) (play_area_height / 2);
    Graphics.draw_string
      " Welcome to Jeopardy Island. Press Spacebar to Start"
  | Game_over ->
    Graphics.draw_rect 0 0 play_area_width play_area_height;
    Graphics.moveto ((play_area_width / 2) - 20) (play_area_height / 2);
    Graphics.draw_string " Game Over"
  | Answering _ -> draw_question_and_answers game
  | Buzzing -> draw_question_and_answers game
  | Selecting _ ->
    draw_islands game;
    (match game.selected_island with
     | None -> ()
     | Some island ->
       let x, y = island.position in
       draw_circle x y ~color:Colors.gold)
;;

let draw_board (game : Game.t) =
  let open Constants in
  let player_one = game.player_one in
  let player_two = game.player_two in
  let player_one_score = player_one.points in
  let player_two_score = player_two.points in
  let game_state = game.game_state in
  Graphics.set_color Colors.black;
  Graphics.set_font "-adobe-courier-medium-r-*-*-18-*-*-*-*-70-iso8859-1";
  Graphics.set_font
    "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1";
  Graphics.display_mode false;
  (* box 1: play area *)
  draw_play_area ();
  (* box 2: top header *)
  Graphics.set_color Colors.head_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  Graphics.moveto (play_area_width / 3) (play_area_height + 50);
  Graphics.set_color Colors.red;
  let header_text = Game.Game_state.to_string game_state in
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto (play_area_width * 4 / 5) (play_area_height + 50);
  Graphics.draw_string
    [%string "%{player_two.name} Score: %{player_two_score#Int}"];
  Graphics.moveto 20 (play_area_height + 50);
  Graphics.draw_string
    [%string "%{player_one.name} Score: %{player_one_score#Int}"];
  Graphics.set_color Colors.head_color;
  (* box 3: bottom box *)
  Graphics.fill_rect 0 0 play_area_width header_height;
  (* Graphics.moveto right_shift (play_area_height - 70);
     Graphics.draw_string "A:"; Graphics.moveto ((play_area_width / 4) +
     right_shift) 70; Graphics.draw_string "B:"; Graphics.moveto
     ((play_area_width / 2) + right_shift) 70; Graphics.draw_string "C:";
     Graphics.moveto ((play_area_width * 3 / 4) + right_shift) 70;
     Graphics.draw_string "D:"; *)
  handle_game_states_visually game;
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
