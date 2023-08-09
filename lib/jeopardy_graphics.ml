open! Core
open! Async
open! Graphics

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 137 177 096
  let head_color = Graphics.rgb 204 176 136
  let gold = Graphics.rgb 255 223 0
  let blue = Graphics.rgb 056 134 151
  let white = Graphics.rgb 225 225 225
  let tan = Graphics.rgb 238 197 158

  (* clothing colors *)
  let jeans = Graphics.rgb 35 94 158
  let red = Graphics.rgb 237 37 78
  let purple = Graphics.rgb 94 35 157
  let orange = Graphics.rgb 255 177 64
  let gray = Graphics.rgb 165 151 151
  let lime = Graphics.rgb 199 239 0
  (* let game_in_progress = Graphics.rgb 100 100 200 let game_lost =
     Graphics.rgb 200 100 100 let game_won = Graphics.rgb 100 200 100 *)
end

let player_one_color = ref Colors.red
let player_two_color = ref Colors.red

(* These constants are optimized for running on a low-resolution screen. Feel
   free to increase the scaling factor to tweak! *)
module Constants = struct
  let play_area_height = 900
  let header_height = 100
  let play_area_width = 1000
  let circle_size = 25

  let colors =
    [ Colors.red; Colors.purple; Colors.orange; Colors.gray; Colors.lime ]
  ;;
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
    (Printf.sprintf " %dx%d" play_area_width play_area_height);
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
  Graphics.set_color Colors.blue;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

let draw_sprites (game : Game.t) =
  let x_1, y_1 = game.player_one.curr_island.position in
  let x_2, y_2 = game.player_two.curr_island.position in
  Graphics.set_color !player_one_color;
  Graphics.fill_rect (x_1 - 11) (y_1 + 25) 21 25;
  Graphics.set_color !player_two_color;
  Graphics.fill_rect (x_2 - 11) (y_2 + 25) 21 25;
  Graphics.set_color Colors.tan;
  Graphics.fill_circle (x_1 - 1) (y_1 + 62) 12;
  Graphics.fill_circle (x_2 - 1) (y_2 + 62) 12;
  Graphics.set_color Colors.black;
  Graphics.draw_rect (x_1 - 11) (y_1 + 25) 21 25;
  Graphics.draw_rect (x_2 - 11) (y_2 + 25) 21 25;
  Graphics.draw_circle (x_1 - 1) (y_1 + 62) 12;
  Graphics.draw_circle (x_2 - 1) (y_2 + 62) 12
;;

let draw_islands (game : Game.t) =
  let (map : (Island.t, Island.Set.t) Hashtbl.t) = game.map in
  Hashtbl.iter_keys map ~f:(fun island_1 ->
    let x, y = island_1.position in
    Graphics.set_color Colors.white;
    Set.iter (Hashtbl.find_exn map island_1) ~f:(fun island_2 ->
      let x_2, y_2 = island_2.position in
      Graphics.moveto x y;
      Graphics.set_line_width 3;
      Graphics.lineto x_2 y_2));
  Hashtbl.iter_keys map ~f:(fun island_1 ->
    let x, y = island_1.position in
    draw_circle x y ~color:Colors.green);
  let player_one_island = game.player_one.curr_island in
  let p_1_x, p_1_y = player_one_island.position in
  draw_circle p_1_x p_1_y ~color:!player_one_color;
  let player_two_island = game.player_two.curr_island in
  let p_2_x, p_2_y = player_two_island.position in
  draw_circle p_2_x p_2_y ~color:!player_two_color;
  draw_sprites game;
  if is_none game.selected_island
  then ()
  else (
    let selected = Option.value_exn game.selected_island in
    let category = selected.question.category in
    let x, y = selected.position in
    Graphics.set_font "-adobe-courier-bold-r-normal--10-0-0-0-m-0-iso8859-1";
    Graphics.moveto (x - (3 * String.length category)) (y - 35);
    Graphics.set_color Colors.black;
    Graphics.draw_string category)
;;

let draw_visited (game : Game.t) =
  let player_one_islands =
    List.filter game.visisted_islands ~f:(fun island ->
      Option.value_exn island.team)
  in
  let player_two_islands =
    List.filter game.visisted_islands ~f:(fun island ->
      not (Option.value_exn island.team))
  in
  List.iter player_one_islands ~f:(fun island ->
    let x, y = island.position in
    draw_circle x y ~color:!player_one_color);
  List.iter player_two_islands ~f:(fun island ->
    let x, y = island.position in
    draw_circle x y ~color:!player_two_color)
;;

(* let draw_apple apple = let apple_position = Apple.position apple in
   draw_block apple_position ~color:(Colors.apple_color apple) ;; *)
(* let draw_snake snake_head snake_tail = List.iter snake_tail ~f:(draw_block
   ~color:Colors.green); (* Snake head is a different color *) draw_block
   ~color:Colors.head_color snake_head ;; *)
(* let render game = (* We want double-buffering. See
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Graphics.html for more
   info! So, we set [display_mode] to false, draw to the background buffer,
   set [display_mode] to true and then synchronize. This guarantees that
   there won't be flickering! *) Graphics.display_mode false; let snake =
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
  let rect_width = 300 in
  let rect_height = 200 in
  let question = game.current_question in
  let question_string = question.question in
  (* let word_split = String.split words ~on:' ' in List.split_n word_split
     number_of_words in *)
  let word_separations = split_string question_string 20 in
  (* print_s [%message (word_separations : string list * string list)]; *)
  Graphics.set_color Colors.head_color;
  Graphics.fill_rect
    ((play_area_width - rect_width) / 2)
    ((play_area_height - rect_height) / 2)
    rect_width
    rect_height;
  Graphics.set_color Colors.black;
  List.iteri word_separations ~f:(fun line_number line ->
    Graphics.moveto
      ((play_area_width / 2) - (5 * String.length line))
      ((play_area_height / 2) + 30 - (20 * line_number));
    Graphics.draw_string line);
  List.iteri
    (question.answers : string list)
    ~f:(fun idx answer ->
      let answer_choice = List.nth_exn choices idx in
      let text = split_string (String.append answer_choice answer) 15 in
      List.iteri text ~f:(fun index line ->
        Graphics.moveto (20 + (play_area_width * idx / 4)) (75 - (20 * index));
        Graphics.draw_string line))
;;

let draw_correct (letter : char) (answer : string) =
  let open Constants in
  let rect_width = 300 in
  let rect_height = 200 in
  Graphics.set_color Colors.head_color;
  Graphics.fill_rect
    ((play_area_width - rect_width) / 2)
    ((play_area_height - rect_height) / 2)
    rect_width
    rect_height;
  Graphics.set_color Colors.black;
  Graphics.moveto ((play_area_width / 2) - 60) ((play_area_height / 2) + 50);
  Graphics.draw_string "Correct answer:";
  let text =
    split_string [%string "%{Char.to_string letter}. %{answer}"] 20
  in
  List.iteri text ~f:(fun index line ->
    Graphics.moveto
      ((play_area_width / 2) - (String.length line * 5))
      ((play_area_height / 2) - (20 * index));
    Graphics.draw_string line)
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;

let handle_game_states_visually (game : Game.t) =
  let open Constants in
  let state = game.game_state in
  match state with
  | Start (player_one_index, player_two_index) ->
    player_one_color := List.nth_exn colors player_one_index;
    player_two_color := List.nth_exn colors player_two_index;
    let text =
      split_string
        " Welcome to Jeopardy Island. Players can choose their colors by \
         pressing 'a' and 'l'. Press spacebar to start!!"
        50
    in
    List.iteri text ~f:(fun index line ->
      Graphics.moveto
        ((play_area_width / 2) - 250)
        ((play_area_height / 2) - (20 * index));
      Graphics.draw_string line);
    (* mounds *)
    Graphics.fill_arc 150 header_height 50 30 0 180;
    Graphics.fill_arc (play_area_width - 150) header_height 50 30 0 180;
    (* legs *)
    Graphics.set_color Colors.jeans;
    Graphics.fill_rect 135 (header_height + 30) 10 60;
    Graphics.fill_rect 160 (header_height + 30) 10 60;
    Graphics.fill_rect (play_area_width - 140) (header_height + 30) 10 60;
    Graphics.fill_rect (play_area_width - 165) (header_height + 30) 10 60;
    Graphics.fill_rect 135 (header_height + 90) 35 20;
    Graphics.fill_rect (play_area_width - 165) (header_height + 90) 35 20;
    (* shirt *)
    Graphics.set_color !player_one_color;
    Graphics.fill_rect 135 (header_height + 110) 35 40;
    Graphics.fill_rect 125 (header_height + 150) 55 20;
    Graphics.set_color !player_two_color;
    Graphics.fill_rect (play_area_width - 165) (header_height + 110) 35 40;
    Graphics.fill_rect (play_area_width - 175) (header_height + 150) 55 20;
    (* head + arms *)
    Graphics.set_color Colors.tan;
    Graphics.fill_rect 125 (header_height + 110) 10 40;
    Graphics.fill_rect 170 (header_height + 110) 10 40;
    Graphics.fill_rect (play_area_width - 175) (header_height + 110) 10 40;
    Graphics.fill_rect (play_area_width - 130) (header_height + 110) 10 40;
    Graphics.fill_circle 152 (header_height + 190) 20;
    Graphics.fill_circle (play_area_width - 148) (header_height + 190) 20
  | Game_over ->
    Graphics.draw_rect 0 0 play_area_width play_area_height;
    Graphics.set_color Colors.tan;
    Graphics.moveto ((play_area_width / 2) - 20) (play_area_height / 2);
    Graphics.draw_string " Game Over"
  | Answering _ | Buzzing ->
    draw_islands game;
    draw_visited game;
    draw_question_and_answers game
  | Correct_answer _ ->
    let correct_answer = game.current_question.correct_answer in
    let index_corect =
      match correct_answer with
      | 'a' -> 0
      | 'b' -> 1
      | 'c' -> 2
      | 'd' -> 3
      | _ -> failwith ""
    in
    let correct_string =
      List.nth_exn game.current_question.answers index_corect
    in
    draw_islands game;
    draw_visited game;
    draw_correct correct_answer correct_string
  | Selecting _ ->
    draw_islands game;
    draw_visited game;
    (match game.selected_island with
     | None -> ()
     | Some island ->
       let x, y = island.position in
       draw_circle x y ~color:Colors.gold)
;;

(* used to move the player two score over to the right so it remains aligned
   with the right edge of the screen*)
let rec num_length num = if abs num < 10 then 1 else 1 + num_length (num / 10)

let draw_board (game : Game.t) =
  let open Constants in
  let player_one = game.player_one in
  let player_two = game.player_two in
  let player_one_score = player_one.points in
  let player_two_score = player_two.points in
  let player_two_score_length = num_length player_two_score in
  let game_state = game.game_state in
  Graphics.set_color Colors.black;
  (* Graphics.set_font
     "-adobe-courier-medium-r-*-*-18-*-*-*-*-70-iso8859-1"; *)
  (* Graphics.set_font
     "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1"; *)
  Graphics.set_font "-adobe-courier-bold-r-normal--18-0-0-0-m-0-iso8859-1";
  Graphics.display_mode false;
  (* box 1: play area *)
  draw_play_area ();
  (* box 2: top header *)
  Graphics.set_color Colors.head_color;
  Graphics.fill_rect
    0
    (play_area_height - header_height)
    play_area_width
    header_height;
  let header_text = Game.Game_state.to_string game_state in
  let header_text_length = String.length header_text in
  Graphics.moveto
    ((play_area_width / 2) - (6 * header_text_length))
    (play_area_height - 75);
  Graphics.set_color Colors.black;
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto
    (play_area_width
     - 140
     - (9 * (player_two_score_length + String.length player_two.name)))
    (play_area_height - 50);
  Graphics.draw_string
    [%string "%{player_two.name} Score: %{player_two_score#Int}"];
  Graphics.moveto
    (20 + (String.length player_one.name * 2 / 3))
    (play_area_height - 50);
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
  Graphics.set_font "-adobe-courier-bold-r-normal--16-0-0-0-m-0-iso8859-1";
  handle_game_states_visually game;
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
