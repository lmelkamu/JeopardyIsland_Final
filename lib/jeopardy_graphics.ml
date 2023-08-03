open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000

  (* let green = Graphics.rgb 000 255 000 *)
  let head_color = Graphics.rgb 100 100 125
  let red = Graphics.rgb 255 000 000
  (* let gold = Graphics.rgb 255 223 0 let game_in_progress = Graphics.rgb
     100 100 200 let game_lost = Graphics.rgb 200 100 100 let game_won =
     Graphics.rgb 100 200 100 *)
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

let init_exn () : Game.t =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (play_area_height + header_height)
       play_area_width);
  Game.create Game.Level.T.Easy
;;

let draw_circle (row : int) (col : int) ~color =
  let open Constants in
  let col = col * circle_size in
  let row = row * circle_size in
  Graphics.set_color color;
  Graphics.fill_circle (col + 1) (row + 1) circle_size
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

let draw_islands (game : Game.t) =
  let (map : (Island.t, Island.t list) Hashtbl.t) = game.map in
  Hashtbl.iter_keys map ~f:(fun island ->
    let x, y = island.position in
    let adjusted_x = 10 * x in
    let adjusted_y = 8 * y in
    draw_circle x y ~color:Colors.red)
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

let draw_question (game : Game.t) =
  let open Constants in
  let questions = game.questions in
  let question = List.hd_exn questions in
  Graphics.set_text_size 16;
  Graphics.set_color Colors.head_color;
  Graphics.fill_rect 700 300 200 300;
  Graphics.moveto 700 300;
  Graphics.draw_string question.question;
  List.fold
    (question.answers : string list)
    ~init:10
    ~f:(fun x_adjusted answer ->
      Graphics.moveto x_adjusted (play_area_height - 70);
      Graphics.draw_string answer;
      x_adjusted + (play_area_width / 4))
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;

let draw_board (game : Game.t) =
  let open Constants in
  let player_one = game.player_one in
  let player_two = game.player_two in
  let player_one_score = player_one.points in
  let player_two_score = player_two.points in
  let game_state = game.game_state in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 20;
  (* box 1: play area *)
  draw_play_area ();
  draw_islands game;
  (* box 2: top header *)
  Graphics.display_mode false;
  Graphics.set_color Colors.head_color;
  Graphics.fill_rect 0 header_height play_area_width header_height;
  Graphics.moveto (play_area_width / 2) (header_height - 20);
  let header_text = Game.Game_state.to_string game_state in
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto (play_area_width - 75) 25;
  Graphics.draw_string (Printf.sprintf "Player_2 Score: %d" player_two_score);
  Graphics.moveto 75 25;
  Graphics.draw_string (Printf.sprintf "Player_1 Score: %d" player_one_score);
  (* box 3: bottom box *)
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  Graphics.moveto 0 (play_area_height - 70);
  Graphics.draw_string "A:";
  Graphics.moveto (play_area_width / 4) (play_area_height - 70);
  Graphics.draw_string "B:";
  Graphics.moveto (play_area_width / 2) (play_area_height - 70);
  Graphics.draw_string "C:";
  Graphics.moveto (play_area_width * 3 / 4) (play_area_height - 70);
  Graphics.draw_string "D:";
  Graphics.display_mode true;
  Graphics.synchronize ()
;;
