open! Core
open! Async
open! In_channel
open! Stdio

(* This is the core logic that actually runs the game. We have implemented
   all of this for you, but feel free to read this file as a reference. *)

let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let handle_keys (game : Game.t) ~game_over =
  every ~stop:game_over 0.001 ~f:(fun () ->
    match Jeopardy_graphics.read_key () with
    | None -> ()
    | Some key ->
      Game.handle_key game key;
      (match game.game_state with
       | Game_over _ -> game_over := true
       | _ -> ());
      Jeopardy_graphics.draw_board game
    (* Jeopardy_graphics.render game) *))
;;

let run () =
  print_string
    "\n\n\
     WELCOME TO JEOPARDY ISLAND!!\n\n\
     Easy - 1\n\
     Medium - 2\n\
     Hard - 3\n\n\
     Choose a level: ";
  let level = Stdlib.read_int () in
  print_string "\nEnter Player 1 name: ";
  let player_one = Stdlib.read_line () in
  print_string "\nEnter Player 2 name: ";
  let player_two = Stdlib.read_line () in
  let%bind game = Jeopardy_graphics.init_exn level player_one player_two in
  Jeopardy_graphics.draw_board game;
  let game_over = ref false in
  return (handle_keys game ~game_over)
;;
