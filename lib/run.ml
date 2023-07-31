open! Core

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
    match Snake_graphics.read_key () with
    | None -> ()
    | Some key ->
      Game.handle_key game key;
      Snake_graphics.render game)
;;

let handle_steps (game : Game.t) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
    Game.step game;
    Snake_graphics.render game;
    match Game.game_state game with
    | Game_over _ | Win -> game_over := true
    | In_progress -> ())
;;

let handle_apple (game : Game.t) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
    Game.step game;
    Snake_graphics.render game;
    match Game.game_state game with
    | Game_over _ | Win -> game_over := true
    | In_progress -> ())
;;


let run () =
  let game = Snake_graphics.init_exn () in
  Snake_graphics.render game;
  let game_over = ref false in
  handle_keys game ~game_over;
  handle_steps game ~game_over
;;


