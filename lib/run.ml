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
    match Jeopardy_graphics.read_key () with
    | None -> ()
    | Some key ->
      Game.handle_key game key;
      Jeopardy_graphics.render game)
;;

let run () =
  let game = Jeopardy_graphics.init_exn () in
  Jeopardy_graphics.render game;
  let game_over = ref false in
  handle_keys game ~game_over
;;
