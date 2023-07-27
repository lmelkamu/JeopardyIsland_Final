open! Core
open! Async

(* [%map_open let how_to_fetch, resource = File_fetcher.param in fun () ->
   let contents = File_fetcher.fetch_exn how_to_fetch ~resource in List.iter
   (get_credits contents) ~f:print_endline] *)

let command =
  Command.group
    ~summary:"A tool for playing the wikipedia game, and other utilities"
    [ "game", JeopardyIsland_Final.Game.game_command
    ; "question", JeopardyIsland_Final.Question.question_command
    ]
;;

let () = Command_unix.run command
