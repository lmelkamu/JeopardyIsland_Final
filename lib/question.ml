open! Core
open! Async
open! Cohttp_async
open! Jsonaf.Export
open! Ppxlib

module Question = struct
  type t =
    { question : string
    ; correct_answer : string
    ; incorrect_answers : string list
    }
  [@@deriving jsonaf, sexp] [@@jsonaf.allow_extra_fields]
end

(* things we need to get set up: pull from an API ORRRR, we need a sufficient
   question bank with quetsions and plausible answers *)
let get_questions number =
  let%bind _, body =
    Cohttp_async.Client.get
      (Uri.of_string
         (String.append
            "https://opentdb.com/api.php?amount="
            (Int.to_string number)))
  in
  Body.to_string body
;;

type t = { results : Question.t list }
[@@deriving jsonaf, sexp] [@@jsonaf.allow_extra_fields]

let%expect_test _ =
  let json_str =
    {|{"response_code":0,"results":[{"category":"General Knowledge","type":"multiple","difficulty":"medium","question":"What is the currency of Poland?","correct_answer":"Z\u0142oty","incorrect_answers":["Ruble","Euro","Krone"]},{"category":"Entertainment: Music","type":"multiple","difficulty":"medium","question":"Which song made by MAN WITH A MISSION was used as the opening for the anime &quot;Log Horizon&quot;?","correct_answer":"&quot;Database&quot;","incorrect_answers":["&quot;Dead End in Tokyo&quot;","&quot;Raise Your Flag&quot;","&quot;Out of Control&quot;"]}]}|}
  in
  let json = Jsonaf.parse json_str |> Or_error.ok_exn in
  let questions = t_of_jsonaf json in
  print_s [%message (questions : t)];
  return
    [%expect
      {|
    (questions
     ((results
       (((question "What is the currency of Poland?")
         (correct_answer "Z\197\130oty") (incorrect_answers (Ruble Euro Krone)))
        ((question
          "Which song made by MAN WITH A MISSION was used as the opening for the anime &quot;Log Horizon&quot;?")
         (correct_answer "&quot;Database&quot;")
         (incorrect_answers
          ("&quot;Dead End in Tokyo&quot;" "&quot;Raise Your Flag&quot;"
           "&quot;Out of Control&quot;"))))))) |}]
;;

let question_command =
  let open Command.Let_syntax in
  Command.async
    ~summary:"parse a question api to locate "
    (let%map_open () = return () in
     fun () ->
       let%map.Deferred response = get_questions 1 in
       print_s [%message (response : string)];
       let json = Jsonaf.parse response |> Or_error.ok_exn in
       let questions = t_of_jsonaf json in
       print_s [%message (questions : t)])
;;
