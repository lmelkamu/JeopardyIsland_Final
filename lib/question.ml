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
  [@@deriving jsonaf, sexp, hash, compare] [@@jsonaf.allow_extra_fields]
end

module Questions = struct
  type t = { results : Question.t list }
  [@@deriving jsonaf, sexp] [@@jsonaf.allow_extra_fields]
end

(* things we need to get set up: pull from an API ORRRR, we need a sufficient
   question bank with quetsions and plausible answers *)

let parse_label (label : label) : Questions.t =
  Jsonaf.parse label |> Or_error.ok_exn |> Questions.t_of_jsonaf
;;

let get_questions number : Questions.t Deferred.t =
  let%bind _, body =
    Cohttp_async.Client.get
      (Uri.of_string
         (String.append
            "https://opentdb.com/api.php?amount="
            (Int.to_string number)))
  in
  let%bind response = Body.to_string body in
  return (parse_label response)
;;

let is_correct (question : Question.t) (answer : string) : bool =
  String.equal question.correct_answer answer
;;

let question_command =
  let open Command.Let_syntax in
  Command.async
    ~summary:"parse a question api to locate "
    (let%map_open () = return () in
     fun () ->
       let%map.Deferred response = get_questions 1 in
       (* print_s [%message (response : string)]; *)
       List.iter response.results ~f:(fun result ->
         print_s
           [%message
             (result.question : string)
               (result.correct_answer : string)
               (result.incorrect_answers : string list)]))
;;

let%expect_test _ =
  let json_str =
    {|{"response_code":0,"results":[{"category":"General Knowledge","type":"multiple","difficulty":"medium","question":"What is the currency of Poland?","correct_answer":"Z\u0142oty","incorrect_answers":["Ruble","Euro","Krone"]},{"category":"Entertainment: Music","type":"multiple","difficulty":"medium","question":"Which song made by MAN WITH A MISSION was used as the opening for the anime &quot;Log Horizon&quot;?","correct_answer":"&quot;Database&quot;","incorrect_answers":["&quot;Dead End in Tokyo&quot;","&quot;Raise Your Flag&quot;","&quot;Out of Control&quot;"]}]}|}
  in
  let questions = parse_label json_str in
  print_s [%message (questions : Questions.t)];
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
