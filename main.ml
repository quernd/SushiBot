open Lwt.Infix
open Lwt.Syntax

type log_level = [ `Log_error | `Log_info | `Log_debug ]

let log_level = `Log_info
let db_uri = Uri.of_string "sqlite3:bot.db?busy_timeout=60000"
let token = Sys.getenv "TELEGRAM_BOT_TOKEN"

let log' msg =
  match (msg, log_level) with
  | `Error text, _
  | `Info text, `Log_info
  | `Info text, `Log_debug
  | `Debug text, `Log_debug -> Lwt_io.printl text
  | _ -> Lwt.return ()

let with_timeout time f =
  Lwt.pick
    [ (f () >>= fun response -> Lwt.return_ok response)
    ; ( Lwt_unix.sleep time >>= fun () ->
        let* _ = log' @@ `Error "Timeout, closing connection." in
        Lwt.return_error `Timeout )
    ]

module Sushi_bot (Db : Caqti_lwt.CONNECTION) = struct
  let member = Yojson.Basic.Util.member

  module Q = struct
    open Db
    open Caqti_type
    open Caqti_request.Infix

    let select_photo =
      find_opt
        ((unit ->! string)
           "SELECT photo_id FROM photos ORDER BY RANDOM() LIMIT 1")

    let insert_photo =
      exec ((string ->. unit) @@ "INSERT INTO photos (photo_id) VALUES (?)")

    let insert_message =
      exec
        ((tup2 int string ->. unit)
           "INSERT INTO messages (message_id, message) VALUES (?, ?)")

    let insert_update =
      exec
        ((tup2 int int ->. unit)
           "INSERT INTO updates (update_id, message_id) VALUES (?, ?)")

    let select_max_update =
      find_opt ((unit ->! int) "SELECT max(update_id) FROM updates")

    let is_user_authorized =
      find_opt ((int ->! bool) "SELECT authorized FROM users WHERE user_id = ?")
  end

  let is_user_authorized user_id =
    Q.is_user_authorized user_id >>= function
    | Ok (Some true) -> Lwt.return true (* user authorized *)
    | Ok (Some false) (* user unauthorized *) | Ok None (* user unknown *) ->
        Lwt.return false
    | Error err ->
        let* _ = log' @@ `Error (Caqti_error.show err) in
        Lwt.return false

  let api_query method' params =
    Uri.make ~scheme:"https" ~host:"api.telegram.org"
      ~path:(Printf.sprintf "bot%s/%s" token method')
      ~query:params ()
    |> Cohttp_lwt_unix.Client.get

  let log_reply body =
    let* content = body |> Cohttp_lwt.Body.to_string in
    let* _ = log' (`Debug content) in
    let* result =
      match Yojson.Basic.from_string content |> member "ok" with
      | `Bool true -> Lwt.return (`Info "Got 'ok' response.")
      | _ -> Lwt.return (`Error "No 'ok' response!")
    in
    log' result

  let photo_reply chat_id =
    (* Get photo from db and send it to user.
       The photo is actually not a file but an id string of
       a previously received photo. *)
    Q.select_photo () >>= function
    | Ok (Some photo_id) ->
        let* _resp, body =
          api_query "sendPhoto"
            [ ("photo", [ photo_id ]); ("chat_id", [ string_of_int chat_id ]) ]
        in
        log_reply body
    | Ok None -> log' (`Error "No photo found")
    | Error err -> log' (`Error (Caqti_error.show err))

  let text_reply reply chat_id =
    let* _resp, body =
      api_query "sendMessage"
        [ ("text", [ reply ]); ("chat_id", [ string_of_int chat_id ]) ]
    in
    log_reply body

  let reply message =
    let chat = member "chat" message in
    match member "id" chat with
    | `Int chat_id -> (
        match member "photo" message with
        | `Null ->
            let* _ =
              log' (`Info "Message is not a photo, will reply with a photo.")
            in
            photo_reply chat_id
        | `List list -> (
            match List.hd list |> member "file_id" with
            | `String photo_id ->
                let* authorized = is_user_authorized chat_id in
                let* reply =
                  if authorized then
                    let* _ =
                      log'
                      @@ `Info
                           ("Saving photo "
                           ^ photo_id
                           ^ " and replying with a text.")
                    in

                    Q.insert_photo photo_id >>= function
                    | Ok _ ->
                        Lwt.return "Lovely photo!  I saved it in my database."
                    | Error _ ->
                        Lwt.return
                          "Lovely photo!  I tried saving it, but failed."
                  else
                    let* _ = log' (`Info "Replying with a text.") in
                    Lwt.return "Lovely photo, meow!"
                in
                text_reply reply chat_id
            | _ -> raise (Failure "No photo id found."))
        | _ -> raise (Failure "No photo found."))
    | _ -> raise (Failure "No chat id found.")

  let store_message message =
    let open Yojson.Basic in
    let message_id = member "message_id" message |> Util.to_int in
    let message_string = message |> to_string in
    let* _ = Q.insert_message (message_id, message_string) in
    Lwt.return message_id

  let store update =
    let open Yojson.Basic in
    let update_id =
      try member "update_id" update |> Util.to_int with _ -> 0
    in
    let message = member "message" update in
    let* message_id = store_message message in
    let* _ = Q.insert_update (update_id, message_id) in
    Lwt.return message

  let process body =
    let* _ = log' (`Info body) in
    let content = Yojson.Basic.from_string body in
    match member "ok" content with
    | `Bool true -> (
        match member "result" content with
        | `List updates ->
            (* this needs to be map_s instead of map_p because SQLite
               doesn't allow concurrent access and would fail with:
               Failure "Invalid concurrent usage of SQLite connection detected."
            *)
            let* _ =
              updates
              |> Lwt_list.iter_s (fun update ->
                     try store update >>= reply
                     with Failure err -> log' (`Error err))
            in
            Lwt.return ()
        | _ -> Lwt.return ())
    | _ -> Lwt.return ()

  let body () =
    let* offset =
      Q.select_max_update () >>= function
      | Ok o -> Lwt.return o
      | Error _ -> Lwt.return None
    in
    let* response =
      let* _ =
        log'
        @@ `Info
             ("Asking for updates with offset "
             ^ (Option.value ~default:(-1) offset + 1 |> string_of_int))
      in
      (* 60 seconds timeout in case Telegram doesn't reply *)
      with_timeout 60.0 (fun () ->
          api_query "getUpdates"
            (* 55 seconds timeout for long-polling *)
            (("timeout", [ string_of_int 55 ])
            ::
            (match offset with
            | Some o -> [ ("offset", [ string_of_int (o + 1) ]) ]
            | None -> [])))
    in
    match response with
    | Error `Timeout -> Lwt.return ()
    | Ok (resp, body) ->
        let status = Cohttp_lwt.Response.status resp in
        let* _ = log' @@ `Info (Cohttp.Code.string_of_status status) in
        Cohttp_lwt.Body.to_string body >>= process

  let rec loop () =
    let* _ = body () in
    loop ()
end

let body =
  Lwt_main.run
    (let* _ = log' (`Info "SushiBot purr purr meow meow!") in
     Caqti_lwt.connect db_uri >>= function
     | Ok (module Db) ->
         let module Bot = Sushi_bot (Db) in
         Bot.loop ()
     | Error err -> Lwt.return (Error err))
