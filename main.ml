open Lwt
open Lwt.Syntax
open Cohttp_lwt_unix

type log_level = [`Log_error | `Log_info | `Log_debug]

let log_level = `Log_info

let db_uri = Uri.of_string "sqlite3:bot.db?busy_timeout=60000"

let token = Sys.getenv "TELEGRAM_BOT_TOKEN"

let log' msg =
  match (msg, log_level) with
  | `Error text, _
  | `Info text, `Log_info | `Info text, `Log_debug
  | `Debug text, `Log_debug -> Lwt_io.printl text
  | _ -> return ()


module Sushi_bot (Db : Caqti_lwt.CONNECTION) = struct

  let member = Yojson.Basic.Util.member

  module Q = struct
    let select_photo = Caqti_request.find_opt
        Caqti_type.unit Caqti_type.string
        "SELECT photo_id FROM photos ORDER BY RANDOM() LIMIT 1"

    let insert_photo = Caqti_request.exec
        Caqti_type.string
        "INSERT INTO photos (photo_id) VALUES (?)"

    let insert_message = Caqti_request.exec
        (Caqti_type.tup2 Caqti_type.int Caqti_type.string)
        "INSERT INTO messages (message_id, message) VALUES (?, ?)"

    let insert_update = Caqti_request.exec
        (Caqti_type.tup2 Caqti_type.int Caqti_type.int)
        "INSERT INTO updates (update_id, message_id) VALUES (?, ?)"

    let select_max_update = Caqti_request.find_opt
        Caqti_type.unit Caqti_type.int
        "SELECT max(update_id) FROM updates"

    let is_user_authorized = Caqti_request.find_opt
        Caqti_type.int Caqti_type.bool
        "SELECT authorized FROM users WHERE id = ?"
  end

  let is_user_authorized chat_id =
    Db.find_opt Q.is_user_authorized chat_id >>= function
    | Ok (Some true) -> return true  (* user authorized *)
    | Ok (Some false)                (* user unauthorized *)
    | Ok None        -> return false (* user unknown *)
    | Error err      -> (let* _ = log' (`Error (Caqti_error.show err)) in
                         return false)

  let api_query method' params =
    Uri.make
      ~scheme:"https"
      ~host:"api.telegram.org"
      ~path:(Printf.sprintf "bot%s/%s" token method')
      ~query:params
      ()
    |> Client.get

  let log_reply body =
    let* content = body |> Cohttp_lwt.Body.to_string in
    let* _ = log' (`Debug content) in
    let* result = 
      match Yojson.Basic.from_string content |> member "ok" with
      | `Bool true -> return (`Info  "Got 'ok' response.")
      | _          -> return (`Error "No 'ok' response!")
    in
    log' result

  let photo_reply chat_id =
    (* Get photo from db and send it to user.
       The photo is actually not a file but an id string of 
       a previously received photo. *)
    Db.find_opt Q.select_photo () >>= function
    | Ok (Some photo_id) ->
      let* (_resp, body) =
        api_query "sendPhoto" [ ("photo",   [ photo_id ])
                              ; ("chat_id", [ string_of_int chat_id ])
                              ]
      in
      log_reply body
    | Ok None   -> log' (`Error "No photo found")
    | Error err -> log' (`Error (Caqti_error.show err))

  let text_reply reply chat_id =
    let* (_resp, body) =
      api_query "sendMessage" [ ("text",    [ reply ])
                              ; ("chat_id", [ string_of_int chat_id ])
                              ]
    in
    log_reply body

  let reply message =
    let chat = member "chat" message in
    match member "id" chat with
    | `Int chat_id ->
      begin match member "photo" message with
        | `Null ->
          let* _ =
            log' (`Info "Message is not a photo, will reply with a photo.")
          in
          photo_reply chat_id
        | `List list -> begin match List.hd list |> member "file_id" with
            | `String photo_id ->
              let* authorized = is_user_authorized chat_id in
              let* reply = if authorized then
                  let* _ =
                    log' (`Info ("Saving photo " ^ photo_id ^
                                 " and replying with a text.")) in

                  Db.exec Q.insert_photo photo_id >>= function
                  | Ok _    ->
                    return "Lovely photo!  I saved it in my database."
                  | Error _ ->
                    return "Lovely photo!  I tried saving it, but failed."
                else
                  let* _ = log' (`Info "Replying with a text.") in
                  return "Lovely photo, meow!"
              in
              text_reply reply chat_id
            | _ -> raise (Failure "No photo id found.")
          end
        | _ -> raise (Failure "No photo found.")
      end
    | _ -> raise (Failure "No chat id found.")

  let store_message message =
    let open Yojson.Basic in
    let message_id = member "message_id" message |> Util.to_int in
    let message_string = message |> to_string in
    let* _ = Db.exec Q.insert_message (message_id, message_string) in
    return message_id

  let store update =
    let open Yojson.Basic in
    let update_id =
      try member "update_id" update |> Util.to_int
      with _ -> 0 in
    let message = member "message" update in
    let* message_id = store_message message in
    let* _ = Db.exec Q.insert_update (update_id, message_id) in
    return message

  let process body =
    let* _ = log' (`Info body) in
    let content = Yojson.Basic.from_string body in
    match member "ok" content with
    | `Bool true -> begin match member "result" content with
        | `List updates ->
          (* this needs to be map_s instead of map_p because SQLite
             doesn't allow concurrent access and would fail with:
             Failure "Invalid concurrent usage of SQLite connection detected."
          *)
          let* _ =
            updates |> Lwt_list.iter_s
              begin
                fun update ->
                  try store update >>= reply
                  with (Failure err) -> log' (`Error err)
              end in
          return ()
        | _ -> return ()
      end
    | _ -> return ()

  let body () =
    let* offset =
      Db.find_opt Q.select_max_update () >>= function
      | Ok o    -> return o
      | Error _ -> return None in
    let* (resp, body) =
      let* _ = log' (`Info ("Asking for updates with offset " ^ (
          Option.value ~default:(-1) offset + 1 |> string_of_int
        ))) in
      (* 10 minutes timeout *)
      api_query "getUpdates"
        (("timeout", [ string_of_int 600 ])::
         match offset with
         | Some o -> [ ("offset", [ string_of_int (o + 1) ]) ]
         | None   -> []
        )
    in
    let status = Cohttp_lwt.Response.status resp in
    let* _ = log' (`Info (Cohttp.Code.string_of_status status)) in
    Cohttp_lwt.Body.to_string body >>= process

  let rec loop () =
    let* _ = body ()
    in loop ()

end


let body = Lwt_main.run
    begin
      let* _ = log' (`Info "SushiBot purr purr meow meow!") in
      Caqti_lwt.connect db_uri >>= function
      | Ok (module Db) ->
        let module Bot = Sushi_bot(Db) in
        Bot.loop ()
      | Error err -> Lwt.return (Error err)
    end
