open Eio.Std
open Anthropic

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let client = create_client ~sw ~env () in

  let content =
    "Write me a function to call the Anthropic message API in Node.js using \
     the Anthropic Typescript SDK."
  in

  print_endline ("[user]: " ^ content);

  let messages = [ Message.user [ Content_block.text content ] ] in

  match
    Messages.send client ~max_tokens:1024 ~model:`Claude_3_5_Sonnet_20240620
      ~messages ~stop_sequences:[ "```\n" ] ()
  with
  | Ok response -> (
      match response.content with
      | [ Messages.Text_block { text } ] ->
          let stop_seq = Option.value response.stop_sequence ~default:"" in
          print_endline ("[assistant]: " ^ text ^ stop_seq)
      | _ -> print_endline "Received unexpected content.")
  | Error err -> print_endline ("Error: " ^ string_of_error err)
