open Eio.Std
open Anthropic

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let client = create ~sw ~env () in

  let content =
    "Write me a function to call the Anthropic message API in Node.js using \
     the Anthropic Typescript SDK."
  in

  print_endline ("[user]: " ^ content);
  print_string "[assistant]: ";
  flush stdout;

  let messages = [ Message.user [ Content_block.text content ] ] in

  match
    Messages.create_stream client ~max_tokens:1024
      ~model:`Claude_3_5_Sonnet_20240620 ~messages ~stop_sequences:[ "```\n" ]
      ()
  with
  | Ok stream ->
      let rec process_stream () =
        match Eio.Stream.take stream with
        | Messages.Message_stop -> ()
        | Messages.Message_delta { stop_reason = _; usage = _ } as event ->
            (match event with
            | Messages.Message_delta { stop_reason = _; usage = _ } -> ()
            | _ -> ());
            process_stream ()
        | Messages.Content_block_delta { index = _; delta } ->
            (match delta with
            | `Text text ->
                print_string text;
                flush stdout
            | `Input_json _ -> ());
            process_stream ()
        | _ -> process_stream ()
      in
      process_stream ();
      print_endline ""
  | Error err -> print_endline ("\nError: " ^ string_of_error err)
