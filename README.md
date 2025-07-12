# OCaml Anthropic

OCaml bindings for the [Anthropic API](https://docs.anthropic.com/), providing a type-safe and ergonomic interface to interact with Claude models. This library uses Eio for non-blocking I/O, and supports all major Anthropic API features including streaming responses, tool use, batch processing, and the beta Files API.

## Features

- **Streaming support**: Real-time response streaming with backpressure control
- **Tool use**: Enable Claude to interact with external functions and APIs
- **Batch processing**: Efficiently process multiple requests asynchronously  
- **File uploads (beta)**: Upload and reference files in conversations
- **Automatic retries**: Built-in retry logic with exponential backoff
- **Full model support**: Compatible with all Claude models (Haiku, Sonnet, Opus)

## Installation

```bash
opam install anthropic
```

## Quick Start

```ocaml
open Eio.Std
open Anthropic

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  (* Create client - reads API key from ANTHROPIC_API_KEY env var *)
  let client = create ~sw ~env () in
  
  (* Send a message *)
  let messages = [
    Message.user [Content_block.text "What is OCaml?"]
  ] in
  
  match Messages.create client ~model:`Claude_3_5_Haiku_Latest 
          ~messages ~max_tokens:1000 () with
  | Ok response ->
      List.iter (function
        | Messages.Text_block { text } -> print_endline text
        | _ -> ()) response.content
  | Error err -> 
      Printf.eprintf "Error: %s\n" (string_of_error err)
```

## Usage Examples

### Streaming Responses

```ocaml
(* Stream responses in real-time *)
match Messages.create_stream client ~model:`Claude_3_5_Sonnet_Latest
        ~messages ~max_tokens:1000 () with
| Ok stream ->
    Eio.Stream.iter (function
      | Content_block_delta { delta = `Text text; _ } -> 
          print_string text; flush stdout
      | Message_stop -> print_newline ()
      | _ -> ()) stream
| Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
```

### Using Tools

```ocaml
(* Define a tool *)
let weather_tool = {
  name = "get_weather";
  description = Some "Get the current weather in a location";
  input_schema = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("location", `Assoc [
        ("type", `String "string");
        ("description", `String "City and state")
      ])
    ]);
    ("required", `List [`String "location"])
  ]
} in

(* Let Claude use the tool *)
Messages.create client ~model:`Claude_3_5_Haiku_Latest
  ~messages:[Message.user [Content_block.text "What's the weather in Paris?"]]
  ~tools:[weather_tool]
  ~max_tokens:1000 ()
```

### Batch Processing

```ocaml
(* Process multiple requests efficiently *)
let requests = List.init 100 (fun i -> {
  custom_id = Printf.sprintf "req_%d" i;
  params = `Assoc [
    ("model", `String "claude-3-5-haiku-20241022");
    ("max_tokens", `Int 100);
    ("messages", `List [
      `Assoc [
        ("role", `String "user");
        ("content", `String (Printf.sprintf "Summarize article %d" i))
      ]
    ])
  ]
}) in

match Batches.create client ~requests () with
| Ok batch -> 
    Printf.printf "Batch %s created, processing %d requests\n" 
      batch.id (List.length requests)
| Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
```

### File Uploads (Beta)

```ocaml
(* Upload a file *)
let content = Eio.Flow.string_source "Analysis data..." in
match Beta.Files.upload client ~filename:"data.csv" 
        ~media_type:"text/csv" ~content () with
| Ok file ->
    (* Reference in a message *)
    let messages = [{
      role = `User;
      content = [
        Beta.Messages.Text "Analyze this data:";
        Beta.Messages.File { id = file.id }
      ]
    }] in
    Beta.Messages.create_with_betas client
      ~betas:["files-api-2025-04-14"]
      ~model:`Claude_3_5_Sonnet_Latest
      ~messages ~max_tokens:2000 ()
| Error e -> Error e
```

## Configuration

### Environment Variables
- `ANTHROPIC_API_KEY`: Your Anthropic API key (required if not passed to `create`)

### Client Options
```ocaml
let client = create ~sw ~env 
  ~api_key:"sk-ant-..." 
  ~base_url:"https://api.anthropic.com/v1"
  ~max_retries:5 
  ()
```

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
