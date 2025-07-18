open Eio.Std
module Log = (val Logs.src_log (Logs.Src.create "anthropic") : Logs.LOG)

type auth_method = Api_key of string | Bearer_token of string

type client = {
  sw : Eio.Switch.t;
  auth : auth_method;
  base_url : Uri.t;
  cohttp_client : Cohttp_eio.Client.t;
  max_retries : int;
  clock : float Eio.Time.clock_ty Eio.Std.r;
}

type api_error_type =
  | Invalid_request_error
  | Authentication_error
  | Permission_error
  | Not_found_error
  | Rate_limit_error
  | Api_error_other of string

let api_error_type_of_string = function
  | "invalid_request_error" -> Invalid_request_error
  | "authentication_error" -> Authentication_error
  | "permission_error" -> Permission_error
  | "not_found_error" -> Not_found_error
  | "rate_limit_error" -> Rate_limit_error
  | other -> Api_error_other other

let string_of_api_error_type = function
  | Invalid_request_error -> "invalid_request_error"
  | Authentication_error -> "authentication_error"
  | Permission_error -> "permission_error"
  | Not_found_error -> "not_found_error"
  | Rate_limit_error -> "rate_limit_error"
  | Api_error_other s -> s

type api_error = {
  type_ : string; [@key "type"]
  message : string;
  request_id : string option; [@default None]
}
[@@deriving of_yojson { strict = false }]

type error =
  | Api_error of {
      type_ : api_error_type;
      message : string;
      request_id : string option;
    }
  | Http_error of { status : Cohttp.Code.status_code; message : string }
  | Connection_error of exn

let string_of_error = function
  | Api_error { type_; message; _ } ->
      Printf.sprintf "API Error (%s): %s"
        (string_of_api_error_type type_)
        message
  | Http_error { status; message } ->
      Printf.sprintf "HTTP Error (%s): %s"
        (Cohttp.Code.string_of_status status)
        message
  | Connection_error exn ->
      Printf.sprintf "Connection Error: %s" (Printexc.to_string exn)

type 'a page = {
  data : 'a list;
  has_more : bool;
  first_id : string option; [@default None]
  last_id : string option; [@default None]
  get_next_page : unit -> ('a page, error) result option;
}

let page_of_yojson of_item_yojson json =
  let open Yojson.Safe.Util in
  let data =
    member "data" json |> to_list
    |> List.map (fun j ->
           match of_item_yojson j with Ok v -> v | Error e -> failwith e)
  in
  let has_more =
    member "has_more" json |> to_bool_option |> Option.value ~default:false
  in
  let first_id =
    member "first_id" json |> function
    | `Null -> None
    | `String s -> Some s
    | _ -> failwith "invalid first_id"
  in
  let last_id =
    member "last_id" json |> function
    | `Null -> None
    | `String s -> Some s
    | _ -> failwith "invalid last_id"
  in
  Ok { data; has_more; first_id; last_id; get_next_page = (fun () -> None) }

type model =
  [ `Claude_3_7_Sonnet_Latest
  | `Claude_3_7_Sonnet_20250219
  | `Claude_3_5_Haiku_Latest
  | `Claude_3_5_Haiku_20241022
  | `Claude_Sonnet_4_20250514
  | `Claude_Sonnet_4_0
  | `Claude_4_Sonnet_20250514
  | `Claude_3_5_Sonnet_Latest
  | `Claude_3_5_Sonnet_20241022
  | `Claude_3_5_Sonnet_20240620
  | `Claude_Opus_4_0
  | `Claude_Opus_4_20250514
  | `Claude_4_Opus_20250514
  | `Claude_3_Opus_Latest
  | `Claude_3_Opus_20240229
  | `Claude_3_Sonnet_20240229
  | `Claude_3_Haiku_20240307
  | `Claude_2_1
  | `Claude_2_0
  | `Other of string ]

let model_to_string = function
  | `Claude_3_7_Sonnet_Latest -> "claude-3-7-sonnet-latest"
  | `Claude_3_7_Sonnet_20250219 -> "claude-3-7-sonnet-20250219"
  | `Claude_3_5_Haiku_Latest -> "claude-3-5-haiku-latest"
  | `Claude_3_5_Haiku_20241022 -> "claude-3-5-haiku-20241022"
  | `Claude_Sonnet_4_20250514 -> "claude-sonnet-4-20250514"
  | `Claude_Sonnet_4_0 -> "claude-sonnet-4-0"
  | `Claude_4_Sonnet_20250514 -> "claude-4-sonnet-20250514"
  | `Claude_3_5_Sonnet_Latest -> "claude-3-5-sonnet-latest"
  | `Claude_3_5_Sonnet_20241022 -> "claude-3-5-sonnet-20241022"
  | `Claude_3_5_Sonnet_20240620 -> "claude-3-5-sonnet-20240620"
  | `Claude_Opus_4_0 -> "claude-opus-4-0"
  | `Claude_Opus_4_20250514 -> "claude-opus-4-20250514"
  | `Claude_4_Opus_20250514 -> "claude-4-opus-20250514"
  | `Claude_3_Opus_Latest -> "claude-3-opus-latest"
  | `Claude_3_Opus_20240229 -> "claude-3-opus-20240229"
  | `Claude_3_Sonnet_20240229 -> "claude-3-sonnet-20240229"
  | `Claude_3_Haiku_20240307 -> "claude-3-haiku-20240307"
  | `Claude_2_1 -> "claude-2.1"
  | `Claude_2_0 -> "claude-2.0"
  | `Other s -> s

type role = [ `User | `Assistant ]

let role_to_yojson = function
  | `User -> `String "user"
  | `Assistant -> `String "assistant"

let role_of_yojson = function
  | `String "user" -> Ok `User
  | `String "assistant" -> Ok `Assistant
  | _ -> Error "Invalid role"

type input_content_block =
  | Text of string
  | Image of { media_type : string; data : string }
  | Document of { name : string; content : string; media_type : string }
  | Tool_result of {
      tool_use_id : string;
      content : string;
      is_error : bool option;
    }
  | Tool_use of { id : string; name : string; input : Yojson.Safe.t }

let input_content_block_to_yojson = function
  | Text text -> `Assoc [ ("type", `String "text"); ("text", `String text) ]
  | Image { media_type; data } ->
      `Assoc
        [
          ("type", `String "image");
          ( "source",
            `Assoc
              [
                ("type", `String "base64");
                ("media_type", `String media_type);
                ("data", `String data);
              ] );
        ]
  | Document { name; content; media_type } ->
      `Assoc
        [
          ("type", `String "document");
          ("name", `String name);
          ( "source",
            `Assoc
              [
                ("type", `String "base64");
                ("media_type", `String media_type);
                ("data", `String content);
              ] );
        ]
  | Tool_result { tool_use_id; content; is_error } -> (
      let base =
        [
          ("type", `String "tool_result");
          ("tool_use_id", `String tool_use_id);
          ("content", `String content);
        ]
      in
      match is_error with
      | Some b -> `Assoc (("is_error", `Bool b) :: base)
      | None -> `Assoc base)
  | Tool_use { id; name; input } ->
      `Assoc
        [
          ("type", `String "tool_use");
          ("id", `String id);
          ("name", `String name);
          ("input", input);
        ]

module Content_block = struct
  let text s = Text s
  let image ~media_type ~data = Image { media_type; data }

  let document ~name ~content ~media_type =
    Document { name; content; media_type }

  let tool_result ~tool_use_id ~content ?is_error () =
    Tool_result
      { tool_use_id; content = Yojson.Safe.to_string content; is_error }

  let tool_use ~id ~name ~input = Tool_use { id; name; input }
end

type message = { role : role; content : input_content_block list }

let message_to_yojson msg =
  `Assoc
    [
      ("role", role_to_yojson msg.role);
      ("content", `List (List.map input_content_block_to_yojson msg.content));
    ]

module Message = struct
  let user content = { role = `User; content }
  let assistant content = { role = `Assistant; content }
end

type tool = {
  name : string;
  description : string option; [@default None]
  input_schema : Yojson.Safe.t; [@key "input_schema"]
  type_ : string option; [@key "type"] [@default None]
  cache_control : Yojson.Safe.t option; [@default None]
}
[@@deriving to_yojson { strict = false }]

type tool_choice = Auto | Any | Tool of string | Tool_none

let tool_choice_to_yojson = function
  | Auto -> `Assoc [ ("type", `String "auto") ]
  | Any -> `Assoc [ ("type", `String "any") ]
  | Tool name -> `Assoc [ ("type", `String "tool"); ("name", `String name) ]
  | Tool_none -> `Assoc [ ("type", `String "none") ]

type metadata = { user_id : string option [@default None] [@key "user_id"] }
[@@deriving to_yojson { strict = false }]

let default_https ~authenticator =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> failwith ("TLS configuration error: " ^ msg)
    | Ok config -> config
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let create_client ~sw ~env ?api_key ?auth_token
    ?(base_url = "https://api.anthropic.com/v1") ?(max_retries = 2) () =
  Log.debug (fun m -> m "Creating Anthropic client with base URL: %s" base_url);
  let auth =
    match (api_key, auth_token) with
    | Some key, None ->
        Log.debug (fun m -> m "Using provided API key");
        Api_key key
    | None, Some token ->
        Log.debug (fun m -> m "Using provided OAuth token");
        Bearer_token token
    | None, None -> (
        match Sys.getenv_opt "ANTHROPIC_API_KEY" with
        | Some key ->
            Log.debug (fun m ->
                m "Using API key from ANTHROPIC_API_KEY environment variable");
            Api_key key
        | None ->
            Log.err (fun m -> m "No authentication provided");
            raise
              (Invalid_argument
                 "Authentication must be provided either via 'api_key', \
                  'auth_token', or the ANTHROPIC_API_KEY environment variable"))
    | Some _, Some _ ->
        raise
          (Invalid_argument "Cannot provide both 'api_key' and 'auth_token'")
  in
  (* Initialize RNG for TLS - this is idempotent so safe to call multiple times *)
  Mirage_crypto_rng_unix.use_default ();
  (* Create authenticator for TLS *)
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok auth -> auth
    | Error (`Msg msg) ->
        failwith ("Failed to create X509 authenticator: " ^ msg)
  in
  {
    sw;
    auth;
    base_url = Uri.of_string base_url;
    cohttp_client =
      Cohttp_eio.Client.make
        ~https:(Some (default_https ~authenticator))
        env#net;
    max_retries;
    clock = env#clock;
  }

module Api_helpers = struct
  let build_auth_headers auth =
    match auth with
    | Api_key key -> Cohttp.Header.init_with "x-api-key" key
    | Bearer_token token ->
        Cohttp.Header.init_with "Authorization" ("Bearer " ^ token)

  let base_headers auth =
    let auth_headers = build_auth_headers auth in
    let base_list =
      [
        ("content-type", "application/json"); ("anthropic-version", "2023-06-01");
      ]
    in
    let headers_list =
      match auth with
      | Bearer_token _ -> ("anthropic-beta", "oauth-2025-04-20") :: base_list
      | Api_key _ -> base_list
    in
    Cohttp.Header.add_list auth_headers headers_list

  let add_beta_headers headers betas =
    List.fold_left
      (fun h (name, value) -> Cohttp.Header.add h name value)
      headers betas

  let is_retryable = function
    | Http_error { status; _ } -> (
        match status with
        | `Too_many_requests | `Request_timeout | `Internal_server_error
        | `Bad_gateway | `Service_unavailable | `Gateway_timeout ->
            true
        | _ -> false)
    | Connection_error _ -> true
    | Api_error { type_ = Api_error_other "overloaded_error"; _ } -> true
    | Api_error _ -> false

  let get_retry_after_delay headers =
    match
      ( Cohttp.Header.get headers "retry-after-ms",
        Cohttp.Header.get headers "retry-after" )
    with
    | Some ms_str, _ ->
        Option.map
          (fun ms -> float_of_int ms /. 1000.0)
          (int_of_string_opt ms_str)
    | None, Some s_str -> int_of_string_opt s_str |> Option.map float_of_int
    | None, None -> None

  let rec request client ~meth ~path ~headers ?body ~retries_left ~current_delay
      () =
    Log.debug (fun m ->
        m "Making %s request to %s (retries left: %d)"
          (Cohttp.Code.string_of_method meth)
          path retries_left);
    let headers_with_retry =
      Cohttp.Header.add headers "x-stainless-retry-count"
        (string_of_int (client.max_retries - retries_left))
    in
    let do_request () =
      let base_path = Uri.path client.base_url in
      let full_path =
        if String.ends_with ~suffix:"/" base_path then
          base_path ^ String.sub path 1 (String.length path - 1)
        else base_path ^ path
      in
      let uri = Uri.with_path client.base_url full_path in
      try
        Log.debug (fun m -> m "Sending request to URI: %s" (Uri.to_string uri));
        Ok
          (Cohttp_eio.Client.call client.cohttp_client ~sw:client.sw
             ~headers:headers_with_retry ?body meth uri)
      with exn ->
        Log.err (fun m -> m "Connection error: %s" (Printexc.to_string exn));
        Error (Connection_error exn)
    in
    match do_request () with
    | Ok (resp, body_stream) -> (
        let status = Cohttp.Response.status resp in
        Log.debug (fun m ->
            m "Received response with status: %s"
              (Cohttp.Code.string_of_status status));
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
          Ok (resp, body_stream)
        else
          let error_body =
            Eio.Buf_read.of_flow ~max_size:max_int body_stream
            |> Eio.Buf_read.take_all
          in
          Log.warn (fun m -> m "Error response body: %s" error_body);
          let err =
            try
              let json = Yojson.Safe.from_string error_body in
              match
                Yojson.Safe.Util.member "error" json |> api_error_of_yojson
              with
              | Ok { type_; message; request_id } ->
                  let error_type = api_error_type_of_string type_ in
                  Log.err (fun m ->
                      m "API error (%s): %s (request_id: %s)" type_ message
                        (Option.value request_id ~default:"none"));
                  Error (Api_error { type_ = error_type; message; request_id })
              | Error _ ->
                  Log.err (fun m ->
                      m "HTTP error %s: %s"
                        (Cohttp.Code.string_of_status status)
                        error_body);
                  Error (Http_error { status; message = error_body })
            with _ ->
              Log.err (fun m -> m "Failed to parse error response");
              Error (Http_error { status; message = error_body })
          in
          match err with
          | Error e when is_retryable e && retries_left > 0 ->
              let delay =
                Option.value ~default:current_delay
                  (get_retry_after_delay (Cohttp.Response.headers resp))
              in
              Log.info (fun m ->
                  m "Retrying request to %s in %.2fs (%d retries left)" path
                    delay retries_left);
              Eio.Time.sleep client.clock delay;
              request client ~meth ~path ~headers ?body
                ~retries_left:(retries_left - 1) ~current_delay:(delay *. 1.5)
                ()
          | error_result -> error_result)
    | Error e when is_retryable e && retries_left > 0 ->
        Log.info (fun m ->
            m
              "Retrying request to %s due to connection error in %.2fs (%d \
               retries left)"
              path current_delay retries_left);
        Eio.Time.sleep client.clock current_delay;
        request client ~meth ~path ~headers ?body
          ~retries_left:(retries_left - 1) ~current_delay:(current_delay *. 1.5)
          ()
    | Error e -> Error e

  let request_json client ~meth ~path ?(betas = []) ?body of_yojson =
    let headers = add_beta_headers (base_headers client.auth) betas in
    let body = Option.map Cohttp_eio.Body.of_string body in
    match
      request client ~meth ~path ~headers ?body ~retries_left:client.max_retries
        ~current_delay:0.5 ()
    with
    | Ok (resp, body_stream) -> (
        let body_str =
          Eio.Buf_read.of_flow ~max_size:max_int body_stream
          |> Eio.Buf_read.take_all
        in
        try
          match Yojson.Safe.from_string body_str |> of_yojson with
          | Ok data -> Ok data
          | Error msg ->
              Error
                (Http_error
                   {
                     status = Cohttp.Response.status resp;
                     message = "JSON parsing error: " ^ msg;
                   })
        with Yojson.Json_error msg ->
          Error
            (Http_error
               {
                 status = Cohttp.Response.status resp;
                 message = "Invalid JSON response: " ^ msg;
               }))
    | Error e -> Error e

  let request_stream client ~meth ~path ~headers ?body () =
    let body = Option.map Cohttp_eio.Body.of_string body in
    request client ~meth ~path ~headers ?body ~retries_left:client.max_retries
      ~current_delay:0.5 ()

  let rec make_paginated_list_request client ~path_prefix of_yojson
      ?(betas = []) ?(limit = 20) ?after_id () =
    let query = [ ("limit", string_of_int limit) ] in
    let query =
      match after_id with Some id -> ("after_id", id) :: query | None -> query
    in
    let path =
      Uri.add_query_params' (Uri.of_string path_prefix) query
      |> Uri.path_and_query
    in

    let list_call () =
      request_json client ~meth:`GET ~path ~betas (page_of_yojson of_yojson)
    in

    match list_call () with
    | Ok page ->
        let get_next_page () =
          if page.has_more then
            match page.last_id with
            | Some id ->
                Some
                  (make_paginated_list_request client ~path_prefix of_yojson
                     ~betas ~limit ~after_id:id ())
            | None -> None
          else None
        in
        Ok { page with get_next_page }
    | Error e -> Error e
end

module Messages = struct
  type response_content_block =
    | Text_block of { text : string }
    | Tool_use_block of { id : string; name : string; input : Yojson.Safe.t }
    | Thinking_block of { id : string; name : string }
    | Redacted_thinking_block of { id : string; name : string }

  let response_content_block_of_yojson json =
    let open Yojson.Safe.Util in
    try
      match member "type" json |> to_string with
      | "text" ->
          let text = member "text" json |> to_string in
          Ok (Text_block { text })
      | "tool_use" ->
          let id = member "id" json |> to_string in
          let name = member "name" json |> to_string in
          let input =
            match member "input" json with
            | `Null -> `Assoc [] (* Initialize as empty object *)
            | input -> input
          in
          Ok (Tool_use_block { id; name; input })
      | "thinking" ->
          let id = member "id" json |> to_string in
          let name = member "name" json |> to_string in
          Ok (Thinking_block { id; name })
      | "redacted_thinking" ->
          let id = member "id" json |> to_string in
          let name = member "name" json |> to_string in
          Ok (Redacted_thinking_block { id; name })
      | t -> Error ("Unknown content block type: " ^ t)
    with
    | Type_error (msg, _) -> Error ("JSON parsing error: " ^ msg)
    | e -> Error (Printexc.to_string e)

  type server_tool_usage = { web_search_requests : int }
  [@@deriving of_yojson { strict = false }]

  type service_tier = [ `Standard | `Priority | `Batch ]

  let service_tier_of_yojson = function
    | `String "standard" -> Ok `Standard
    | `String "priority" -> Ok `Priority
    | `String "batch" -> Ok `Batch
    | _ -> Error "Invalid service_tier"

  type usage = {
    input_tokens : int;
    output_tokens : int;
    cache_creation_input_tokens : int option; [@default None]
    cache_read_input_tokens : int option; [@default None]
    server_tool_use : server_tool_usage option; [@default None]
    service_tier : service_tier option; [@default None]
  }
  [@@deriving of_yojson { strict = false }]

  (* Message delta events have a different usage structure *)
  type delta_usage = {
    input_tokens : int; [@default 0]
    output_tokens : int; [@default 0]
    cache_creation_input_tokens : int; [@default 0]
    cache_read_input_tokens : int; [@default 0]
  }
  [@@deriving of_yojson { strict = false }]

  type stop_reason =
    [ `End_turn | `Max_tokens | `Stop_sequence | `Tool_use | `Other of string ]

  let stop_reason_of_yojson = function
    | `String "end_turn" -> Ok `End_turn
    | `String "max_tokens" -> Ok `Max_tokens
    | `String "stop_sequence" -> Ok `Stop_sequence
    | `String "tool_use" -> Ok `Tool_use
    | `String other -> Ok (`Other other)
    | _ -> Error "Invalid stop_reason"

  type response = {
    id : string;
    type_ : string; [@key "type"]
    model : string;
    role : role;
    stop_reason : stop_reason option; [@default None]
    stop_sequence : string option; [@default None]
    content : response_content_block list;
    usage : usage;
  }
  [@@deriving of_yojson { strict = false }]

  type stream_event =
    | Message_start of response
    | Content_block_start of { index : int; content : response_content_block }
    | Content_block_delta of {
        index : int;
        delta : [ `Text of string | `Input_json of string ];
      }
    | Content_block_stop of { index : int }
    | Message_delta of {
        stop_reason :
          [ `End_turn
          | `Max_tokens
          | `Stop_sequence
          | `Tool_use
          | `Other of string ];
        usage : delta_usage;
      }
    | Message_stop
    | Ping

  let create_request_body ~model ~messages ?max_tokens ?temperature ?top_k
      ?top_p ?stop_sequences ?system ?tools ?tool_choice ?metadata
      ?(stream = false) () =
    let body =
      [
        ("model", `String (model_to_string model));
        ("messages", `List (List.map message_to_yojson messages));
      ]
    in
    let add_opt k v f l = match v with Some x -> (k, f x) :: l | None -> l in
    let body = add_opt "max_tokens" max_tokens (fun i -> `Int i) body in
    let body = add_opt "temperature" temperature (fun f -> `Float f) body in
    let body =
      add_opt "stop_sequences" stop_sequences
        (fun ss -> `List (List.map (fun s -> `String s) ss))
        body
    in
    let body = add_opt "system" system (fun s -> `String s) body in
    let body =
      add_opt "tools" tools (fun ts -> `List (List.map tool_to_yojson ts)) body
    in
    let body = add_opt "top_k" top_k (fun i -> `Int i) body in
    let body = add_opt "top_p" top_p (fun f -> `Float f) body in
    let body = add_opt "tool_choice" tool_choice tool_choice_to_yojson body in
    let body = add_opt "metadata" metadata metadata_to_yojson body in
    let body = if stream then ("stream", `Bool true) :: body else body in
    `Assoc body

  let send client ?max_tokens ?temperature ?top_k ?top_p ?stop_sequences ?system
      ?tools ?tool_choice ?metadata ~model ~messages () =
    Log.info (fun m ->
        m "Sending message with model %s, %d messages" (model_to_string model)
          (List.length messages));
    let body_json =
      create_request_body ~model ~messages ?max_tokens ?temperature ?top_k
        ?top_p ?stop_sequences ?system ?tools ?tool_choice ?metadata ()
    in
    Api_helpers.request_json client ~meth:`POST ~path:"/messages"
      ~body:(Yojson.Safe.to_string body_json)
      response_of_yojson

  let parse_stream_event event data =
    let json = Yojson.Safe.from_string data in
    let open Yojson.Safe.Util in
    try
      match event with
      | "message_start" -> (
          (* For streaming, message_start might have incomplete fields - just parse what we can *)
          try
            let msg_json = member "message" json in
            let id = member "id" msg_json |> to_string in
            let model = member "model" msg_json |> to_string in
            let role =
              member "role" msg_json |> to_string |> function
              | "user" -> `User
              | "assistant" -> `Assistant
              | _ -> failwith "Invalid role"
            in
            (* Create a minimal response for message_start *)
            let response =
              {
                id;
                type_ = "message";
                model;
                role;
                stop_reason = None;
                (* Default for streaming *)
                stop_sequence = None;
                content = [];
                usage =
                  {
                    input_tokens = 0;
                    output_tokens = 0;
                    cache_creation_input_tokens = None;
                    cache_read_input_tokens = None;
                    server_tool_use = None;
                    service_tier = None;
                  };
              }
            in
            Ok (Message_start response)
          with exn ->
            Error ("Failed to parse message_start: " ^ Printexc.to_string exn))
      | "content_block_start" -> (
          match
            member "content_block" json |> response_content_block_of_yojson
          with
          | Ok content ->
              Ok
                (Content_block_start
                   { index = member "index" json |> to_int; content })
          | Error e -> Error ("Failed to parse content_block: " ^ e))
      | "content_block_delta" ->
          let index = member "index" json |> to_int in
          let delta_json = member "delta" json in
          let delta_type = member "type" delta_json |> to_string in
          let delta =
            match delta_type with
            | "text_delta" -> `Text (member "text" delta_json |> to_string)
            | "input_json_delta" ->
                `Input_json (member "partial_json" delta_json |> to_string)
            | _ ->
                raise (Yojson.Json_error ("Unknown delta type: " ^ delta_type))
          in
          Ok (Content_block_delta { index; delta })
      | "content_block_stop" ->
          Ok (Content_block_stop { index = member "index" json |> to_int })
      | "message_delta" -> (
          let delta_json = member "delta" json in
          match member "stop_reason" delta_json |> stop_reason_of_yojson with
          | Ok stop_reason -> (
              match member "usage" json |> delta_usage_of_yojson with
              | Ok usage -> Ok (Message_delta { stop_reason; usage })
              | Error e -> Error ("Failed to parse usage: " ^ e))
          | Error e -> Error ("Failed to parse stop_reason: " ^ e))
      | "message_stop" -> Ok Message_stop
      | "ping" -> Ok Ping
      | "error" ->
          (* Handle error events from the stream *)
          let error_obj = member "error" json in
          let error_type =
            member "type" error_obj |> to_string_option
            |> Option.value ~default:"unknown"
          in
          let error_msg =
            member "message" error_obj |> to_string_option
            |> Option.value ~default:"Unknown error"
          in
          Error (Printf.sprintf "Stream error: %s - %s" error_type error_msg)
      | _ -> Error ("Unknown event type: " ^ event)
    with Yojson.Json_error msg ->
      Error ("JSON parsing error in stream: " ^ msg)

  let send_stream client ?max_tokens ?temperature ?top_k ?top_p ?stop_sequences
      ?system ?tools ?tool_choice ?metadata ~model ~messages () =
    Log.info (fun m ->
        m "Creating streaming message with model %s, %d messages"
          (model_to_string model) (List.length messages));
    let body_json =
      create_request_body ~model ~messages ?max_tokens ?temperature ?top_k
        ?top_p ?stop_sequences ?system ?tools ?tool_choice ?metadata
        ~stream:true ()
    in
    let body_str = Yojson.Safe.to_string body_json in
    let headers = Api_helpers.base_headers client.auth in

    match
      Api_helpers.request_stream client ~meth:`POST ~path:"/messages" ~headers
        ~body:body_str ()
    with
    | Ok (_resp, body) ->
        (* Success case - stream handling *)
        Log.debug (fun m -> m "Successfully started message stream");
        let stream = Eio.Stream.create 16 in
        let fiber () =
          try
            let rd = Eio.Buf_read.of_flow ~max_size:max_int body in
            let event_buffer = Buffer.create 16 in
            let data_buffer = Buffer.create 1024 in
            try
              while true do
                let line = Eio.Buf_read.line rd in
                if line = "" then (
                  let event = Buffer.contents event_buffer in
                  let data = Buffer.contents data_buffer in
                  Buffer.clear event_buffer;
                  Buffer.clear data_buffer;
                  if event <> "" then
                    match parse_stream_event event data with
                    | Ok ev -> Eio.Stream.add stream ev
                    | Error e ->
                        Log.warn (fun m -> m "Stream parse error: %s" e);
                        (* If it's a stream error event, we should stop the stream *)
                        if String.starts_with ~prefix:"Stream error:" e then
                          Eio.Stream.add stream Message_stop)
                else
                  match String.split_on_char ':' line with
                  | "event" :: rest ->
                      Buffer.add_string event_buffer
                        (String.trim (String.concat ":" rest))
                  | "data" :: rest ->
                      Buffer.add_string data_buffer
                        (String.trim (String.concat ":" rest))
                  | _ -> () (* Ignore other fields like 'id' and comments *)
              done
            with End_of_file -> Log.debug (fun m -> m "Stream ended normally")
          with exn ->
            Log.err (fun m ->
                m "Streaming fiber exited with: %s" (Printexc.to_string exn));
            Eio.Stream.add stream Message_stop
        in
        Fiber.fork ~sw:client.sw fiber;
        Ok stream
    | Error e -> Error e

  let iter_stream client ?max_tokens ?temperature ?top_k ?top_p ?stop_sequences
      ?system ?tools ?tool_choice ?metadata ~model ~messages ~on_event ~on_error
      () =
    match
      send_stream client ?max_tokens ?temperature ?top_k ?top_p ?stop_sequences
        ?system ?tools ?tool_choice ?metadata ~model ~messages ()
    with
    | Ok stream ->
        let rec loop () =
          match Eio.Stream.take stream with
          | Message_stop -> ()
          | event ->
              on_event event;
              loop ()
        in
        loop ()
    | Error err -> on_error err

  let accumulate_stream stream =
    let response_ref = ref None in
    let content_blocks = ref [] in
    let tool_use_deltas = Hashtbl.create 4 in
    (* Maps index to partial JSON string *)
    try
      let rec process_events () =
        match Eio.Stream.take stream with
        | Message_start response ->
            response_ref := Some response;
            process_events ()
        | Content_block_start { index; content } ->
            let current = Array.of_list !content_blocks in
            if Array.length current <= index then
              content_blocks := List.append !content_blocks [ content ]
            else (
              current.(index) <- content;
              content_blocks := Array.to_list current);
            process_events ()
        | Content_block_delta { index; delta } ->
            let blocks = Array.of_list !content_blocks in
            (if index >= 0 && index < Array.length blocks then
               match (blocks.(index), delta) with
               | Text_block tb, `Text delta_text ->
                   blocks.(index) <- Text_block { text = tb.text ^ delta_text }
               | Tool_use_block _, `Input_json delta_json ->
                   (* Just append the raw string fragment to the hashtable *)
                   let current =
                     Hashtbl.find_opt tool_use_deltas index
                     |> Option.value ~default:""
                   in
                   Hashtbl.replace tool_use_deltas index (current ^ delta_json)
               | _ -> () (* Ignore mismatched delta types *));
            content_blocks := Array.to_list blocks;
            process_events ()
        | Message_delta { stop_reason; usage } ->
            (match !response_ref with
            | Some r ->
                (* Convert delta_usage to usage *)
                let cache_creation =
                  if usage.cache_creation_input_tokens = 0 then None
                  else Some usage.cache_creation_input_tokens
                in
                let cache_read =
                  if usage.cache_read_input_tokens = 0 then None
                  else Some usage.cache_read_input_tokens
                in
                let updated_usage : usage =
                  {
                    input_tokens = usage.input_tokens;
                    output_tokens = usage.output_tokens;
                    cache_creation_input_tokens = cache_creation;
                    cache_read_input_tokens = cache_read;
                    server_tool_use = None;
                    service_tier = None;
                  }
                in
                response_ref :=
                  Some
                    {
                      r with
                      stop_reason = Some stop_reason;
                      usage = updated_usage;
                    }
            | None -> ());
            process_events ()
        | Content_block_stop _ | Ping -> process_events ()
        | Message_stop -> ()
      in
      process_events ();

      (* Parse accumulated JSON strings in tool_use blocks *)
      let finalized_blocks =
        List.mapi
          (fun i block ->
            match block with
            | Tool_use_block tu -> (
                match Hashtbl.find_opt tool_use_deltas i with
                | Some json_str ->
                    let parsed_input =
                      try Yojson.Safe.from_string json_str
                      with _ ->
                        `Assoc [] (* Default to empty object on parse error *)
                    in
                    Tool_use_block { tu with input = parsed_input }
                | None -> block)
            | _ -> block)
          !content_blocks
      in
      match !response_ref with
      | Some response -> Ok { response with content = finalized_blocks }
      | None ->
          Error
            (Connection_error
               (Failure "Stream ended without message_start event"))
    with exn -> Error (Connection_error exn)

  (* Message Building Helpers *)
  let user text = { role = `User; content = [ Text text ] }
  let assistant text = { role = `Assistant; content = [ Text text ] }
  let user_with_content content = { role = `User; content }
  let assistant_with_content content = { role = `Assistant; content }

  let tool_result_message ~tool_use_id ~content ?(is_error = false) () =
    {
      role = `User;
      content =
        [
          Tool_result
            {
              tool_use_id;
              content = Yojson.Safe.to_string content;
              is_error = Some is_error;
            };
        ];
    }

  (* Content Extraction Helpers *)
  let extract_text blocks =
    List.find_map
      (function Text_block { text } -> Some text | _ -> None)
      blocks

  let extract_all_text blocks =
    List.filter_map
      (function Text_block { text } -> Some text | _ -> None)
      blocks

  let find_tool_use blocks =
    List.find_map
      (function
        | Tool_use_block { id; name; input } -> Some (id, name, input)
        | _ -> None)
      blocks

  let response_content_to_input = function
    | Text_block { text } -> Text text
    | Tool_use_block { id; name; input } -> Tool_use { id; name; input }
    | Thinking_block _ | Redacted_thinking_block _ ->
        failwith "Cannot convert thinking blocks to input content"

  let response_to_input_content blocks =
    List.map response_content_to_input blocks
end

module Tools = struct
  type tool_execution_result = Success of Yojson.Safe.t | Error of string

  let make_tool_result ~tool_use_id ~result =
    let content, is_error =
      match result with
      | Success json -> (Yojson.Safe.to_string json, false)
      | Error err ->
          let error_json = `Assoc [ ("error", `String err) ] in
          (Yojson.Safe.to_string error_json, true)
    in
    Tool_result { tool_use_id; content; is_error = Some is_error }

  let requires_tool_execution blocks =
    List.exists
      (function Messages.Tool_use_block _ -> true | _ -> false)
      blocks

  let extract_tool_calls blocks =
    List.filter_map
      (function
        | Messages.Tool_use_block { id; name; input } -> Some (id, name, input)
        | _ -> None)
      blocks
end

module Models = struct
  type t = {
    id : string;
    created_at : string;
    display_name : string;
    type_ : string; [@key "type"]
  }
  [@@deriving of_yojson { strict = false }]

  let get client ~model_id () =
    Api_helpers.request_json client ~meth:`GET ~path:("/models/" ^ model_id)
      of_yojson

  let list client ?limit ?after_id () =
    Api_helpers.make_paginated_list_request client ~path_prefix:"/models"
      of_yojson ?limit ?after_id ()
end

module Batches = struct
  (* Beta header required for Batches API *)
  let beta_header = ("anthropic-beta", "message-batches-2024-09-24")

  type status = [ `In_progress | `Canceling | `Ended ]

  let status_of_yojson = function
    | `String "in_progress" -> Ok `In_progress
    | `String "canceling" -> Ok `Canceling
    | `String "ended" -> Ok `Ended
    | _ -> Error "invalid processing_status"

  type request_counts = {
    canceled : int;
    errored : int;
    expired : int;
    processing : int;
    succeeded : int;
  }
  [@@deriving of_yojson { strict = false }]

  type t = {
    id : string;
    type_ : string; [@key "type"]
    processing_status : status;
    request_counts : request_counts;
    created_at : string;
    expires_at : string;
    ended_at : string option; [@default None]
    cancel_initiated_at : string option; [@default None]
    archived_at : string option; [@default None]
    results_url : string option;
        [@of_yojson
          fun json ->
            match json with
            | `Null -> Ok None
            | `String s -> Ok (Some s)
            | _ -> Error "expected string or null"]
  }
  [@@deriving of_yojson { strict = false }]

  type request = { custom_id : string; params : Yojson.Safe.t }

  type individual_response = {
    custom_id : string;
    result : [ `Succeeded of Messages.response | `Errored of api_error ];
  }

  (* Use the generic page_of_yojson function defined above *)

  let submit client ~(requests : request list) () =
    Log.info (fun m ->
        m "Submitting batch with %d requests" (List.length requests));
    let body_json =
      `Assoc
        [
          ( "requests",
            `List
              (List.map
                 (fun (req : request) ->
                   `Assoc
                     [
                       ("custom_id", `String req.custom_id);
                       ("params", req.params);
                     ])
                 requests) );
        ]
    in
    Api_helpers.request_json client ~meth:`POST ~path:"/messages/batches"
      ~betas:[ beta_header ]
      ~body:(Yojson.Safe.to_string body_json)
      of_yojson

  let get client ~batch_id () =
    Api_helpers.request_json client ~meth:`GET
      ~path:("/messages/batches/" ^ batch_id)
      ~betas:[ beta_header ] of_yojson

  let list client ?limit ?after_id () =
    Api_helpers.make_paginated_list_request client
      ~path_prefix:"/messages/batches" ~betas:[ beta_header ] of_yojson ?limit
      ?after_id ()

  let cancel client ~batch_id () =
    Api_helpers.request_json client ~meth:`POST
      ~path:("/messages/batches/" ^ batch_id ^ "/cancel")
      ~betas:[ beta_header ] of_yojson

  let results client ~batch_id () =
    let path = "/messages/batches/" ^ batch_id ^ "/results" in
    let headers =
      Api_helpers.add_beta_headers
        (Api_helpers.base_headers client.auth)
        [ beta_header ]
    in
    match Api_helpers.request_stream client ~meth:`GET ~path ~headers () with
    | Error e -> Error e
    | Ok (_resp, body) ->
        let stream = Eio.Stream.create 16 in
        let fiber () =
          try
            let rd = Eio.Buf_read.of_flow ~max_size:max_int body in
            try
              while true do
                let line = Eio.Buf_read.line rd in
                if line <> "" then
                  try
                    let json = Yojson.Safe.from_string line in
                    let open Yojson.Safe.Util in
                    let custom_id = member "custom_id" json |> to_string in
                    let result_json = member "result" json in
                    let result_type = member "type" result_json |> to_string in
                    let result =
                      match result_type with
                      | "succeeded" -> (
                          match
                            member "message" result_json
                            |> Messages.response_of_yojson
                          with
                          | Ok msg -> `Succeeded msg
                          | Error e ->
                              `Errored
                                {
                                  type_ = "parsing_error";
                                  message = "Failed to parse message: " ^ e;
                                  request_id = None;
                                })
                      | "errored" -> (
                          match
                            member "error" result_json |> api_error_of_yojson
                          with
                          | Ok err -> `Errored err
                          | Error e ->
                              `Errored
                                {
                                  type_ = "parsing_error";
                                  message = "Failed to parse error: " ^ e;
                                  request_id = None;
                                })
                      | "canceled" | "expired" ->
                          `Errored
                            {
                              type_ = result_type;
                              message = "Batch item was " ^ result_type;
                              request_id = None;
                            }
                      | _ ->
                          `Errored
                            {
                              type_ = "unknown";
                              message = "Unknown result type: " ^ result_type;
                              request_id = None;
                            }
                    in
                    Eio.Stream.add stream { custom_id; result }
                  with _ -> ()
              done
            with End_of_file -> ()
          with exn ->
            Log.err (fun m ->
                m "Results stream exited with: %s" (Printexc.to_string exn))
        in
        Fiber.fork ~sw:client.sw fiber;
        Ok stream
end

module Beta = struct
  module Files = struct
    type t = {
      id : string;
      type_ : string; [@key "type"]
      filename : string;
      mime_type : string;
      size_bytes : int;
      created_at : string;
      downloadable : bool option; [@default None]
    }
    [@@deriving of_yojson { strict = false }]

    type deleted = { id : string } [@@deriving of_yojson { strict = false }]

    let beta_header = ("anthropic-beta", "files-api-2025-04-14")

    let upload client ~filename ~media_type ~content () =
      Log.info (fun m ->
          m "Uploading file: %s (media_type: %s)" filename media_type);
      Random.self_init ();
      let boundary =
        "---------------------------" ^ string_of_int (Random.bits ())
      in
      let auth_header =
        match client.auth with
        | Api_key key -> ("x-api-key", key)
        | Bearer_token token -> ("Authorization", "Bearer " ^ token)
      in
      let beta_headers =
        match client.auth with
        | Bearer_token _ ->
            [ ("anthropic-beta", "files-api-2025-04-14,oauth-2025-04-20") ]
        | Api_key _ -> [ ("anthropic-beta", "files-api-2025-04-14") ]
      in
      let headers =
        Cohttp.Header.of_list
          ([
             auth_header;
             ("anthropic-version", "2023-06-01");
             ("content-type", "multipart/form-data; boundary=" ^ boundary);
           ]
          @ beta_headers)
      in
      let buf = Buffer.create 4096 in
      Buffer.add_string buf ("--" ^ boundary ^ "\r\n");
      Buffer.add_string buf
        ("Content-Disposition: form-data; name=\"file\"; filename=\"" ^ filename
       ^ "\"\r\n");
      Buffer.add_string buf ("Content-Type: " ^ media_type ^ "\r\n\r\n");
      let br =
        Eio.Buf_read.of_flow ~initial_size:4096 ~max_size:max_int content
      in
      let content_str = Eio.Buf_read.take_all br in
      Buffer.add_string buf content_str;
      Buffer.add_string buf ("\r\n--" ^ boundary ^ "--\r\n");
      let body = Cohttp_eio.Body.of_string (Buffer.contents buf) in

      match
        Api_helpers.request client ~meth:`POST ~path:"/files" ~headers ~body
          ~retries_left:client.max_retries ~current_delay:0.5 ()
      with
      | Ok (resp, body_stream) -> (
          let body_str =
            Eio.Buf_read.of_flow ~max_size:max_int body_stream
            |> Eio.Buf_read.take_all
          in
          try
            match Yojson.Safe.from_string body_str |> of_yojson with
            | Ok data -> Ok data
            | Error msg ->
                Error
                  (Http_error
                     {
                       status = Cohttp.Response.status resp;
                       message = "JSON parsing error: " ^ msg;
                     })
          with _ ->
            Error
              (Http_error
                 {
                   status = Cohttp.Response.status resp;
                   message = "JSON parsing error";
                 }))
      | Error e -> Error e

    let get_metadata client ~file_id () =
      Api_helpers.request_json client ~meth:`GET ~path:("/files/" ^ file_id)
        ~betas:[ beta_header ] of_yojson

    let list client ?limit ?after_id () =
      Api_helpers.make_paginated_list_request client ~path_prefix:"/files"
        ~betas:[ beta_header ] of_yojson ?limit ?after_id ()

    let delete client ~file_id () =
      Api_helpers.request_json client ~meth:`DELETE ~path:("/files/" ^ file_id)
        ~betas:[ beta_header ] deleted_of_yojson

    let download client ~file_id () =
      let path = "/files/" ^ file_id ^ "/content" in
      let headers =
        Api_helpers.add_beta_headers
          (Api_helpers.base_headers client.auth)
          [ beta_header ]
      in
      Api_helpers.request client ~meth:`GET ~path ~headers
        ~retries_left:client.max_retries ~current_delay:0.5 ()
  end

  module Messages = struct
    include Messages

    type beta_input_content_block =
      | Text of string
      | Image of { media_type : string; data : string }
      | Document of { name : string; content : string; media_type : string }
      | Tool_result of {
          tool_use_id : string;
          content : string;
          is_error : bool option;
        }
      | File of { id : string }

    let beta_input_content_block_to_yojson = function
      | Text text -> `Assoc [ ("type", `String "text"); ("text", `String text) ]
      | Image { media_type; data } ->
          `Assoc
            [
              ("type", `String "image");
              ( "source",
                `Assoc
                  [
                    ("type", `String "base64");
                    ("media_type", `String media_type);
                    ("data", `String data);
                  ] );
            ]
      | Document { name; content; media_type } ->
          `Assoc
            [
              ("type", `String "document");
              ("name", `String name);
              ( "source",
                `Assoc
                  [
                    ("type", `String "base64");
                    ("media_type", `String media_type);
                    ("data", `String content);
                  ] );
            ]
      | Tool_result { tool_use_id; content; is_error } -> (
          let base =
            [
              ("type", `String "tool_result");
              ("tool_use_id", `String tool_use_id);
              ("content", `String content);
            ]
          in
          match is_error with
          | Some b -> `Assoc (("is_error", `Bool b) :: base)
          | None -> `Assoc base)
      | File { id } ->
          `Assoc
            [
              ("type", `String "file"); ("file", `Assoc [ ("id", `String id) ]);
            ]

    type beta_message = { role : role; content : beta_input_content_block list }

    let beta_message_to_yojson msg =
      `Assoc
        [
          ("role", role_to_yojson msg.role);
          ( "content",
            `List (List.map beta_input_content_block_to_yojson msg.content) );
        ]

    let create_request_body_with_betas ~model ~messages ?max_tokens ?temperature
        ?top_k ?top_p ?stop_sequences ?system ?tools ?tool_choice ?metadata () =
      let body =
        [
          ("model", `String (model_to_string model));
          ("messages", `List (List.map beta_message_to_yojson messages));
        ]
      in
      let add_opt k v f l =
        match v with Some x -> (k, f x) :: l | None -> l
      in
      let body = add_opt "max_tokens" max_tokens (fun i -> `Int i) body in
      let body = add_opt "temperature" temperature (fun f -> `Float f) body in
      let body =
        add_opt "stop_sequences" stop_sequences
          (fun ss -> `List (List.map (fun s -> `String s) ss))
          body
      in
      let body = add_opt "system" system (fun s -> `String s) body in
      let body =
        add_opt "tools" tools
          (fun ts -> `List (List.map tool_to_yojson ts))
          body
      in
      let body = add_opt "top_k" top_k (fun i -> `Int i) body in
      let body = add_opt "top_p" top_p (fun f -> `Float f) body in
      let body = add_opt "tool_choice" tool_choice tool_choice_to_yojson body in
      let body = add_opt "metadata" metadata metadata_to_yojson body in
      `Assoc body

    let send_with_betas client ?(betas = []) ?max_tokens ?temperature ?top_k
        ?top_p ?stop_sequences ?system ?tools ?tool_choice ?metadata ~model
        ~messages () =
      let body_json =
        create_request_body_with_betas ~model ~messages ?max_tokens ?temperature
          ?top_k ?top_p ?stop_sequences ?system ?tools ?tool_choice ?metadata ()
      in
      Api_helpers.request_json client ~meth:`POST ~path:"/messages" ~betas
        ~body:(Yojson.Safe.to_string body_json)
        response_of_yojson
  end
end
