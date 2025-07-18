(** OCaml bindings for the Anthropic API.

    This library provides type-safe OCaml bindings to the Anthropic API, and
    supports both synchronous and streaming responses.

    {1 Getting Started}

    Create a {!client} using your API key, then interact with the Anthropic API
    through sub-modules like {!Messages} and {!Models}. The library requires an
    Eio environment.

    The client manages connection pooling and automatic retries. Stream
    operations provide backpressure control through Eio streams

    Example: Sends a simple message to Claude.
    {[
      open Eio.Std
      open Anthropic

      let () =
        Eio_main.run @@ fun env ->
        Switch.run @@ fun sw ->
        let client = create ~sw ~env () in
        let messages =
          [ Message.user [ Content_block.text "Hello, Claude!" ] ]
        in
        match
          Messages.send client ~max_tokens:1024
            ~model:`Claude_3_5_Sonnet_20240620 ~messages ()
        with
        | Ok response -> (
            match response.content with
            | [ Text_block { text } ] -> traceln "Assistant: %s" text
            | _ -> traceln "Received unexpected content.")
        | Error err -> traceln "Error: %s" (string_of_error err)
    ]} *)

(** {1 Core Types} *)

type auth_method =
  | Api_key of string
  | Bearer_token of string  (** Authentication method for the client *)

type client
(** [client] represents a configured Anthropic API client.

    The client handles authentication, network requests, and automatic retries.
    All API operations require a client instance. *)

(** [api_error_type] categorizes specific API error responses. *)
type api_error_type =
  | Invalid_request_error  (** The request is malformed or invalid. *)
  | Authentication_error  (** The API key is invalid or missing. *)
  | Permission_error  (** The API key lacks required permissions. *)
  | Not_found_error  (** The requested resource does not exist. *)
  | Rate_limit_error  (** The rate limit has been exceeded. *)
  | Api_error_other of string  (** Any other error type returned by the API. *)

(** [error] represents all possible errors from API operations.

    Errors are categorized into three main types: API-level errors with
    structured information, HTTP-level errors with status codes, and
    connection-level errors for network issues. *)
type error =
  | Api_error of {
      type_ : api_error_type;  (** The specific type of API error. *)
      message : string;  (** Human-readable error description. *)
      request_id : string option;  (** Request ID for debugging, if available. *)
    }  (** Structured errors returned by the Anthropic API. *)
  | Http_error of {
      status : Cohttp.Code.status_code;  (** The HTTP status code. *)
      message : string;  (** Response body or error description. *)
    }  (** HTTP-level errors without API error structure. *)
  | Connection_error of exn  (** Network or connection-level exceptions. *)

val string_of_error : error -> string
(** [string_of_error err] converts an error to a human-readable string.

    The output format varies by error type: API errors show the type and
    message, HTTP errors show the status code and message, and connection errors
    show the exception details.

    Example: Converts an authentication error.
    {[
      let err =
        Api_error
          {
            type_ = Authentication_error;
            message = "Invalid API key";
            request_id = None;
          }
      in
      string_of_error err = "API Error (authentication_error): Invalid API key"
    ]} *)

type 'a page = {
  data : 'a list;  (** The items in the current page. *)
  has_more : bool;  (** Whether more pages are available. *)
  first_id : string option;  (** ID of the first item in this page. *)
  last_id : string option;  (** ID of the last item in this page. *)
  get_next_page : unit -> ('a page, error) result option;
      (** Fetches the next page if available. *)
}
(** ['a page] provides pagination for list endpoints.

    Pages are immutable snapshots of results. The [get_next_page] function
    returns [None] when no more pages exist, or [Some result] containing either
    the next page or an error.

    Example: Iterates through all models.
    {[
      let rec print_all_models page =
        List.iter (fun model -> print_endline model.Models.id) page.data;
        match page.get_next_page () with
        | None -> ()
        | Some (Ok next_page) -> print_all_models next_page
        | Some (Error err) -> Printf.eprintf "Error: %s\n" (string_of_error err)
    ]} *)

type model =
  [ `Claude_3_7_Sonnet_Latest  (** Latest Claude 3.7 Sonnet model. *)
  | `Claude_3_7_Sonnet_20250219
    (** Claude 3.7 Sonnet, February 19, 2025 version. *)
  | `Claude_3_5_Haiku_Latest  (** Latest Claude 3.5 Haiku model (fastest). *)
  | `Claude_3_5_Haiku_20241022
    (** Claude 3.5 Haiku, October 22, 2024 version. *)
  | `Claude_Sonnet_4_20250514  (** Claude 4 Sonnet, May 14, 2025 version. *)
  | `Claude_Sonnet_4_0  (** Claude 4.0 Sonnet base version. *)
  | `Claude_4_Sonnet_20250514  (** Alternative naming for Claude 4 Sonnet. *)
  | `Claude_3_5_Sonnet_Latest  (** Latest Claude 3.5 Sonnet model (balanced). *)
  | `Claude_3_5_Sonnet_20241022
    (** Claude 3.5 Sonnet, October 22, 2024 version. *)
  | `Claude_3_5_Sonnet_20240620
    (** Claude 3.5 Sonnet, June 20, 2024 version. *)
  | `Claude_Opus_4_0  (** Claude 4.0 Opus base version (most capable). *)
  | `Claude_Opus_4_20250514  (** Claude 4 Opus, May 14, 2025 version. *)
  | `Claude_4_Opus_20250514  (** Alternative naming for Claude 4 Opus. *)
  | `Claude_3_Opus_Latest
    (** @deprecated
          Will reach end-of-life on January 5th, 2026. Please migrate to a newer
          model. Visit
          https://docs.anthropic.com/en/docs/resources/model-deprecations for
          more information. *)
  | `Claude_3_Opus_20240229
    (** @deprecated
          Will reach end-of-life on January 5th, 2026. Please migrate to a newer
          model. Visit
          https://docs.anthropic.com/en/docs/resources/model-deprecations for
          more information. *)
  | `Claude_3_Sonnet_20240229
    (** @deprecated
          Will reach end-of-life on July 21st, 2025. Please migrate to a newer
          model. Visit
          https://docs.anthropic.com/en/docs/resources/model-deprecations for
          more information. *)
  | `Claude_3_Haiku_20240307
    (** @deprecated
          Will reach end-of-life on July 21st, 2025. Please migrate to a newer
          model. Visit
          https://docs.anthropic.com/en/docs/resources/model-deprecations for
          more information. *)
  | `Claude_2_1
    (** @deprecated
          Will reach end-of-life on July 21st, 2025. Please migrate to a newer
          model. Visit
          https://docs.anthropic.com/en/docs/resources/model-deprecations for
          more information. *)
  | `Claude_2_0
    (** @deprecated
          Will reach end-of-life on July 21st, 2025. Please migrate to a newer
          model. Visit
          https://docs.anthropic.com/en/docs/resources/model-deprecations for
          more information. *)
  | `Other of string
    (** Custom model identifier for experimental or private models. *) ]
(** [model] specifies which Claude model to use for API requests.

    Models are organized by family (Haiku, Sonnet, Opus) and version. Use the
    "Latest" variants for automatic updates to the newest stable version within
    a family. The [`Other] variant allows specifying custom model identifiers.
*)

type role = [ `User | `Assistant ]
(** [role] identifies the author of a message in a conversation.

    Messages alternate between [`User] (human input) and [`Assistant] (Claude's
    responses). Conversations must start with a [`User] message. *)

(** [input_content_block] represents content that can be sent to the API.

    Content blocks are the building blocks of messages. A single message can
    contain multiple content blocks of different types, enabling rich
    conversations with text, images, documents, and tool results. *)
type input_content_block =
  | Text of string  (** Plain text content. *)
  | Image of {
      media_type : string;  (** MIME type (e.g., "image/jpeg", "image/png"). *)
      data : string;  (** Base64-encoded image data. *)
    }  (** Image content for vision tasks. *)
  | Document of {
      name : string;  (** Display name for the document. *)
      content : string;  (** Base64-encoded document data. *)
      media_type : string;  (** MIME type (e.g., "application/pdf"). *)
    }  (** Document content for analysis. *)
  | Tool_result of {
      tool_use_id : string;  (** ID from the corresponding tool use request. *)
      content : string;  (** Result of the tool execution. *)
      is_error : bool option;  (** Whether the tool execution failed. *)
    }  (** Results from tool execution. *)
  | Tool_use of {
      id : string;  (** Unique identifier for this tool use. *)
      name : string;  (** Name of the tool to invoke. *)
      input : Yojson.Safe.t;  (** Arguments for the tool. *)
    }  (** Tool use requests from assistant responses. *)

(** Helper functions for creating content blocks. *)
module Content_block : sig
  val text : string -> input_content_block
  (** [text s] creates a text content block. *)

  val image : media_type:string -> data:string -> input_content_block
  (** [image ~media_type ~data] creates an image content block.

      The [data] must be base64-encoded image data. Supported formats include
      JPEG, PNG, GIF, and WebP. *)

  val document :
    name:string -> content:string -> media_type:string -> input_content_block
  (** [document ~name ~content ~media_type] creates a document content block.

      The [content] must be base64-encoded document data. The [name] is
      displayed to the model for context. *)

  val tool_result :
    tool_use_id:string ->
    content:Yojson.Safe.t ->
    ?is_error:bool ->
    unit ->
    input_content_block
  (** [tool_result ~tool_use_id ~content ?is_error ()] creates a tool result
      block.

      Tool results must reference a [tool_use_id] from a previous assistant
      message. Set [is_error] to [true] when the tool execution failed. The
      [content] parameter accepts structured JSON data which will be
      automatically serialized. *)

  val tool_use :
    id:string -> name:string -> input:Yojson.Safe.t -> input_content_block
  (** [tool_use ~id ~name ~input] creates a tool use content block.

      This is used to represent tool invocations from assistant responses when
      maintaining conversation history. *)
end

type message = {
  role : role;  (** The author of the message. *)
  content : input_content_block list;
      (** Content blocks comprising the message. *)
}
(** [message] represents a single turn in a conversation.

    Messages contain one or more content blocks and are attributed to either the
    user or assistant. Conversations must start with a user message and
    alternate between roles. *)

(** Helper functions for creating messages. *)
module Message : sig
  val user : input_content_block list -> message
  (** [user content] creates a user message.

      User messages represent human input in the conversation. They can contain
      text, images, documents, or tool results. *)

  val assistant : input_content_block list -> message
  (** [assistant content] creates an assistant message.

      Assistant messages represent Claude's responses. They typically contain
      text or tool use requests. *)
end

type tool = {
  name : string;  (** Unique identifier for the tool. *)
  description : string option;
      (** Human-readable description of the tool's purpose. *)
  input_schema : Yojson.Safe.t;
      (** JSON Schema defining the tool's parameters. *)
  type_ : string option;  (** The type of the tool. *)
  cache_control : Yojson.Safe.t option;
      (** Cache control configuration for the tool. *)
}
(** [tool] defines a function that Claude can call.

    Tools enable Claude to interact with external systems or perform
    computations. The [input_schema] must be a valid JSON Schema object
    describing the expected parameters. *)

(** [tool_choice] controls how Claude selects tools during generation. *)
type tool_choice =
  | Auto
      (** Claude decides whether and which tools to use based on the
          conversation. *)
  | Any  (** Claude must use at least one tool, but chooses which one. *)
  | Tool of string  (** Claude must use the specified tool by name. *)
  | Tool_none  (** Claude cannot use any tools, even if provided. *)

type metadata = {
  user_id : string option;  (** Optional identifier for the end user. *)
}
(** [metadata] provides optional tracking information for requests.

    Use metadata to associate API requests with specific users for analytics or
    debugging. The [user_id] should be an opaque identifier that doesn't contain
    PII. *)

(** {1 Client Creation} *)

val create_client :
  sw:Eio.Switch.t ->
  env:< net : 'a Eio.Net.t ; clock : float Eio.Time.clock_ty Eio.Std.r ; .. > ->
  ?api_key:string ->
  ?auth_token:string ->
  ?base_url:string ->
  ?max_retries:int ->
  unit ->
  client
(** [create_client ~sw ~env ?api_key ?auth_token ?base_url ?max_retries ()]
    creates a new API client.

    The client manages connection pooling, automatic retries, and request
    authentication. It requires an Eio switch for lifecycle management and an
    environment with network and clock capabilities.

    @param sw The Eio switch managing the client's resources.
    @param env The Eio environment providing network and clock access.
    @param api_key The Anthropic API key. Cannot be used with [auth_token].
    @param auth_token
      OAuth bearer token for authentication. Cannot be used with [api_key].
    @param base_url
      The API base URL. Defaults to ["https://api.anthropic.com/v1"].
    @param max_retries
      Maximum retry attempts for failed requests. Defaults to [2].

    @raise Invalid_argument
      if neither authentication method is provided or both are provided.

    Example: Creates a client with default settings.
    {[
      Eio_main.run @@ fun env ->
      Switch.run @@ fun sw ->
      let client = Anthropic.create ~sw ~env
        ~api_key:"sk-ant-api03-..."
        ~max_retries:5 () in
      (* Use client for API operations *)
    ]} *)

(** Messages API for conversations with Claude.

    This module provides functions to create messages and handle responses,
    supporting both synchronous and streaming modes. *)
module Messages : sig
  (** [response_content_block] represents content in Claude's responses.

      Response blocks differ from input blocks, containing generated text, tool
      invocations, or thinking traces. *)
  type response_content_block =
    | Text_block of { text : string }  (** Generated text content. *)
    | Tool_use_block of {
        id : string;  (** Unique identifier for this tool use. *)
        name : string;  (** Name of the tool to invoke. *)
        input : Yojson.Safe.t;  (** Arguments for the tool. *)
      }  (** Request to invoke a tool with specific arguments. *)
    | Thinking_block of { id : string; name : string }
        (** Claude's internal reasoning (experimental). *)
    | Redacted_thinking_block of { id : string; name : string }
        (** Redacted reasoning content. *)

  type server_tool_usage = { web_search_requests : int }
  (** [server_tool_usage] tracks server-side tool usage statistics. *)

  type service_tier = [ `Standard | `Priority | `Batch ]
  (** [service_tier] indicates the processing tier for the request. *)

  type usage = {
    input_tokens : int;  (** Tokens processed from the input. *)
    output_tokens : int;  (** Tokens generated in the response. *)
    cache_creation_input_tokens : int option;
        (** Tokens used to create cache entries. *)
    cache_read_input_tokens : int option;  (** Tokens read from cache. *)
    server_tool_use : server_tool_usage option;  (** Server-side tool usage. *)
    service_tier : service_tier option;  (** Processing tier used. *)
  }
  (** [usage] tracks token consumption for billing and limits.

      Cache-related fields indicate when the caching beta feature is active.
      Token counts include all content, system prompts, and tool definitions. *)

  type stop_reason =
    [ `End_turn  (** Natural conversation end. *)
    | `Max_tokens  (** Hit the token limit. *)
    | `Stop_sequence  (** Encountered a stop sequence. *)
    | `Tool_use  (** Stopped to use a tool. *)
    | `Other of string  (** Other stop reason. *) ]
  (** [stop_reason] indicates why generation stopped.

      This field is optional in responses.*)

  type delta_usage = {
    input_tokens : int;  (** Cumulative input tokens. *)
    output_tokens : int;  (** Cumulative output tokens. *)
    cache_creation_input_tokens : int;  (** Cumulative cache creation tokens. *)
    cache_read_input_tokens : int;  (** Cumulative cache read tokens. *)
  }
  (** [delta_usage] tracks cumulative token usage in streaming responses.

      Unlike the regular usage type, all fields are cumulative totals and
      non-optional in streaming delta events. *)

  type response = {
    id : string;  (** Unique identifier for this response. *)
    type_ : string;  (** The type of the response. *)
    model : string;  (** The model that generated the response. *)
    role : role;  (** Always [`Assistant] for responses. *)
    stop_reason : stop_reason option;  (** Why generation stopped. *)
    stop_sequence : string option;
        (** The stop sequence encountered, if any. *)
    content : response_content_block list;  (** Generated content blocks. *)
    usage : usage;  (** Token usage statistics. *)
  }
  (** [response] contains Claude's complete reply to a message.

      The response includes all generated content, metadata about why generation
      stopped, and token usage for monitoring costs. *)

  (** [stream_event] represents incremental updates during streaming.

      Events arrive in order, allowing real-time processing of Claude's response
      as it's generated. *)
  type stream_event =
    | Message_start of response  (** Initial response metadata. *)
    | Content_block_start of {
        index : int;  (** Zero-based position in content array. *)
        content : response_content_block;  (** The starting content block. *)
      }  (** Beginning of a new content block. *)
    | Content_block_delta of {
        index : int;  (** Index of the content block being updated. *)
        delta : [ `Text of string | `Input_json of string ];
            (** Incremental content. *)
      }  (** Partial content for a block. *)
    | Content_block_stop of { index : int }  (** End of a content block. *)
    | Message_delta of {
        stop_reason :
          [ `End_turn
          | `Max_tokens
          | `Stop_sequence
          | `Tool_use
          | `Other of string ];
        usage : delta_usage;  (** Cumulative token counts. *)
      }  (** Final message metadata. *)
    | Message_stop  (** End of the message stream. *)
    | Ping  (** Keep-alive signal. *)

  val send :
    client ->
    ?max_tokens:int ->
    ?temperature:float ->
    ?top_k:int ->
    ?top_p:float ->
    ?stop_sequences:string list ->
    ?system:string ->
    ?tools:tool list ->
    ?tool_choice:tool_choice ->
    ?metadata:metadata ->
    model:model ->
    messages:message list ->
    unit ->
    (response, error) result
  (** [send client ~model ~messages ?max_tokens ... ()] sends messages to Claude
      and awaits a complete response.

      This is the primary function for synchronous conversations with Claude.
      The response contains all generated content at once.

      For streaming responses, see {!send_stream}. For simple text queries, see
      {!simple_query}.

      @param max_tokens Maximum tokens to generate. Required for some models.
      @param temperature
        Randomness in generation from 0.0 (deterministic) to 1.0 (creative).
      @param top_k Sample from the k most likely tokens.
      @param top_p Nucleus sampling threshold.
      @param stop_sequences Strings that stop generation when encountered.
      @param system System prompt to guide Claude's behavior.
      @param tools Available tools Claude can use.
      @param tool_choice How Claude should select tools.
      @param metadata Request tracking information.
      @param model The Claude model to use.
      @param messages Conversation history.

      @raise Api_error for invalid requests or authentication issues.
      @raise Http_error for network-level failures.
      @raise Connection_error for connection problems.

      Example: Sends a simple message.
      {[
        let response =
          Messages.send client ~model:`Claude_3_5_Sonnet_Latest
            ~messages:[ Message.user [ Content_block.text "Hello!" ] ]
            ~max_tokens:1000 ()
        in
        match response with
        | Ok resp ->
            List.iter
              (function
                | Messages.Text_block { text } -> print_endline text | _ -> ())
              resp.content
        | Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
      ]} *)

  val send_stream :
    client ->
    ?max_tokens:int ->
    ?temperature:float ->
    ?top_k:int ->
    ?top_p:float ->
    ?stop_sequences:string list ->
    ?system:string ->
    ?tools:tool list ->
    ?tool_choice:tool_choice ->
    ?metadata:metadata ->
    model:model ->
    messages:message list ->
    unit ->
    (stream_event Eio.Stream.t, error) result
  (** [send_stream client ~model ~messages ... ()] sends messages to Claude and
      streams the response incrementally.

      Returns an Eio stream of events, enabling real-time processing of Claude's
      response. The stream automatically closes when the response completes.

      Parameters are identical to {!send}.

      For higher-level stream processing, see {!iter_stream} for callbacks or
      {!accumulate_stream} to collect the stream into a final response.

      Example: Prints text as it arrives.
      {[
        match
          Messages.send_stream client ~model ~messages ~max_tokens:1000 ()
        with
        | Ok stream ->
            Eio.Stream.iter
              (function
                | Content_block_delta { delta = `Text text; _ } ->
                    print_string text;
                    flush stdout
                | Message_stop -> print_newline ()
                | _ -> ())
              stream
        | Error e -> Printf.eprintf "Stream error: %s\n" (string_of_error e)
      ]} *)

  val iter_stream :
    client ->
    ?max_tokens:int ->
    ?temperature:float ->
    ?top_k:int ->
    ?top_p:float ->
    ?stop_sequences:string list ->
    ?system:string ->
    ?tools:tool list ->
    ?tool_choice:tool_choice ->
    ?metadata:metadata ->
    model:model ->
    messages:message list ->
    on_event:(stream_event -> unit) ->
    on_error:(error -> unit) ->
    unit ->
    unit
  (** [iter_stream client ~on_event ~on_error ... ()] processes a stream with
      callbacks.

      This function handles stream lifecycle automatically. It opens the stream,
      processes all events, and ensures proper cleanup.

      @param on_event Called for each stream event.
      @param on_error Called if streaming fails.

      Example: Collects streamed text with error handling.
      {[
        let buffer = Buffer.create 1024 in
        Messages.iter_stream client ~model:`Claude_3_5_Haiku_Latest ~messages
          ~max_tokens:500
          ~on_event:(function
            | Content_block_delta { delta = `Text text; _ } ->
                Buffer.add_string buffer text
            | Message_stop ->
                Printf.printf "Complete: %s\n" (Buffer.contents buffer)
            | _ -> ())
          ~on_error:(fun e ->
            Printf.eprintf "Streaming failed: %s\n" (string_of_error e))
          ()
      ]} *)

  val accumulate_stream : stream_event Eio.Stream.t -> (response, error) result
  (** [accumulate_stream stream] collects all stream events into a complete
      response.

      This function consumes the entire stream, accumulating text deltas and
      updating content blocks until [Message_stop] is received. The final
      response contains all generated content as if it were created
      synchronously.

      @raise Connection_error if the stream ends without proper completion.

      Example: Converts streaming to synchronous style.
      {[
        match
          Messages.create_stream client ~model ~messages ~max_tokens:1000 ()
        with
        | Ok stream -> (
            match Messages.accumulate_stream stream with
            | Ok response ->
                Printf.printf "Got %d content blocks\n"
                  (List.length response.content)
            | Error e ->
                Printf.eprintf "Accumulation error: %s\n" (string_of_error e))
        | Error e ->
            Printf.eprintf "Stream creation error: %s\n" (string_of_error e)
      ]} *)

  (** {2 Message Building Helpers} *)

  val user : string -> message
  (** [user text] creates a user message with text content.

      Example:
      {[
        let msg = Messages.user "What is the capital of France?"
      ]} *)

  val assistant : string -> message
  (** [assistant text] creates an assistant message with text content.

      Example:
      {[
        let msg = Messages.assistant "The capital of France is Paris."
      ]} *)

  val user_with_content : input_content_block list -> message
  (** [user_with_content blocks] creates a user message with custom content
      blocks.

      Example:
      {[
        let msg =
          Messages.user_with_content
            [
              Text "Here's an image:";
              Image { media_type = "image/png"; data = base64_data };
            ]
      ]} *)

  val assistant_with_content : input_content_block list -> message
  (** [assistant_with_content blocks] creates an assistant message with custom
      content blocks. *)

  val tool_result_message :
    tool_use_id:string ->
    content:Yojson.Safe.t ->
    ?is_error:bool ->
    unit ->
    message
  (** [tool_result_message ~tool_use_id ~content ?is_error ()] creates a user
      message containing a tool result.

      The [content] parameter accepts structured JSON data which will be
      automatically serialized.

      Example:
      {[
        let msg =
          Messages.tool_result_message ~tool_use_id:"tool_123"
            ~content:(`Assoc [ ("result", `Int 42) ])
            ()
      ]} *)

  (** {2 Content Extraction Helpers} *)

  val extract_text : response_content_block list -> string option
  (** [extract_text blocks] extracts the first text content from response
      blocks.

      Example:
      {[
        match Messages.extract_text response.content with
        | Some text -> print_endline text
        | None -> print_endline "No text content"
      ]} *)

  val extract_all_text : response_content_block list -> string list
  (** [extract_all_text blocks] extracts all text content from response blocks.
  *)

  val find_tool_use :
    response_content_block list -> (string * string * Yojson.Safe.t) option
  (** [find_tool_use blocks] finds the first tool use in response blocks.
      Returns [(id, name, input)] if found. *)

  val response_to_input_content :
    response_content_block list -> input_content_block list
  (** [response_to_input_content blocks] converts response blocks to input
      blocks for use in conversation continuations. *)

  val response_content_to_input : response_content_block -> input_content_block
  (** [response_content_to_input block] converts a single response block to an
      input block. *)
end

(** Tools module for handling tool execution patterns.

    This module provides utilities to simplify tool handling in conversations.
*)
module Tools : sig
  type tool_execution_result =
    | Success of Yojson.Safe.t
    | Error of string  (** Result of executing a tool. *)

  val make_tool_result :
    tool_use_id:string -> result:tool_execution_result -> input_content_block
  (** [make_tool_result ~tool_use_id ~result] creates a tool result content
      block.

      Example:
      {[
        let result =
          Tools.make_tool_result ~tool_use_id:"tool_123"
            ~result:(Success (`Assoc [ ("answer", `Int 42) ]))
      ]} *)

  val requires_tool_execution : Messages.response_content_block list -> bool
  (** [requires_tool_execution blocks] checks if response contains tool use
      requests. *)

  val extract_tool_calls :
    Messages.response_content_block list ->
    (string * string * Yojson.Safe.t) list
  (** [extract_tool_calls blocks] extracts all tool calls from response blocks.
      Returns a list of [(id, name, input)] tuples. *)
end

(** Models API for querying available Claude models.

    This module provides functions to retrieve information about available
    models and their capabilities. *)
module Models : sig
  type t = {
    id : string;  (** Unique model identifier. *)
    created_at : string;  (** ISO 8601 timestamp of model creation. *)
    display_name : string;  (** Human-readable model name. *)
    type_ : string;  (** Model type (e.g., "chat"). *)
  }
  (** [t] contains metadata about an available model. *)

  val get : client -> model_id:string -> unit -> (t, error) result
  (** [get client ~model_id ()] retrieves information about a specific model.

      @param model_id The model identifier to query.

      @raise Api_error if the model doesn't exist or isn't accessible.

      Example: Gets details about a specific model.
      {[
        match Models.get client ~model_id:"claude-3-5-sonnet-20241022" () with
        | Ok model ->
            Printf.printf "Model %s created at %s\n" model.display_name
              model.created_at
        | Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
      ]} *)

  val list :
    client -> ?limit:int -> ?after_id:string -> unit -> (t page, error) result
  (** [list client ?limit ?after_id ()] retrieves a paginated list of available
      models.

      @param limit Maximum models per page (default: 20).
      @param after_id Pagination cursor for subsequent pages.

      Example: Lists all available models.
      {[
        let rec print_models page =
          List.iter
            (fun m -> Printf.printf "- %s (%s)\n" m.display_name m.id)
            page.data;
          match page.get_next_page () with
          | Some (Ok next) -> print_models next
          | Some (Error e) ->
              Printf.eprintf "Pagination error: %s\n" (string_of_error e)
          | None -> ()
        in
        match Models.list client () with
        | Ok page -> print_models page
        | Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
      ]} *)
end

type api_error = {
  type_ : string;  (** Error type identifier. *)
  message : string;  (** Human-readable error message. *)
  request_id : string option;  (** Request ID for debugging. *)
}
(** [api_error] represents structured error responses from the API.

    This type is primarily used internally and in the Batches module for
    individual request failures. *)

(** Batches API for asynchronous bulk message processing.

    This module enables processing multiple message requests asynchronously,
    reducing costs and improving throughput for large-scale operations. Batches
    process in the background and results can be retrieved when complete. *)
module Batches : sig
  type status = [ `In_progress | `Canceling | `Ended ]
  (** [status] indicates the current state of batch processing.

      Batches transition from [`In_progress] through optional [`Canceling] to
      [`Ended]. *)

  type request_counts = {
    canceled : int;  (** Requests canceled before processing. *)
    errored : int;  (** Requests that failed during processing. *)
    expired : int;  (** Requests that expired before processing. *)
    processing : int;  (** Requests currently being processed. *)
    succeeded : int;  (** Successfully completed requests. *)
  }
  (** [request_counts] tracks the status of individual requests in a batch.

      The sum of all counts equals the total number of requests in the batch. *)

  type t = {
    id : string;  (** Unique batch identifier. *)
    type_ : string;  (** Batch type (always "messages_batch"). *)
    processing_status : status;  (** Current processing state. *)
    request_counts : request_counts;  (** Request statistics. *)
    created_at : string;  (** ISO 8601 creation timestamp. *)
    expires_at : string;  (** ISO 8601 expiration timestamp. *)
    ended_at : string option;  (** ISO 8601 completion timestamp. *)
    cancel_initiated_at : string option;
        (** ISO 8601 cancellation timestamp. *)
    archived_at : string option;  (** ISO 8601 archival timestamp. *)
    results_url : string option;  (** URL for downloading results. *)
  }
  (** [t] represents a batch job with its current state and statistics.

      Batches have a 24-hour processing window before expiration. Results remain
      available for download after completion. *)

  type request = {
    custom_id : string;  (** User-defined identifier for tracking. *)
    params : Yojson.Safe.t;  (** Message API parameters as JSON. *)
  }
  (** [request] defines a single message request within a batch.

      The [params] field should contain the same JSON structure as a regular
      Messages.create call, including model, messages, and other parameters. *)

  type individual_response = {
    custom_id : string;  (** The custom ID from the original request. *)
    result : [ `Succeeded of Messages.response | `Errored of api_error ];
        (** Processing result. *)
  }
  (** [individual_response] contains the result of a single batch request.

      Each response maps back to its original request via [custom_id]. Failed
      requests include structured error information. *)

  val submit : client -> requests:request list -> unit -> (t, error) result
  (** [submit client ~requests ()] submits a new batch for processing.

      @param requests List of message requests to process (max 10,000).

      @raise Api_error if the batch is too large or malformed.

      Example: Submits a batch of similar requests.
      {[
        let make_request i =
          {
            custom_id = Printf.sprintf "req_%d" i;
            params =
              `Assoc
                [
                  ("model", `String "claude-3-5-haiku-20241022");
                  ("max_tokens", `Int 100);
                  ( "messages",
                    `List
                      [
                        `Assoc
                          [
                            ("role", `String "user");
                            ( "content",
                              `String (Printf.sprintf "Summarize article %d" i)
                            );
                          ];
                      ] );
                ];
          }
        in
        let requests = List.init 100 make_request in
        match Batches.submit client ~requests () with
        | Ok batch -> Printf.printf "Submitted batch %s\n" batch.id
        | Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
      ]} *)

  val get : client -> batch_id:string -> unit -> (t, error) result
  (** [get client ~batch_id ()] retrieves the current status of a batch.

      Use this to poll for batch completion or check processing statistics.

      Example: Polls until batch completes.
      {[
        let rec wait_for_completion client batch_id =
          match Batches.get client ~batch_id () with
          | Ok batch when batch.processing_status = `Ended ->
              Printf.printf "Batch completed: %d succeeded, %d failed\n"
                batch.request_counts.succeeded batch.request_counts.errored
          | Ok _ ->
              Eio.Time.sleep env#clock 5.0;
              wait_for_completion client batch_id
          | Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
      ]} *)

  val list :
    client -> ?limit:int -> ?after_id:string -> unit -> (t page, error) result
  (** [list client ?limit ?after_id ()] retrieves a paginated list of batches.

      Lists batches in reverse chronological order (newest first).

      @param limit Maximum batches per page (default: 20).
      @param after_id Pagination cursor for subsequent pages. *)

  val cancel : client -> batch_id:string -> unit -> (t, error) result
  (** [cancel client ~batch_id ()] cancels an in-progress batch.

      Only batches with status [`In_progress] can be canceled. Already-processed
      requests are not affected.

      @raise Api_error if the batch has already ended or been canceled. *)

  val results :
    client ->
    batch_id:string ->
    unit ->
    (individual_response Eio.Stream.t, error) result
  (** [results client ~batch_id ()] streams individual results from a completed
      batch.

      Results are streamed as newline-delimited JSON. The batch must have status
      [`Ended] to retrieve results.

      @raise Api_error if the batch hasn't completed yet.

      Example: Processes batch results.
      {[
        match Batches.results client ~batch_id () with
        | Ok stream ->
            Eio.Stream.iter
              (function
                | { custom_id; result = `Succeeded response } ->
                    Printf.printf "Request %s succeeded\n" custom_id
                | { custom_id; result = `Errored error } ->
                    Printf.printf "Request %s failed: %s\n" custom_id
                      error.message)
              stream
        | Error e -> Printf.eprintf "Error: %s\n" (string_of_error e)
      ]} *)
end

(** Beta features for experimental functionality.

    This module contains features that are still in development. Interfaces may
    change without notice between versions. Use with caution in production code.
*)
module Beta : sig
  (** Files API for uploading and managing files (beta).

      This API enables uploading files that can be referenced in messages.
      Requires beta header "files-api-2025-04-14". *)
  module Files : sig
    type t = {
      id : string;  (** Unique file identifier. *)
      type_ : string;  (** The type of the file. *)
      filename : string;  (** Original filename. *)
      mime_type : string;  (** MIME type of the file. *)
      size_bytes : int;  (** File size in bytes. *)
      created_at : string;  (** ISO 8601 creation timestamp. *)
      downloadable : bool option;  (** Whether the file can be downloaded. *)
    }
    (** [t] represents metadata about an uploaded file.

        Files are stored securely and can be referenced in messages using the
        File content block type. *)

    type deleted = { id : string }
    (** Confirmation of file deletion. *)

    val upload :
      client ->
      filename:string ->
      media_type:string ->
      content:Eio.Flow.source_ty Eio.Resource.t ->
      unit ->
      (t, error) result
    (** [upload client ~filename ~media_type ~content ()] uploads a file.

        @param filename The name to display for the file.
        @param media_type MIME type (e.g., "application/pdf", "text/plain").
        @param content An Eio flow containing the file data.

        @raise Api_error if the file is too large or has an unsupported type.

        Example: Uploads a text file.
        {[
          let content = Eio.Flow.string_source "Hello, world!" in
          match
            Beta.Files.upload client ~filename:"greeting.txt"
              ~media_type:"text/plain" ~content ()
          with
          | Ok file -> Printf.printf "Uploaded file %s\n" file.id
          | Error e -> Printf.eprintf "Upload failed: %s\n" (string_of_error e)
        ]} *)

    val get_metadata : client -> file_id:string -> unit -> (t, error) result
    (** [get_metadata client ~file_id ()] retrieves file metadata.

        @param file_id The file identifier to query.

        @raise Api_error if the file doesn't exist or isn't accessible. *)

    val list :
      client -> ?limit:int -> ?after_id:string -> unit -> (t page, error) result
    (** [list client ?limit ?after_id ()] retrieves a paginated list of files.

        Lists files in reverse chronological order (newest first).

        @param limit Maximum files per page.
        @param after_id Pagination cursor for subsequent pages. *)

    val delete : client -> file_id:string -> unit -> (deleted, error) result
    (** [delete client ~file_id ()] permanently deletes a file.

        Deleted files cannot be recovered. Any messages referencing the file
        will fail.

        @param file_id The file to delete.

        @raise Api_error if the file doesn't exist. *)

    val download :
      client ->
      file_id:string ->
      unit ->
      (Cohttp.Response.t * Cohttp_eio.Body.t, error) result
    (** [download client ~file_id ()] retrieves file content.

        Returns the raw HTTP response and body for flexible handling.

        Example: Downloads and saves a file.
        {[
          match Beta.Files.download client ~file_id () with
          | Ok (resp, body) ->
              let content_type =
                Cohttp.Header.get (Cohttp.Response.headers resp) "content-type"
              in
              Printf.printf "Content type: %s\n"
                (Option.value content_type ~default:"unknown")
              (* Process body stream *)
          | Error e ->
              Printf.eprintf "Download failed: %s\n" (string_of_error e)
        ]} *)
  end

  (** Extended Messages API with beta features.

      This module includes all stable Messages features plus experimental
      additions like file references. *)
  module Messages : sig
    include module type of Messages
    (** Includes all stable Messages API features. *)

    (** [beta_input_content_block] extends content blocks with beta features.

        The File variant enables referencing uploaded files in messages. *)
    type beta_input_content_block =
      | Text of string  (** Plain text content. *)
      | Image of { media_type : string; data : string }
          (** Base64-encoded image. *)
      | Document of { name : string; content : string; media_type : string }
          (** Base64-encoded document. *)
      | Tool_result of {
          tool_use_id : string;
          content : string;
          is_error : bool option;
        }  (** Tool execution result. *)
      | File of { id : string }  (** Reference to an uploaded file (beta). *)

    type beta_message = {
      role : role;  (** Message author. *)
      content : beta_input_content_block list;
          (** Content with beta features. *)
    }
    (** [beta_message] supports beta content block types.

        Use beta messages to include file references from the Files API. *)

    val send_with_betas :
      client ->
      ?betas:(string * string) list ->
      ?max_tokens:int ->
      ?temperature:float ->
      ?top_k:int ->
      ?top_p:float ->
      ?stop_sequences:string list ->
      ?system:string ->
      ?tools:tool list ->
      ?tool_choice:tool_choice ->
      ?metadata:metadata ->
      model:model ->
      messages:beta_message list ->
      unit ->
      (response, error) result
    (** [send_with_betas client ?betas ... ~messages ()] sends a message with
        beta features.

        @param betas
          Beta feature headers to enable (e.g.,
          [("anthropic-beta", "files-api-2025-04-14")]).

        Example: Uses an uploaded file in a message.
        {[
          (* First upload a file *)
          let file_id = match Beta.Files.upload client ~filename:"data.csv" ... with
            | Ok file -> file.id
            | Error _ -> failwith "Upload failed" in

          (* Reference it in a message *)
          let messages = [
            { role = `User;
              content = [
                Text "Please analyze this CSV file:";
                File { id = file_id }
              ]
            }
          ] in
          Beta.Messages.send_with_betas client
            ~betas:["files-api-2025-04-14"]
            ~model:`Claude_3_5_Sonnet_Latest
            ~messages
            ~max_tokens:1000
            ()
        ]} *)
  end
end
