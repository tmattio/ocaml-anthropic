open Eio.Std
open Anthropic

(* Helper function for colored output *)
let color s = Printf.sprintf "\027[1;33m%s\027[0m" s

(* Tool implementations *)
type coordinate_response = { long : float; lat : float }

let get_coordinates _location =
  (* Simulated response for San Francisco *)
  { long = -122.4194; lat = 37.7749 }

let get_temperature_unit _country = "fahrenheit"

type weather_response = { unit : string; temperature : float }

let get_weather _lat _long _unit = { unit = "fahrenheit"; temperature = 122.0 }

(* Convert tool responses to JSON *)
let coordinate_to_json coord =
  `Assoc [ ("long", `Float coord.long); ("lat", `Float coord.lat) ]

let weather_to_json weather =
  `Assoc
    [
      ("unit", `String weather.unit); ("temperature", `Float weather.temperature);
    ]

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let client = create_client ~sw ~env () in

  let content = "What is the weather in San Francisco, CA?" in

  print_endline (color "[user]: " ^ content);

  let initial_message = Message.user [ Content_block.text content ] in
  let messages = ref [ initial_message ] in

  let tools =
    [
      {
        name = "get_coordinates";
        description =
          Some
            "Accepts a place as an address, then returns the latitude and \
             longitude coordinates.";
        input_schema =
          `Assoc
            [
              ("type", `String "object");
              ( "properties",
                `Assoc
                  [
                    ( "location",
                      `Assoc
                        [
                          ("type", `String "string");
                          ("description", `String "The location to look up.");
                        ] );
                  ] );
              ("required", `List [ `String "location" ]);
            ];
      };
      {
        name = "get_temperature_unit";
        description = None;
        input_schema =
          `Assoc
            [
              ("type", `String "object");
              ( "properties",
                `Assoc
                  [
                    ( "country",
                      `Assoc
                        [
                          ("type", `String "string");
                          ("description", `String "The country");
                        ] );
                  ] );
              ("required", `List [ `String "country" ]);
            ];
      };
      {
        name = "get_weather";
        description = Some "Get the weather at a specific location";
        input_schema =
          `Assoc
            [
              ("type", `String "object");
              ( "properties",
                `Assoc
                  [
                    ( "lat",
                      `Assoc
                        [
                          ("type", `String "number");
                          ( "description",
                            `String
                              "The latitude of the location to check weather."
                          );
                        ] );
                    ( "long",
                      `Assoc
                        [
                          ("type", `String "number");
                          ( "description",
                            `String
                              "The longitude of the location to check weather."
                          );
                        ] );
                    ( "unit",
                      `Assoc
                        [
                          ("type", `String "string");
                          ( "enum",
                            `List [ `String "celsius"; `String "fahrenheit" ] );
                          ("description", `String "Unit for the output");
                        ] );
                  ] );
              ( "required",
                `List [ `String "lat"; `String "long"; `String "unit" ] );
            ];
      };
    ]
  in

  let rec loop () =
    match
      Messages.send client ~model:`Claude_3_5_Sonnet_20240620 ~max_tokens:1024
        ~messages:!messages ~tools ()
    with
    | Error err -> print_endline ("Error: " ^ string_of_error err)
    | Ok response ->
        print_string (color "[assistant]: ");

        let tool_results = ref [] in
        List.iter
          (function
            | Messages.Text_block { text } ->
                print_endline text;
                print_endline ""
            | Messages.Tool_use_block { id; name; input } ->
                let input_str = Yojson.Safe.to_string input in
                print_endline
                  (Printf.sprintf "%s (id: %s): %s" name id input_str);
                print_endline ""
            | _ -> ())
          response.content;
        print_endline "";

        (* Process tool calls separately *)
        List.iter
          (function
            | Messages.Tool_use_block { id; name; input } ->
                (* Process tool calls *)
                print_string (color ("[user (" ^ name ^ ")]: "));
                let result_json =
                  match name with
                  | "get_coordinates" ->
                      let location =
                        match Yojson.Safe.Util.member "location" input with
                        | `String s -> s
                        | _ -> failwith "Invalid location"
                      in
                      coordinate_to_json (get_coordinates location)
                  | "get_temperature_unit" ->
                      let country =
                        match Yojson.Safe.Util.member "country" input with
                        | `String s -> s
                        | _ -> failwith "Invalid country"
                      in
                      `String (get_temperature_unit country)
                  | "get_weather" ->
                      let lat =
                        match Yojson.Safe.Util.member "lat" input with
                        | `Float f -> f
                        | `Int i -> float_of_int i
                        | _ -> failwith "Invalid lat"
                      in
                      let long =
                        match Yojson.Safe.Util.member "long" input with
                        | `Float f -> f
                        | `Int i -> float_of_int i
                        | _ -> failwith "Invalid long"
                      in
                      let unit =
                        match Yojson.Safe.Util.member "unit" input with
                        | `String s -> s
                        | _ -> failwith "Invalid unit"
                      in
                      weather_to_json (get_weather lat long unit)
                  | _ -> failwith ("Unknown tool: " ^ name)
                in
                print_endline (Yojson.Safe.to_string result_json);
                tool_results :=
                  !tool_results
                  @ [
                      Content_block.tool_result ~tool_use_id:id
                        ~content:result_json ();
                    ]
            | _ -> ())
          response.content;

        (* Convert response content blocks to input content blocks for history *)
        let assistant_content =
          List.filter_map
            (function
              | Messages.Text_block { text } when text <> "" ->
                  Some (Content_block.text text)
              | Messages.Tool_use_block { id; name; input } ->
                  Some (Content_block.tool_use ~id ~name ~input)
              | _ -> None)
            response.content
        in

        (* Add the assistant's message to the conversation history *)
        let assistant_message = Message.assistant assistant_content in
        messages := !messages @ [ assistant_message ];

        (* If there were tool calls, add tool results as a user message and continue *)
        if !tool_results <> [] then (
          let tool_result_message = Message.user !tool_results in
          messages := !messages @ [ tool_result_message ];
          loop ())
        else print_endline "\n(Conversation complete - no tool calls)"
  in
  loop ()
