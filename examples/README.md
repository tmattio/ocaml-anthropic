# Anthropic OCaml SDK Examples

This directory contains examples demonstrating various features of the Anthropic OCaml SDK.

## Running Examples

All examples can be run using dune. Make sure you have set your Anthropic API key first:

```bash
export ANTHROPIC_API_KEY="your-api-key-here"
```

## Examples Overview

### 1. Basic Message (`basic_message.ml`)
**What it demonstrates:**
- Creating a client
- Sending a simple message to Claude
- Handling the response
- Basic error handling

```bash
dune exec examples/basic_message.exe
```

**Key concepts:**
- Using `Messages.create` for synchronous responses
- Pattern matching on response content blocks
- Converting errors to readable strings

### 2. Message Streaming (`message_streaming.ml`)
**What it demonstrates:**
- Streaming responses in real-time
- Processing stream events as they arrive
- Accumulating streamed content
- Handling different event types

```bash
dune exec examples/message_streaming.exe
```

**Key concepts:**
- Using `Messages.create_stream` for streaming
- Processing `Content_block_delta` events
- Real-time output with `flush stdout`
- Stream completion handling

### 3. Tool Use (`tool_use.ml`)
**What it demonstrates:**
- Defining tools with JSON schemas
- Letting Claude decide when to use tools
- Processing tool use requests
- Sending tool results back to Claude

```bash
dune exec examples/tool_use.exe
```

**Key concepts:**
- Tool definition with input schemas
- Handling `Tool_use_block` in responses
- Simulating tool execution
- Multi-turn conversations with tool results

### 4. Tool Use with Streaming (`tool_use_streaming.ml`)
**What it demonstrates:**
- Combining tool use with streaming
- Accumulating tool arguments from stream deltas
- Real-time display of Claude's reasoning
- Complete tool interaction flow

```bash
dune exec examples/tool_use_streaming.exe
```

**Key concepts:**
- Streaming with `accumulate_stream`
- Processing accumulated tool calls
- Handling JSON deltas for tool inputs
- Maintaining conversation context

## Understanding the Examples

Each example is self-contained and demonstrates a specific aspect of the API. They progress from simple to complex:

1. Start with `basic_message.ml` to understand the fundamentals
2. Move to `message_streaming.ml` to see real-time streaming
3. Explore `tool_use.ml` to understand function calling
4. Combine concepts with `tool_use_streaming.ml`

## Common Patterns

### Error Handling
All examples use OCaml's `Result` type for error handling:
```ocaml
match operation with
| Ok result -> (* Handle success *)
| Error err -> Printf.eprintf "Error: %s\n" (Anthropic.string_of_error err)
```

### Client Creation
```ocaml
Eio_main.run @@ fun env ->
Switch.run @@ fun sw ->
let client = Anthropic.create ~sw ~env () in
(* Use client *)
```

### Message Construction
```ocaml
let messages = [
  Anthropic.Message.user [
    Anthropic.Content_block.text "Your message here"
  ]
] in
```

## Tips for Your Own Code

1. **Always handle errors**: The API can fail due to network issues, rate limits, or invalid requests
2. **Use streaming for better UX**: Streaming provides immediate feedback for long responses
3. **Design tools carefully**: Clear descriptions and schemas help Claude use tools effectively
4. **Monitor token usage**: Check the `usage` field in responses to track costs
5. **Use appropriate models**: Haiku for speed, Sonnet for balance, Opus for complex tasks
