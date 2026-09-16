type t =
  | Text_delta of string
  | Reasoning_delta of string
  | Tool_call_start of { id : string; name : string }
  | Tool_input_delta of { id : string; delta : string }
  | Tool_call_end of string
  | Usage of Usage.t
  | Finish of [ `Stop | `Tool_calls | `Length | `Content_filter | `Error of string ]

let pp ppf = function
  | Text_delta s -> Fmt.pf ppf "text_delta %S" s
  | Reasoning_delta s -> Fmt.pf ppf "reasoning_delta %d" (String.length s)
  | Tool_call_start { id; name } -> Fmt.pf ppf "tool_call_start %s/%s" name id
  | Tool_input_delta { id; delta } ->
      Fmt.pf ppf "tool_input_delta %s +%d" id (String.length delta)
  | Tool_call_end id -> Fmt.pf ppf "tool_call_end %s" id
  | Usage u -> Fmt.pf ppf "usage %a" Usage.pp u
  | Finish `Stop -> Fmt.string ppf "finish stop"
  | Finish `Tool_calls -> Fmt.string ppf "finish tool_calls"
  | Finish `Length -> Fmt.string ppf "finish length"
  | Finish `Content_filter -> Fmt.string ppf "finish content_filter"
  | Finish (`Error msg) -> Fmt.pf ppf "finish error %S" msg
