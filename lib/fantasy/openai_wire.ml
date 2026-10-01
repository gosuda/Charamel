let effort = function
  | Request.Off -> None
  | Request.Low -> Some "low"
  | Request.Medium -> Some "medium"
  | Request.High -> Some "high"
