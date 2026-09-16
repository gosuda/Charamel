let render = Mime.serialise
let write sink message = Eio.Flow.copy_string (render message) sink
