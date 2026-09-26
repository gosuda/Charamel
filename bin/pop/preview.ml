let render = Mime.serialise
let write sink message = Lwt_io.write sink (render message)
