type t = {
  cwd : string;
  fs_root : string;
  stdin : Lwt_io.input_channel;
  stdout : Lwt_io.output_channel;
  stderr : Lwt_io.output_channel;
  clock : Charamel_os.Time.clock;
}
