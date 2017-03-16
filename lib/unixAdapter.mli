type raw_chan = {in_ch:Lwt_io.input_channel; in_buf:string; out_ch:Lwt_io.output_channel}
include Session.Adapter with type raw_chan := raw_chan

module TcpSession : sig
  val new_channel : 'p net -> string -> 'p Session.channel Lwt.t
end
