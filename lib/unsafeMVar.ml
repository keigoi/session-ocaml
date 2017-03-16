type t = unit Lwt_mvar.t
let create_empty () = Obj.magic (Lwt_mvar.create_empty ())
let put m v = Lwt_mvar.put m (Obj.magic v)
let take m = Lwt_mvar.take (Obj.magic m)
