module Chan = Session_ocaml.Channel.Make
                (Linocaml_lwt.IO)
                (struct
                  type +'a io = 'a Lwt.t
                  include Lwt_mutex
                end)
                (struct
                  type +'a io = 'a Lwt.t
                  type m = Lwt_mutex.t
                  type t = unit Lwt_condition.t
                  let create = Lwt_condition.create
                  let signal c = Lwt_condition.signal c ()
                  let wait c m = Lwt_condition.wait ~mutex:m c
                end)

include Base.Make(Linocaml_lwt)(Chan)
