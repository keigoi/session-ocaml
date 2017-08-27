module type RAW_DCHAN = sig
  type +'a io
  type t
  val create : unit -> t
  val send : t -> 'a -> unit io
  val receive : t -> 'a io
  val reverse : t -> t
end

module Make_raw_dchan(M:Dchannel.S)
       : RAW_DCHAN with type 'a io = 'a M.io
  = struct
  type +'a io = 'a M.io
  type t = unit M.t
  let create = M.create
  let send c v = M.send c (Obj.magic v)
  let receive c = Obj.magic (M.receive c)
  let reverse c = M.reverse c
end
