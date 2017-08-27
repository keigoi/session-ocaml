module Chan = Channel.Make
                (Linocaml.Direct.IO)
                (struct
                  type +'a io = 'a
                  include Mutex
                end)
                (struct
                  type +'a io = 'a
                  type m = Mutex.t
                  include Condition
                end)

include Base.Make(Linocaml.Direct)(Chan)

module Net = Net.Make(Linocaml.Direct)(Chan)(Unsafe.Make_raw_dchan(Dchannel.Make(Chan)))
