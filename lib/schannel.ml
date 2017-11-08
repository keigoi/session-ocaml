type 'a t      = 'a Event.channel
let create     = Event.new_channel
let send ch x  = Event.sync (Event.send ch x)
let receive ch = Event.sync (Event.receive ch)
