module type Session = sig
  val send : 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, empty, ('p, 'r1*'r2) sess) lmonad
  val receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, empty, 'v data * ('p, 'r1*'r2) sess) lmonad
end

    
