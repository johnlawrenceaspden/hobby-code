 <<
  \time 4/4
  \chords {
    \repeat volta 2 { g1 | g |g |g |g |g |g2 d2| g1  } 
    \repeat volta 2 { g1 | c |g |g2 d2|  }
    \alternative {{g1 | c | s | g }
                  {g1 | g | g2 d2 | g}}
  }
 
  \relative c'' {
      \key g \major
      \repeat volta 2 {g4 e d e | g2 g4. a8 | b4 d a b8 a | g4 e d4. e8 | g4 e d e | g2 g4. a8 | b4 d a b8 a8 | g1 |}
      \repeat volta 2 {b4 d d2 | c4 e e2 | d4 b a b8 a8 | g4 e d2 |}
      \alternative {{b'4 d d2 |c4 e e2 | g4. fis8 e4 d | e fis g2}
                    {g,4 e d e | g2 g4. a8 | b4 d a b8 a8 | g1}}
  }
 >>