 \header{
   title=" Virginia Reel: for Accordion and Violin"
 }
 
 \score{
   \header {   piece="Swallow Tails"}
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
 
 }\score{
   \header {   piece="Not For Joe"}
 <<
  \time 4/4
  \chords {
    \repeat volta 2 { g1 | d | g | d | g | d | d | g  } 
    \repeat volta 2 { g1 | c | g | d | g | c | c2 d2 | g1   }
  }
 
  \relative c' {
      \key g \major
      \repeat volta 2 { d4 g d g | fis e fis e | d4 g d g | fis g a2 | d,4 g d g | fis e fis e | d4. d8 d4 fis | g1 }
      \repeat volta 2 { <b d>2 <b d>2 | <c e>1 | <b d>4. <a d>8 <g d'>4 <b d>4 | <a d>1 | <b d>2 <b d>2 | <c e> 1 | <b d>4 <b g> <a d> <fis d'> |<b g>1}
  }
 >>
 }