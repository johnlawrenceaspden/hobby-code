\version "2.18.2"
\language "english"

\header {
  title = "Gwine Follow"
}

global = {
  \time 2/4
  \key c \major
  \tempo 4=50
}

melody = \transpose c' a {\relative c'' {
  \global
  \repeat volta 2 {\partial 8  c16 c | c e8 e16 g, g e8 f16 g8. b8 b8 c16 d8. r8 b8 c16 d8. r8 b16 b c16 e8 e16 g, a e e 16f g8. f'8 f8 e e d16 d8. c8 r8}
  \repeat volta 2 {\partial 8  g8 |  c d e f16 f16 g g g e f8 f16 f16 e e e e e8 c16 e16 d e d b g8 g8 c8 d8 e8 f16 f16 g g g e f8 f16 f e8 e d16 d8. c4 r8 }
}}

\score {
  <<
    \new Staff \with{midiInstrument=violin} { \melody }
  >>
  \layout { }
  \midi { }
}


