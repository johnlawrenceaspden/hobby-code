\version "2.18.2"
\language "english"


\header {
  title = ""
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}

melody = \relative c' {
  \global

  c4 c g' g ds f f g2.
  
  g4 g g af f g2.

  r1
  
  c,4 c g' g ds f f g2.
  c,4 c g' g ds f f e2.
  
  
  g4 g gs as2 c4 as gs f g2.
  ds4 f f g2.

  g4 g gs as2 c4 as gs f g2.
  ds4 f f e2.
  
}

words = \lyricmode {
  words go here, here
}

\score {
  <<
    \new Staff \with{midiInstrument=violin} { \melody }
    \addlyrics { \words }
  >>
  \layout { }
  \midi { }
}

