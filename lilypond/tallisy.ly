\version "2.18.2"
\language "english"

% 1 1 5 5 2# 4# 4# 5#

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
  
  g4 g
  g gs a f g
  
  g g gs as c as gs f g
  ds f f g2.
  
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

