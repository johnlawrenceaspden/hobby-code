\version "2.18.2"
\language "english"

% 1 1 5 5

\header {
  title = ""
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}

melody = \relative c'' {
  \global
  c c g g  
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

