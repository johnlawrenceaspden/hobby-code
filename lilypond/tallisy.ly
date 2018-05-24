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

melody = \relative c'' {
  \global
  c c g g ds fs fs gs 
  gs a fs fs
  e fs fs gs
  c c gs gs a b cs b a fs gs
  gs gs a b cs b a fs gs
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

