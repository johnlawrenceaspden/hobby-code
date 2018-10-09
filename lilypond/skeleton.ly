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
  \repeat volta 2 {\partial 4 c4 | c g g}  
}

words = \lyricmode {
  words go here, here
}

\score {
  <<
    \new Staff { \melody }
    \addlyrics { \words }
  >>
  \layout { }
}

\score {
  <<
    \new Staff \with{midiInstrument=violin} { \unfoldRepeats \melody }
  >>
  \midi { }
}

\markup { \column{
\line{more words}
\line{}
\line{}
\line{}
\vspace #0.5

\line{can go here}
\line{}
\line{}
\line{}
\vspace #0.5

\line{}
\line{}
\line{}
\line{}
\vspace #0.5

\line{}
\line{}
\line{}
\line{}
}}
