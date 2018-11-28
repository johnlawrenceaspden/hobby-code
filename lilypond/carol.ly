\version "2.18.2"
\language "english"

\header {
  title = "Carol"
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}

melody = \relative c'' {
  \global
% 164571 164551 346571 164551
  c4 a f g b c2.
  c4 a f g g c,2.
  e4 f a g b c2.
  c4 a f g g c,2.
  
}

words = \lyricmode {
  one six four five seven one 
  one six four five five one
  three four six five seven one
  one six four five five one
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
\line{Come all good men and pray}
\line{For Christ is born to - day}
\line{So sing the joy - ful song}
\line{For he will stand no wrong!}
\vspace #0.5

\line{He comes to har - row Hell}
\line{To make all foul things well!}
\line{He rises like the dawn}
\line{Sing out! the Christ is born!}
\vspace #0.5

\line{All men on earth to - day}
\line{Down on your knees and pray}
\line{For Christ is come at last}
\line{And all our woe is past}
\vspace #0.5

\line{}
\line{}
\line{}
\line{}
}}
