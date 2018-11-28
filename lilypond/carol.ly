\version "2.18.2"
\language "english"

\header {
  title = "Carol"
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=200
}

melody = \relative c'' {
  \global
% 164571 164551 346571 164551
  c4 a f g b c2.
  c4 a f g g c,2.
  e4 f a g b c2.
  c4 a f g g c,2.
  r1 r1
  c'4 a4. f8 g4 b4 c2.
  
}

words = \lyricmode {
  (A)one six four five seven one 
  (B)one six four five five one
  (C)three four six five seven one
  (D)one six four five five one
  
  (E)one six four five seven one
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
\line{A Come all good men and pray}
\line{B For Christ is born to - day}
\line{C So sing the joy - ful song}
\line{D For he will stand no wrong!}
\vspace #0.5

\line{A He comes to har - row Hell}
\line{A To make all foul things well!}
\line{B He rises like the dawn}
\line{E Sing out! for Christ is born!}
\vspace #0.5

\line{B All men on earth to - day}
\line{C Down on your knees and pray}
\line{B The Christ is come at last}
\line{C And all our woe is past!}
\vspace #0.5

\line{}
\line{}
\line{}
\line{}
}}
