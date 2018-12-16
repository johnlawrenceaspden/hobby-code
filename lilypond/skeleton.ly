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

melody = \relative c'' {
  \global
  \partial 4 
  c4
  e d e
  \repeat volta 2 {c g g a a }  
  \alternative {{c e g }{g (e c)}}
  a b c d 
}

words = \lyricmode {
  words go here hy -- phen hy -- phen skip _ extender __  ex __ me -- lisma __ _ _
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
