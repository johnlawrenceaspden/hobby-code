\version "2.18.2"
\language "english"

% 1 1 5 5

\header {
  title = "Remon"
}

global = {
  \time 2/4
  \key f \major
  \tempo 4=100
}

melody = \relative c' {
  \global
  \repeat volta 2 {\partial 4 \tuplet 3/2 {f8 a8 c8} |  
                   a8 bf4 g8 | bf4 \tuplet 3/2 {g8 bf8 d8} |
                   bf8 c4 f,8 | a4 \tuplet 3/2 {f8 a8 c8} |
                   a8 bf4 e8 | g8 g8 \tuplet 3/2 {bf8 bf8 e,8} |
                   g8 f4 a8 | f4 \tuplet 3/2 {a8 a8 a8} |
                   g8 bf4 a8 | e4 \tuplet 3/2 {g8 g8 g8} |
                   f8 a4 a8 | f4 \tuplet 3/2 {f8 a8 c8} |
                   bf8 d4 bf8 | \tuplet 3/2 {g8 g8 bf8} bf8 e,8 |
                   g8 f4 |
                    
                  
  }  
}

words = \lyricmode {
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

