\version "2.18.2"

\header {
  title = ""
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}

soprano = \relative c'' {
  \global
  c4
  
}

alto = \relative c' {
  \global
  c4
  
}

tenor = \relative c' {
  \global
  c4
  
}

bass = \relative c {
  \global
  c4
  
}

verseOne = \lyricmode {
  \set stanza = "1."
  hi
  
}

verseTwo = \lyricmode {
  \set stanza = "2."
  ha
  
}

verseThree = \lyricmode {
  \set stanza = "3."
  ho
  
}

\score {
  \new ChoirStaff <<
    \new Staff \with {
      midiInstrument = "choir oohs"
      instrumentName = \markup \center-column { S A }
    } <<
      \new Voice = "soprano" { \voiceOne \soprano }
      \new Voice = "alto" { \voiceTwo \alto }
    >>
    \new Lyrics \with {
      \override VerticalAxisGroup #'staff-affinity = #CENTER
    } \lyricsto "soprano" \verseOne
    \new Lyrics \with {
      \override VerticalAxisGroup #'staff-affinity = #CENTER
    } \lyricsto "soprano" \verseTwo
    \new Lyrics \with {
      \override VerticalAxisGroup #'staff-affinity = #CENTER
    } \lyricsto "soprano" \verseThree
    \new Staff \with {
      midiInstrument = "choir oohs"
      instrumentName = \markup \center-column { T B }
    } <<
      \clef bass
      \new Voice = "tenor" { \voiceOne \tenor }
      \new Voice = "bass" { \voiceTwo \bass }
    >>
  >>
  \layout { }
  \midi { }
}
