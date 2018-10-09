\version "2.18.2"
\language "english"

\header {
  title = "Gwine Follow"
}

global = {
  \time 2/4
  \key c \major
  \tempo 4=25
}

melody = {\relative c'' {
  \global
  \repeat volta 2 {\partial 8  c16 c | c e8 e16 g, g e8 | f16 g8. b8 b8 | c16 d8. r8 b8 | c16 d8. r8 b16 b | c16 e8 e16 g, a e e | f16 g8. f'8 f8 e e d16 d8. c4 r8}
  \repeat volta 2 {\partial 8  g8 |  c d e f16 f16 g g g e f8 f16 f16 e e e e e8 c16 e16 d e d b g8 g8 c8 d8 e8 f16 f16 g g g e f8 f16 f e8 e d16 d8. c4 r8 }
}}

words = \lyricmode {}

\score {
  <<
    \new Staff { \melody }
    \addlyrics { \words }

  >>
  \layout { }
}

\score {
  <<
    \new Staff \with{midiInstrument=violin} \transpose c f, { \unfoldRepeats \melody }
  >>
  \midi { }
}

\markup { \column{
\line{Titty Mary, you know I gwine follow, I gwine follow, gwine follow}
\line{Brudder William, you know I gwine to follow, For to do my Fader will.}
\line{'Tis well and good I'm acomin' here tonight, I'm acomin' here tonight,}
\line{I'm acomin' here tonight.}
\line{'Tis well and good, I'm acomin' here tonight, For to do my Fader will}

}}
