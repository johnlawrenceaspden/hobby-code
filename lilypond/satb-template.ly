\version "2.18.2"

global = {
  \key c \major
  \time 4/4
}

sopranoVoice = \relative c'' {
  \global
  \dynamicUp
  % Music follows here.
  c4
  
}

verseSopranoVoice = \lyricmode {
  % Lyrics follow here.
  sop
  
}

altoVoice = \relative c' {
  \global
  \dynamicUp
  % Music follows here.
  a4
  
}

verseAltoVoice = \lyricmode {
  % Lyrics follow here.
  alt
  
}

tenorVoice = \relative c' {
  \global
  \dynamicUp
  % Music follows here.
  e4
  
}

verseTenorVoice = \lyricmode {
  % Lyrics follow here.
  tenor
}

bassVoice = \relative c {
  \global
  \dynamicUp
  % Music follows here.
  c4
  
}

verseBassVoice = \lyricmode {
  % Lyrics follow here.
  bass
}

sopranoVoicePart = \new Staff \with {
  instrumentName = "Soprano"
  midiInstrument = "choir aahs"
} { \sopranoVoice }
\addlyrics { \verseSopranoVoice }

altoVoicePart = \new Staff \with {
  instrumentName = "Alto"
  midiInstrument = "choir aahs"
} { \altoVoice }
\addlyrics { \verseAltoVoice }

tenorVoicePart = \new Staff \with {
  instrumentName = "Tenor"
  midiInstrument = "choir aahs"
} { \clef "treble_8" \tenorVoice }
\addlyrics { \verseTenorVoice }

bassVoicePart = \new Staff \with {
  instrumentName = "Bass"
  midiInstrument = "choir aahs"
} { \clef bass \bassVoice }
\addlyrics { \verseBassVoice }

\score {
  <<
    \sopranoVoicePart
    \altoVoicePart
    \tenorVoicePart
    \bassVoicePart
  >>
  \layout { }
  \midi {
    \tempo 4=100
  }
}
