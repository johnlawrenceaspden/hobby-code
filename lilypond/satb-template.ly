\version "2.18.2"
\language "english"

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
  
}

verseAltoVoice = \lyricmode {
  % Lyrics follow here.
  
}

tenorVoice = \relative c' {
  \global
  \dynamicUp
  % Music follows here.
  
}

verseTenorVoice = \lyricmode {
  % Lyrics follow here.
  
}

bassVoice = \relative c {
  \global
  \dynamicUp
  % Music follows here.
  
}

verseBassVoice = \lyricmode {
  % Lyrics follow here.
  
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
