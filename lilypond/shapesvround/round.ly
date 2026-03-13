\version "2.24.2"


europeanHymn = <<
  \new ChoirStaff <<
    \new Staff = "Soprano" {
      \clef treble
      \key c \major
      c'4 e' g' fis' g' a' g' e'
    }
    \new Staff = "Alto" {
      \clef treble
      \key c \major
      a'4 g' f' e' f' g' f' d'
    }
    \new Staff = "Tenor" {
      \clef treble
      \key c \major
      f'4 e' d' c' d' e' d' c'
    }
    \new Staff = "Bass" {
      \clef bass
      \key c \major
      c4 c g f g c g c
    }
  >>
>>

\score {
  \europeanHymn
  \layout { }
}
