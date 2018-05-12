%=============================================
%   created by MuseScore Version: 1.3
%          Saturday, 12 May 2018
%=============================================

\version "2.12.0"




AvoiceAA = \relative c'{
   \clef treble
    \key c \major 
    \time 4/4 
    <g' c e>4 <a c e> <fis a d> <g b d>      | % 1
    <e a c> <fis a c> <d g b>2      | % 2
}

 

AvoiceBA = \relative c{
     \clef bass
     \key c \major 
    \time 4/4 
    c'4 fis, b e,      | % 1
    a d, g2      | % 2
}

\score { 
    << 
        \context PianoStaff <<
        \set PianoStaff.instrumentName="Piano" 
            \context Staff = ApartA << 
                \context Voice = AvoiceAA \AvoiceAA
                \set Staff.instrumentName = #""
                \set Staff.shortInstrumentName = #""
            >>
            \context Staff = ApartB << 
                \context Voice = AvoiceBA \AvoiceBA
                \set Staff.instrumentName = #""
                \set Staff.shortInstrumentName = #""
            >>
        >> 
   >>
   \layout{}
   \midi{}
}




\version "2.18.2"

\header {
  title = ""
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}

chordNames = \chordmode {
  \global
  c1
  
}

melody = \relative c'' {
  \global
  c4 d e f
  
}

words = \lyricmode {
  
  
}

\score {
  <<
    \new ChordNames \chordNames
    \new FretBoards \chordNames
    \new Staff { \melody }
    \addlyrics { \words }
  >>
  \layout { }
  \midi { }
}
