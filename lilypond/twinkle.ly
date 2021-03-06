\version "2.18.2"

\header {
  title = ""
}

global = {
  \time 1/1
  \key c \major
  \tempo 4=120
}


melody = \relative c' {
  \global
  c4 c g' g 
  a a g2
  f4 f e e 
  d d c2

%  5 5 4 4 3 3 2
%  5 5 4 4 3 3 2

  g'4 g f f e e d2
  g4 g f f e e d2
  
%  1 1 5 5 6 6 5
%  4 4 3 3 2 2 1

  c4 c g' g a a g2
  f4 f e e d d c2
  
}

melodywords = \lyricmode {
  one one five five six six five
  four four three three two two one
  five five four four three three two
  five five four four three three two
  one one five five six six five
  four four three three two two one
}

harmony = \relative c {
  \clef bass
  \global
  c4 
  e
  e
  c
  
  f4
  e8 f8
  c2
  
  f4
  <f>
  <a c> 
  d
  
  <a>
  d8 e8
  <g,>2 
  
  r1*4
 
  c,4 
  e
  e
  c
  
  f4
  e8 f8
  c2

  <a' c>4
  <a f>
  <g b>  
  <c g>
  
  <f, a>4
  <a, c e>8
  <g b d f>8
  <g' e c>2 
}

harmonywords = \lyricmode {
  one three three one  
  four three four one
  four four iii I 
  ii ii I
  
  I vi iii V 
  vi vic V

  IV "IV7" iii I 
  IV "V7" _ I
}

\score {
  <<
    \new Staff \with{midiInstrument=violin} { \melody }
    \addlyrics { \melodywords }
    \new Staff \with{midiInstrument=cello} { \harmony }
    \addlyrics { \harmonywords }
  >>
  \layout { }
  \midi { }
}
