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

harmony = \relative c' {
  \global
  <c e g>4 
  <c e g>
  <g' b d>
  <g b d>
  
  <a c e>4
  <a c e> 
  <g b d>2
  
  <f a c>4
  <f a c>
  <e g b>  
  <e g b>
  
  <d f a>
  <d f a>
  <c e g>2 
  
  r1*4
  
  <c e g>4 
  <c e g>
  <g' b d>
  <g b d>
  
  <a c e>4
  <a c e> 
  <g b d>2
  
  <f a c>4
  <f a c>
  <e g b>  
  <e g b>
  
  <d f a>
  <d f a>
  <c e g>2 
}

harmonywords = \lyricmode {
  I I V V 
  vi vi V
  IV IV iii iii 
  ii ii I
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
