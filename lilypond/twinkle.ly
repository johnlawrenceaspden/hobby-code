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
  <c e g>4 
  <c e g>
  <e g b>
  <g b d>
  
  <a c e>4
  <e a c> 
  <g b d>2
  
  <f a c>4
  <f a c>
  <e g b>  
  <e g b>
  
  <d f a>
  <d f a>
  < e g c>2 
  
  r1*4
  
  <c e g>4 
  <a c e>
  <e' g b>
  <g b d>
  
  <c e a>4
  <a c e> 
  <c, e g>2
 

  <f a c>4
  <f a c e>
  <e g b>  
  <c e g>
  
  <f a c>4
  <g, b d f>
  <g' e c>2 
}

harmonywords = \lyricmode {
  I I iii V 
  vi vic V
  IV IV iii iii 
  ii ii I
  
  I vi iii V 
  vi vic V

  IV "IV7" iii I 
  IV "V7" I
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
