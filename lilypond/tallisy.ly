\version "2.18.2"
\language "english"


\header {
  title = "a tallisy sort of thing"
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=100
}

melody = \relative c' {
  \global

  c4 c g' g ds f f g2.
  
  g4 g g af f g2.

  r1
  
  c,4 c g' g ds f f g2.
  
  g4 g g af f g2.

  
  % c,4 c g' g ds f f e2.
  
  
  g4 g gs as2 c4 as gs f g2.
  ds4 f f g2.

  g4 g gs as2 c4 as gs f g2.
  ds4 f f e2.
  
}

words = \lyricmode {
  I gave my love a pea -- cock's wings
  That nev -- er would de -- cay
  I gave my love so ma -- ny things
  That she has thrown aw -- ay
  
  I gave my love all that I could make
  She hurt and spoiled
  I gave my love all the world ent -- ire
  She made it vile.  
}

\score {
  <<
    \new Staff \with{midiInstrument=violin} { \melody }
    \addlyrics { \words }
  >>
  \layout { }
  \midi { }
}

\markup { \column{
\line{  I gave my love a peacock's wings }
\line{  That never would decay. }
\line{  I gave my love so many things }
\line{  That she has thrown away. }
\vspace #0.5
\line{  I gave my love all that I could make }
\line{  She hurt and spoiled. }
\line{  I gave my love all the world entire }
\line{  She made it vile.   }
\vspace #0.5
\line{ And when I sang my great song of joy }
\line{ She snarled and spat. }
\line{ And so I sang to the moon and stars }
\line{ And left the world. }



}}
