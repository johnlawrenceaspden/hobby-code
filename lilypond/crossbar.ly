\version "2.18.2"

\header {
  title = "Crossing the Bar"
}

global = {
  \time 4/4
  \key c \major
  \tempo 4=120
}


melody = \relative c' {
  \global
  \clef treble

% 3 4 3 2 2 3 
% 1 1 2 2 3 3
% 1 1 2 3 4 4 3 2 2 3 3
% 1 1  1 2 1 7 1
e2 f4. e8 d4 d4 e4. |
c8 c4. d8 d4. e8 e4.  
c8 \tuplet 3/2 {c4 d4 e4} f2 \tuplet 3/2 {f4 (e4 d4)} d4. e8 e4. 
c8 c4. c8 \tuplet 3/2 {d4 (c4) b4} c2 r4.

% 5 5 5 6 6 5 4 5
% 1 1 2 2 3 3
% 1 1 2 3 4 4 3 2 2 3 3 
% 1 1 1 2 1 7 1

g'8 | g4. g8 \tuplet 3/2 {a4 (g4) f4} g2 r4. 
c,8 c4. d8 d4. e8 e4.  
c8 \tuplet 3/2 {c4 d4 e4} f2 \tuplet 3/2 {f4 (e4 d4)} d4. e8 e4. 
c8 
c4. c8 \tuplet 3/2 {d4 (c4) b4} c4. 

% 11235 
% 43223 
% 112233 
% 11235 
% 44223 
% 12217 1

c8 c8 d8 e8 g8

f4. (e8) d4. d8 (e4.)

c8 c4. d8 d4. e8 e4.
c8 c8 d8 e8 g8 

f4. e8 d4. d8 e2 
c4. c8  \tuplet 3/2 {d4 (c b)}  c1

% 556545
% 12233
% 1123543223
% 122171

g'4. g8 \tuplet 3/2 {a4 (g4 f4)} g2 r2 
c,8 c4. d8 d4. e8 e4.  
c8 \tuplet 3/2 {c4 d4 e4} f2 \tuplet 3/2 {f4 (e4 d4)} d4. e8 e4. 
c8 
c4. c8 \tuplet 3/2 {d4 (c4) b4} c4. 




}

melodywords = \lyricmode {
  
Sun -- set and even -- ing star,
And one clear call for me!
And may there be no moaning of the bar,
When I put out to 'sea,
 
When I put out to sea,
When I put out to sea,
And may there be no moaning of the bar,
When 

I put out to sea,

But such a tide as moving seems asleep,
Too full for sound and foam,
When that which drew from out the bound -- less deep
Turns a -- gain home.

Turns a -- gain home.
Turns again home.
When that which drew from out the boundless deep
Turns again home.


}

harmony = \relative c {
  \clef bass
  \global

}

harmonywords = \lyricmode {

}

\score {
  <<
    \new Staff \with{midiInstrument="voice oohs"} { \melody }
    \addlyrics { \melodywords }
    \new Staff \with{midiInstrument=cello} { \harmony }
    \addlyrics { \harmonywords }
  >>
  \layout { }
  \midi { }
}


\markup { \column{
\line{Sunset and evening star,}
\line{And one clear call for me!}
\line{And may there be no moaning of the bar,}
\line{When I put out to sea,}
\vspace #0.5

\line{But such a tide as moving seems asleep,}
\line{Too full for sound and foam,}
\line{When that which drew from out the boundless deep}
\line{Turns again home.}
\vspace #0.5

\line{Twilight and evening bell,}
\line{And after that the dark!}
\line{And may there be no sadness of farewell,}
\line{When I embark;}
\vspace #0.5

\line{For tho' from out our bourne of Time and Place}
\line{The flood may bear me far,}
\line{I hope to see my Pilot face to face}
\line{When I have crost the bar.}
}}

