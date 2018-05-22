\version "2.18.2"

\header {
  title = ""
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
c8 \tuplet 3/2 {c4 d4 e4} f2 \tuplet 3/2 {f4 e4 d4} d4. e8 e4. 
c8 c4. c8 \tuplet 3/2 {d4 c4 b4} c2 r4.

% 5 5 5 6 6 5 4 5
% 1 1 2 2 3 3
% 1 1 2 3 4 4 3 2 2 3 3 
% 1 1 1 2 1 7 1

g'8 | g4. g8 \tuplet 3/2 {a4 g4 f4} g2 
c,8 c4. d8 d4. e4  e4
% c c d e f f e d d e e 
% c c c d c b c


}

melodywords = \lyricmode {
  
'Sun -- set and 'even -- ing star,
And 'one clear 'call for 'me!
And 'may there be 'no mo -- an ing 'of the bar,
When 'I put 'ou -- ut to 'sea,
 
When I put ou -- t to sea,
When I put out to sea,
And may there be no moaning of the bar,
When I put out to sea,


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


allwords= \lyricmode{
Sunset and evening star,
And one clear call for me!
And may there be no moaning of the bar,
When I put out to sea,

But such a tide as moving seems asleep,
Too full for sound and foam,
When that which drew from out the boundless deep
Turns again home.

Twilight and evening bell,
And after that the dark!
And may there be no sadness of farewell,
When I embark;

For tho' from out our bourne of Time and Place
The flood may bear me far,
I hope to see my Pilot face to face
When I have crost the bar.
}