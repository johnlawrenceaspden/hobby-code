\version "2.18.2"
\language "english"

\header {
  title = "Red Lips"
}

global = {
  \time 3/4
  \key c \minor
  \tempo 4=50
}

melody = \transpose c fs \relative c''
{
  \global

  \repeat volta 4 {
% 5    3b    3b  4    3b 2 1 7    5,  5,  1       2      3b     2  1   7  5,   1
  g4   ef8   ef  f16  (ef) d (c) b8   g16  g  c8       d      ef     d16  c   b8  (g)   c4

% 5     3b    3b 4   3b  2  1    7   5,     1     2  3b    4    5
  g'4   (ef8)   ef f16 (ef)  d (c)   b8  g      c     d  ef    f    g4
  
  % 5    3b   4  3b 2  1  7    5,
  g    ef   f16  (ef) d  (c)  b8 
  }
\alternative{{
%  1 2  3b   4    5    1   2 1 7  1
   g    c d  (ef)   f    g    (c,)   d16 (c) b8  c4
}
{
% 5      5#  6#  4     2   3b  5   2     5(7,?)    1('?)
  g'4     af8  bf  f4.     d8   ef8  g8   d8     g8  c,4
}
}}

words = \lyricmode {
% 1=f#
% -   -        -          -          -              -             -       -
 Red lips are not so red as the stained stones kissed by the English dead.
% 5   3b   3b  4  3b 2 1 7   5, 5,  1       2      3b     2  1   7  5,   1

% -    -       -           -           -        -          -
 Kindness of wooed and wooer seems shame to their love pure.
%5     3b   3b 4 3b  2 1 7     5,     1     2  3b    4    5

%-    -    -             -         -     -         -         -       -
 Love your eyes lose lure when I behold eyes blinded in my stead.
% 5    3b   4  3b 2  1   7    5,   1 2   3b   4    5     1   2 1 7  1

% -  -    -   -   - -   - -   -   -   -     -    - 
  Weep,   you may weep,   for you may touch them not. 
% 5     5#  6#  4     2   3b  5   2     5(7,?)    1('?)

}

\score {
  <<
    \new Staff { \melody }
    \addlyrics { \words }
  >>
  \layout {   }
}

\score {
  <<
    \new Staff \with{midiInstrument=violin} { \unfoldRepeats \melody }
  >>
  \midi { }
}

\markup { \column{
  \line{Red lips are not so red}
  \line{As the stained stones kissed by the English dead.}
  \line{Kindness of wooed and wooer}
  \line{Seems shame to their love pure.}
  \line{O Love, your eyes lose lure}
  \line{When I behold eyes blinded in my stead!}

\vspace #0.5
\line{Your slender attitude}
\line{Trembles not exquisite like limbs knife-skewed,}
\line{Rolling and rolling there}
\line{Where God seems not to care;}
\line{Till the fierce Love they bear}
\line{Cramps them in death's extreme decrepitude.}
\vspace #0.5
\line{Your voice sings not so soft, --}
\line{Though even as wind murmuring through raftered loft, --}
\line{Your dear voice is not dear,}
\line{Gentle, and evening clear,}
\line{As theirs whom none now hear}
\line{Now earth has stopped their piteous mouths that coughed.}
\vspace #0.5
\line{Heart, you were never hot,}
\line{Nor large, nor full like hearts made great with shot;}
\line{And though your hand be pale,}
\line{Paler are all which trail}
\line{Your cross through flame and hail:}
\line{Weep, you may weep, for you may touch them not. }
}}




