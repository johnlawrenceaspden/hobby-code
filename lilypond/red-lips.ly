\version "2.18.2"
\language "english"

% 1 1 5 5

\header {
  title = ""
}

global = {
  \time 3/4
  \key c \minor
  \tempo 4=50
}

% transpose {c fs} 

melody = \relative c''
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
\line{more words}
\line{}
\line{}
\line{}
\vspace #0.5

\line{can go here}
\line{}
\line{}
\line{}
\vspace #0.5

\line{}
\line{}
\line{}
\line{}
\vspace #0.5

\line{}
\line{}
\line{}
\line{}
}}


%Red lips are not so red
%As the stained stones kissed by the English dead.
%Kindness of wooed and wooer
%Seems shame to their love pure.
%O Love, your eyes lose lure
%When I behold eyes blinded in my stead!
%
%Your slender attitude
%Trembles not exquisite like limbs knife-skewed,
%Rolling and rolling there
%Where God seems not to care;
%Till the fierce Love they bear
%Cramps them in death's extreme decrepitude.
%
%Your voice sings not so soft, --
%Though even as wind murmuring through raftered loft, --
%Your dear voice is not dear,
%Gentle, and evening clear,
%As theirs whom none now hear
%Now earth has stopped their piteous mouths that coughed.
%
%Heart, you were never hot,
%Nor large, nor full like hearts made great with shot;
%And though your hand be pale,
%Paler are all which trail
%Your cross through flame and hail:
%Weep, you may weep, for you may touch them not. 


