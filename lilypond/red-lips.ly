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

melody = \relative c'' {
  \global

% 5    3b    3b  4    3b 2 1 7    5,  5,  1       2      3b     2  1   7  5,   1
  g4   ef8   ef  f16  ef d c b8   g16  g  c8       d      ef     d16  c   b8  g   c4

% 5     3b    3b 4   3b  2  1    7   5,     1     2  3b    4    5
  g'4   ef8   ef f16 ef  d (c)   b8  g      c     d  ef    f    g4


% 5    3b   4  3b 2  1  7    5,   1 2  3b   4    5    1   2 1 7  1
  g    ef   f16  ef d  c  b8    g    c d  ef   f    g    c,   d16 c b8  c4


% 5      5#  6#  4     2   3b  5   2     5(7,?)    1('?)
  g'4     af8  bf  f4.     d8   ef8  g8   d8     g8  c,4

}

words = \lyricmode {
% 1=f#
% -   -        -          -          -              -             -       -
 Red lips are no- ot s- o red as the stained stones kissed by the Eng- lish dead.
% 5   3b   3b  4  3b 2 1 7   5, 5,  1       2      3b     2  1   7  5,   1

% -    -       -           -           -        -          -
 Kind- ness of woo- ed and wooer seems shame to their love pure.
%5     3b   3b 4 3b  2 1 7     5,     1     2  3b    4    5

%-    -    -             -         -     -         -         -       -
 Love your ey- es lo- se lure when I be- hold eyes blin- ded i- n my stead.
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
  \layout {     
%    \context {
%      \Staff
%      whichBar = #""
%      \remove Time_signature_engraver
%    }
  }
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
