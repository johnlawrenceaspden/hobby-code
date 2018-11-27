\version "2.18.2"
\language "english"

% 1 1 5 5

\header {
  title = ""
}

global = {
  \time 4/4
  \key c \minor
  \tempo 4=100
}

% transpose {c fs} 

melody = \relative c'' {
  \global

% 5   3b   3b  4  3b 2 1 7   5, 5,  1       2      3b     2  1   7  5,   1
  g   ef   ef  f  ef d c b   g  g   c       d      ef     d  c   b  g   c

% 5   3b   3b 4 3b  2 1 7     5,     1     2  3b    4    5
  g'   ef   ef f ef  d (c) b  g      c     d  ef    f    g


% 5    3b   4  3b 2  1  7    5,   1 2  3b   4    5    1   2 1 7  1
  g    ef   f  ef d  c  b    g    c d  ef   f    g    c,   d c b  c


% 5      5#  6#  4     2   3b  5   2     5(7,?)    1('?)
  g'     af  bf  f     d   ef  g   d     g         c,

}

words = \lyricmode {
% 1=f#
% -   -        -         -          -              -             -       -
 Red lips are no- ot s- o red as the stained stones kissed by the Eng- lish dead.
% 5   3b   3b  4  3b 2 1 7   5, 5,  1       2      3b     2  1   7  5,   1

% -   -       -         -           -        -          -
 Kind- ness of woo- ed and wooer seems shame to their love pure.
%5     3b   3b 4 3b  2 1 7     5,     1     2  3b    4    5

% -    -    -           -         -    -         -        -      -
 Love your ey- es lo- se lure when I be- hold eyes blin- ded i- n my stead.
% 5    3b   4  3b 2  1   7    5,   1 2   3b   4    5     1   2 1 7  1


Weep,   you may weep, for you may touch them not. 
% 5     5#  6#  4     2   3b  5   2     5(7,?)    1('?)

}

\score {
  <<
    \new Staff { \melody }
    \addlyrics { \words }
  >>
  \layout { }
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
