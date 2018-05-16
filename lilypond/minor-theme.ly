\version "2.18.2"


%% 6 5# 6 77
%% 76711
%% 17122
%% 5,5'43

%% 3542
%% 2431

%% 112363216736

\header {
  title = "Minor Theme"
}

global = {
                                %\time 4/4
  \key c \major
  \tempo 4=100
}

melody = \relative c' {
  \global

a8 gis8 a8 b4 b4 r8
b8 a8 b8 c4 c4 r8
c8 b8 c8 d4 d4 r8
g,8 g'8 f8 e2 r8 

e8 g8 f8 d2 r8 
d8 f8 e8 c2 r8

c8 c8 d8 e8 a8 e d c a b e a

  
}

words = \lyricmode {
  % words go here
}

\score {
  <<
    \new Staff { \melody }
    \addlyrics { \words }
  >>
  \layout { }
  \midi { }
}

