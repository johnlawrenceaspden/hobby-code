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

a gis a b b 
b ab c c 
c b c d d 
g ,g 'f e 

e g f d 
d f e c 

c c d e a e d c a b e a

  
}

words = \lyricmode {
  words go here
}

\score {
  <<
    \new Staff { \melody }
    \addlyrics { \words }
  >>
  \layout { }
  \midi { }
}

