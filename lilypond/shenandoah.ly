 \version "2.18.2"

% 5 | 1 2 3 4 | 6 5 1 7 | 6 5 6 5 | 3 5 5 | 6 6 6 3 5 3 | 2 1 | 2 3 1 3 6 5 | 1 2 3 1 | 2 | 1
% - | I       | I       | IV      | I     | IV          | vi  | V           | vi      | V | I

% - | 5       | 5       | 6       | 5     | 6           | 6   | 7           | 6       | 7 | 5
% - | 3       | 3       | 4       | 3     | 4           | 3   | 5           | 3       | 5 | 3
% - | 1       | 1       | 1       | 1     | 1           | 1   | 2           | 1       | 2 | 1


% 4   432  12         121
% ___|---|---|___|---|---|_______
%         7   5   646     3

% 4   432  12      1   21 1
% ___|---|---|___|---|---|_______
%         7   5   6 6 7    



 \header {
   title = "Shenandoah"
 }
 
 global = {
   %\time 4/4
   \key c \major
   \tempo 4=100
 }
 
 chordNames = \chordmode {
   \global
   c1 | c | f | c | f | a | g | a | g | c
 }
 
 melody = \relative c' {
   \global
   g4 c8 c8 c2 r4 d4 e f a2 g2 r4 c4  b a4~ a2 r4 g4 a g e g4~ g2. r4 g4 a8 a8 a2~ a4 r4 e4 g e d2 c2 r4 d4 e2. r4 c4 e4 a4 g2~ g2~ g4 r4 c4 d e2. r4 c d2 c1
 }
 
 words = \lyricmode {
  O Shen- an- doah, I long to see you. Roll aw- ay, you roll- ing ri- ver. Oh Shen- an- doah, I long to see you. A- way, I'm bound to go. 'Cross the wide Mis- sou- ri 
 }
 
 \score {
   <<
     \new ChordNames \chordNames
     %\new FretBoards \chordNames
     \new Staff { \melody }
     \addlyrics { \words }
   >>
   \layout { }
   \midi { }
 }


 \header{
   title="Shenandoah"
 }