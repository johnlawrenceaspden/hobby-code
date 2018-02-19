 \header{
   title="Shenandoah"
 }


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



 \score{
   \header {   piece="Shenandoah"}
 <<
  \time 4/4
  \chords {
    c1 | c | f | c | f | a | g | a | g | c
  }
 
  \relative c'' {
      \key c \major
      {g | c d e 4 | a g c b | a g a g | e g g | a a a e g e | d c | d e c e a g | c d e c | d | c}
  }
 >>
 }