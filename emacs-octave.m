# emacs octave test file

# You should just be able to do Ctrl-Meta-x on this line
A=eye(3)
# and it will start octave in another window like so
## >> A=eye(3)
## A =

## Diagonal Matrix

##    1   0   0
##    0   1   0
##    0   0   1

#With GNU Emacs 24.3.1 and GNU Octave, version 3.8.1
# it hangs, this is because they disagree on what the prompt should be
# Creating ~/.octaverc with the line PS1(">> ")
# should fix this:

# cat >~/.octaverc
# PS1(">> ")
