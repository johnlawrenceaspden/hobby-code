#!/bin/bash

FLUFFY="$@";
SERVERSOCKET=/tmp/emacs${UID}/server;
echo "trying to open: " $FLUFFY;
echo " checking: " $SERVERSOCKET "for emacs server ";
if [ -e $SERVERSOCKET ]; then
    echo "using emacsclient";
    emacsclient -n $FLUFFY;
else
    echo "starting emacs: make tea...";
    emacs --geometry 10x10 --fullscreen --no-splash $FLUFFY & disown;
fi
