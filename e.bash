#!/bin/bash

# The emacs server should create
# $XDG_RUNTIME_DIR/$UID/emacs/server
# if that's not defined then it defaults to
# /run/user/$UID/emacs/server.
# prior to emacs 27, it was:
# /tmp/emacs$UID/server

echo "trying to open: " $@;

SERVERSOCKET1=${XDG_RUNTIME_DIR}/emacs/server;
SERVERSOCKET2=/run/user/${UID}/emacs/server;
SERVERSOCKET3=/tmp/emacs${UID}/server;

USE_CLIENT=false

echo " checking: " $SERVERSOCKET1 "for emacs server ";
if [ -e $SERVERSOCKET1 ]; then
    echo "found, using emacsclient";
    USE_CLIENT=true ;
fi
echo " checking: " $SERVERSOCKET2 "for emacs server ";
if [ -e $SERVERSOCKET2 ]; then
    echo "found, using emacsclient";
    USE_CLIENT=true ;
fi
echo " checking: " $SERVERSOCKET3 "for emacs server ";
if [ -e $SERVERSOCKET3 ]; then
    echo "found, using emacsclient";
    USE_CLIENT=true ;
fi

if $USE_CLIENT ; then
    echo "trying to use emacsclient";
    emacsclient --no-wait "$@";
else
    echo "starting emacs: make tea...";
    emacs --geometry 100x30 --fullscreen --no-splash "$@" & disown;
fi
