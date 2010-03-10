#!/bin/bash
for i in hgrc emacs bashrc inputrc gitconfig; do ln -s ~/hobby-code/dot$i ~/.$i; done
