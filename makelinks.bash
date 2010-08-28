#!/bin/bash
for i in hgrc emacs bashrc inputrc gitconfig; 
    do 
	mv ~/.$i ~/old.$i
	ln -s ~/hobby-code/dot$i ~/.$i; 
done
