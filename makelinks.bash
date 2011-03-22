#!/bin/bash
for i in hgrc emacs bashrc inputrc gitconfig; 
    do 
	echo replacing $i
	mv ~/.$i ~/old.$i
	ln -s ~/hobby-code/dot$i ~/.$i; 
done

echo replacing .ssh/authorized_keys
mv ~/.ssh/authorized_keys ~/.ssh/old.authorized_keys
ln -s ~/hobby-code/authorized_keys ~/.ssh/authorized_keys
