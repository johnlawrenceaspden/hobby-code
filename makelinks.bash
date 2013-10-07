#!/bin/bash
for i in hgrc emacs bashrc inputrc gitconfig profile xinitrc; 
    do 
	echo replacing $i
	mv ~/.$i ~/old.$i
	ln -s ~/hobby-code/dot$i ~/.$i; 
done

echo replacing ~/.ssh/authorized_keys
mv ~/.ssh/authorized_keys ~/.ssh/old.authorized_keys
ln -s ~/hobby-code/authorized_keys ~/.ssh/authorized_keys

mkdir -p ~/.lein/globaluser

echo replacing ~/.lein/profiles.clj
mv ~/.lein/profiles.clj ~/.lein/old.profiles.clj
ln -s ~/hobby-code/hidefromleiningen/profiles.clj ~/.lein/profiles.clj

echo replacing ~/.lein/globaluser/user.clj
mv ~/.lein/globaluser/user.clj ~/.lein/globaluser/old.user.clj
ln -s ~/hobby-code/hidefromleiningen/user.clj ~/.lein/globaluser/user.clj

echo replacing ~/gitignore
mv ~/gitignore ~/old.gitignore
ln -s ~/hobby-code/gitignore ~/gitignore
