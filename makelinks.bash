#!/bin/bash
for i in hgrc emacs bashrc inputrc gitconfig profile xmodmaprc; 
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

echo making ~/emacs.d/tmp
mkdir -p ~/emacs.d/tmp

echo making ~/bin
mkdir -p ~/bin

echo replacing ~/bin/e.bash 
mv ~/bin/e.bash ~/bin/old.e.bash
ln -s ~/hobby-code/e.bash ~/bin/e.bash

echo replacing ~/bin/fix 
mv ~/bin/fix ~/bin/old.fix
ln -s ~/hobby-code/fix ~/bin/fix
