# Makefile for controlling python virtual environments etc
# See https://github.com/sio/Makefile.venv

# sudo apt-file update
# apt-file search "javac "
# must be java 8
# sudo apt install openjdk-8-jdk

# this may not be true, uninstalled it and it still works
# but certainly the first time it needed to be done to get past the 'missing cython' stage
# even though cython was in the requirements.txt file 
# must for some reason install system-wide cython
# sudo apt install cython

# a list of buildozer dependencies (https://buildozer.readthedocs.io/en/latest/installation.html#targeting-android)
# sudo apt install git zip unzip openjdk-8-jdk python3-pip autoconf libtool pkg-config zlib1g-dev libncurses5-dev libncursesw5-dev libtinfo5 cmake libffi-dev
# and also (although requirements.txt should cover this) pip3 install --upgrade cython virtualenv

# buildozer apparently needs to be run in a venv, so to get those sorts of things to run you need to activate the environment before using make

default: venv show-venv
	KIVY_VIDEO=ffpyplayer $(VENV)/python main.py
	echo to activate the virtual environment in a shell
	echo source .venv/bin/activate

bin/myapp__armeabi-v7a-0.1-armeabi-v7a-debug.apk: main.py
	echo yo
	$(VENV)/buildozer android debug



deploy: venv bin/myapp__armeabi-v7a-0.1-armeabi-v7a-debug.apk
	$(VENV)/buildozer android deploy run

run: venv bin/myapp__armeabi-v7a-0.1-armeabi-v7a-debug.apk
	$(VENV)/buildozer android deploy run

logcat: venv bin/myapp__armeabi-v7a-0.1-armeabi-v7a-debug.apk
	$(VENV)/buildozer android deploy run logcat

include Makefile.venv

pyflakes: $(VENV)/pyflakes
	$(VENV)/pyflakes *.py

flake8: $(VENV)/flake8
	$(VENV)/flake8 *.py

pipreqs: $(VENV)/pipreqs
	$(VENV)/pipreqs --force --ignore .venv .

Makefile.venv:
	curl \
		-o Makefile.fetched \
		-L "https://github.com/sio/Makefile.venv/raw/v2019.11.22/Makefile.venv"
	echo "048c4a1b9265231db97b4903bb2e835b01e0d84a5b7435d4bb8d5926c99aa7f7 *Makefile.fetched" \
		| sha256sum --check - \
		&& mv Makefile.fetched Makefile.venv



clean:
	rm -rf __pycache__

reallyclean:clean clean-venv
