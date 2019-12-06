# Makefile for controlling python virtual environments etc

# default action is to set up the virtual environment, run hello world
default: venv show-venv hello
	@echo to activate the virtual environment in a shell
	@echo source .venv/bin/activate
	@echo or type make bash

# See https://github.com/sio/Makefile.venv
include Makefile.venv

# Rather than listing every target like:
# hello: venv
#     $(VENV)/python hello.py
# Better to say that any .py file is a potential target and run it 

ALLPYS := $(patsubst %.py,%,$(wildcard *.py))

$(ALLPYS): venv
	$(VENV)/python $@.py

.PHONY: $(ALLPYS)



# various python utilities can be run (and auto-installed at need by developers without needing to
# be in the requirements file) like so:

pyflakes: $(VENV)/pyflakes
	$(VENV)/pyflakes *.py

flake8: $(VENV)/flake8
	$(VENV)/flake8 *.py

fuckflake8:
	$(VENV)/pip uninstall flake8

pipreqs: $(VENV)/pipreqs
	$(VENV)/pipreqs --force --ignore .venv . 

# If we've lost the magic file itself, can get that from github (and verify with checksum) with:

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
