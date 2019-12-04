# Makefile for controlling python virtual environments etc
# See https://github.com/sio/Makefile.venv

default: venv show-venv hello
	echo to activate the virtual environment in a shell
	echo source .venv/bin/activate

hello: venv
	$(VENV)/python hello.py

generators: venv
	$(VENV)/python generators.py


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
