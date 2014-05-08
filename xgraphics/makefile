FAVOURITE := simplest

run: $(FAVOURITE)
	./$< -root

install: $(FAVOURITE)
	rm ~/bin/$(FAVOURITE)
	ln simplest ~/bin/$(FAVOURITE)
	@echo "add "
	@echo "$(FAVOURITE)" "-root                              \\\n\\"
	@echo "to the ~/.xscreensaver file if it's not already there"

%: %.c vroot.h
	gcc -std=gnu99 -o $@ $< -lX11

SRCS := $(wildcard *.c)
PRGS := $(patsubst %.c,%,$(SRCS))

all:$(PRGS)

clean:
	-rm -f $(PRGS)

.PHONY: install clean all run
