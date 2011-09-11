#examples of pattern substitution
$(warning $(subst ee,EE,feet))
$(warning $(patsubst f%t,F%T,feeytt))
$(warning $(call EXTRACT,f%t,F%T,feeytt))


TARGETS=abc.hex def.flap

EXTRACT=$(patsubst $1,$2,$(filter $1,$3))

DOOM=$(call EXTRACT,%.hex,%.bin, $(TARGETS))
SARNATH=$(patsubst %.hex,%.bin,$(TARGETS))

$(warning what : $$$$$$(DOOM))
$(warning sarnath   : $(SARNATH))
$(warning doom   : $(DOOM))
$(warning targets: $(TARGETS))
$(warning extracts $(EXTRACT))

default:$(warning $(DOOM)) $(DOOM)
	$(warning $(DOOM))
	echo making $(DOOM)