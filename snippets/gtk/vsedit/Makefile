include ../../../config.mak

NPATH   = ../../../boot
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

vsedit.exe: vsedit.n
	$(COMPILE) -pkg gtk-sharp -texe -out:$@ $<

run: vsedit.exe
	$(MONO) vsedit.exe

clean:
	rm -f vsedit.exe
