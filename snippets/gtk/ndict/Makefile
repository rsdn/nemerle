include ../../../config.mak

NPATH   = ../../../boot
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

ndict.exe: dict.n DictBuffer.n
	$(COMPILE) -pkg glade-sharp -texe -out:$@ *.n

run: ndict.exe
	$(MONO) ndict.exe

clean:
	rm -f ndict.exe
