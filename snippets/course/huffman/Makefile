include ../../../config.mak

NPATH   = ../../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

all: kompres.exe dekompres.exe

kompres.exe:
	$(COMPILE) kompres.n -o kompres.exe -r System.Windows.Forms

dekompres.exe:
	$(COMPILE) dekompres.n -o dekompres.exe -r System.Windows.Forms

clean:
	rm -f kompres.exe dekompres.exe
