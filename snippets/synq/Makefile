include ../../config.mak

NPATH   = ../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

all: synq.n
	$(COMPILE) synq.n -o synq.exe

clean:
	rm -f synq.exe
