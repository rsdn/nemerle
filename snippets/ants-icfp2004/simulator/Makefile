
include ../../../config.mak

NPATH   = ../../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

SIMULATOR_SOURCES = environment.n brain.n simulator.n random.n

all: simulator.exe

simulator.exe: $(SIMULATOR_SOURCES)
	$(COMPILE) -texe -out simulator.exe $(SIMULATOR_SOURCES)

clean:
	rm -fr simulator.exe
