
include ../../../config.mak

NPATH   = ../../../boot
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

SIMULATOR_SOURCES = environment.n brain.n simulator.n random.n

all: simulator.exe

simulator.exe: $(SIMULATOR_SOURCES)
	$(COMPILE) -r ant-random.dll -texe -out simulator.exe $(SIMULATOR_SOURCES)

