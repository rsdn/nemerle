
include ../../../config.mak

NPATH   = ../../../boot
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe
MCS     = mcs

SIMULATOR_SOURCES = environment.n brain.n simulator.n
ANT_RANDOM_SOURCES = random.cs

all: simulator.exe

simulator.exe: $(SIMULATOR_SOURCES) ant-random.dll
	$(COMPILE) -r ant-random.dll -texe -out simulator.exe $(SIMULATOR_SOURCES)

ant-random.dll: $(ANT_RANDOM_SOURCES)
	$(MCS) /target:library /out:ant-random.dll $(ANT_RANDOM_SOURCES)
