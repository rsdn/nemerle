
include ../../../config.mak

NPATH   = .
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

SIMULATOR_SOURCES = environment.n brain.n simulator.n
ANT_RANDOM_SOURCES = random.cs

all: simulator.exe

simulator.exe: $(SIMULATOR_SOURCES) ant-random.dll
	$(MONO) ./ncc.exe -r ant-random.dll -texe -out simulator.exe $(SIMULATOR_SOURCES)

ant-random.dll: $(ANT_RANDOM_SOURCES)
	csc /target:library /out:ant-random.dll $(ANT_RANDOM_SOURCES)
