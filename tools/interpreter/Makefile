TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe -q -debug

all:	evallib mainexe

evallib:
	$(NCC) -r Nemerle.Compiler.dll -t library -o eval.dll eval.n

mainexe:
	$(NCC) -r eval.dll main.n

clean:
	rm *.dll *.exe *~
