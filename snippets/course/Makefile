include ../../config.mak

NPATH   = ../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe -pedantic-lexer


all: swf-calculator.exe backup-tool.exe getmxbyname.exe
	$(MAKE) -C huffman all
	$(MAKE) -C ERA-SMS-Sender all
	$(MAKE) -C chat all

swf-calculator.exe: swf-calculator.n
	$(COMPILE) swf-calculator.n -o swf-calculator.exe -r System.Windows.Forms

backup-tool.exe: backup-tool.n
	$(COMPILE) backup-tool.n -o backup-tool.exe

getmxbyname.exe: getmxbyname.n
	$(COMPILE) getmxbyname.n -o getmxbyname.exe

clean:
	$(MAKE) -C ERA-SMS-Sender clean
	$(MAKE) -C huffman clean
	$(MAKE) -C chat clean
	rm -f *.exe *~
