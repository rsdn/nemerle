#
#  Makefile
#

include ../../../config.mak

NPATH   = ../../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe
NO_GLADE= $(shell if (pkg-config --exists glade-sharp 2>/dev/null >/dev/null) ; then echo false ; else echo true ; fi)

all: server client

Packet.dll: packet.n
	$(COMPILE) -tdll -out:$@ $<

Server.dll: server.n Packet.dll
	$(COMPILE) -tdll -r:Packet -out:$@ server.n

server.exe: daemon.n Server.dll
	$(COMPILE) -texe -r:Server -out:$@ daemon.n

client.exe: client.n Packet.dll 
	$(NO_GLADE) || $(COMPILE) -texe -r:Packet -pkg:glade-sharp -out:$@ client.n

client: Packet.dll client.exe

server: Packet.dll Server.dll server.exe

clean:
	rm -f *.dll *.exe *~

