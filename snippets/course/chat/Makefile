#
#  Makefile
#

include ../../../config.mak

NPATH   = ../../../boot
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

all: server client

Packet.dll: packet.n
	$(COMPILE) -tdll -out:$@ $<

Server.dll: server.n Packet.dll
	$(COMPILE) -tdll -r:Packet -out:$@ server.n

server.exe: daemon.n Server.dll
	$(COMPILE) -texe -r:Server -out:$@ daemon.n

client.exe: client.n Packet.dll 
	$(COMPILE) -texe -r:Packet -pkg:glade-sharp -out:$@ client.n

client: Packet.dll client.exe

server: Packet.dll Server.dll server.exe

clean:
	rm -f *.dll *.exe *~
