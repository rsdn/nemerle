SOURCES = Main.n Person.n Parser.n HeaderParameter.n MailParser.n ContentType.n \
          Attachment.n Encoder.n MailInfo.n MailInfoArray.n MailHeader.n Informer.n \
          Message.n NakedNetwork.n CommandException.n POP3Naked.n POP3Client.n POP3EraClient.n \
          EraHTTP.n AccountDatabase.n EraSMSSender.n

include ../../../config.mak

NPATH   = ../../../ncc/out.stage3
MONO    = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(MONO) $(NPATH)/ncc.exe

all: $(SOURCES)
	$(COMPILE) $(SOURCES) -o SMS.exe -r System.Windows.Forms -r System.Data

clean:
	rm -f SMS.exe
