NCC     = ../../boot/ncc.exe
SOURCES = httpd.n config.n logger.n

httpd.exe: $(SOURCES)
	$(NCC) -texe -r:System.Xml -out:$@ $(SOURCES)

clean:
	rm -f httpd.exe
