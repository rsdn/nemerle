all: prof

.PHONY: prof

prof:
	gcc -fPIC -O2 -W -Wall -shared -o libmono-profiler-x.so profx.c `pkg-config mono --cflags --libs mono`

clean:
	rm -f *.so
