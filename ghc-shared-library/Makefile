GHC=ghc -c -fforce-recomp
GHC_VERSION=$(shell ghc --numeric-version)

all: plugin.so host

clean:
	rm -f *.so *.o *.hi *_stub.h host

Plugin.o: Plugin.hs
	$(GHC) -dynamic -fPIC Plugin.hs

plugin_wrapper.o: Plugin.o plugin_wrapper.c
	$(GHC) -dynamic -fPIC plugin_wrapper.c

plugin.so: Plugin.o plugin_wrapper.o
	$(GHC) -dynamic -shared -lHSrts_thr-ghc$(GHC_VERSION) Plugin.o plugin_wrapper.o -o plugin.so

host: host.c
	gcc -ldl host.c -o host