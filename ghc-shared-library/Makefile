GHC=ghc -c -fforce-recomp

all: plugin.so

clean:
	rm -f *.so *.o *.hi *_stub.h host

Plugin.o: Plugin.hs
	$(GHC) -dynamic -fPIC Plugin.hs

plugin_wrapper.o: Plugin.o plugin_wrapper.c
	$(GHC) -dynamic -fPIC plugin_wrapper.c

plugin.so: Plugin.o plugin_wrapper.o
	$(GHC) -dynamic -shared -lHSrts_thr-ghc8.8.3 Plugin.o plugin_wrapper.o -o plugin.so


host: host.c
	gcc -ldl host.c -o host