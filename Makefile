.PHONY: all clean

ISPC        := ispc
# enable DWARF debug info and disable optimizations for easier stepping
ISPC_FLAGS  := -g -O0 --target=sse2-i32x4 --PIC

CC          := gcc
# Emit debug symbols in the shared library
CC_FLAGS    := -shared -fPIC -g

all: vector

aes_kernels.o: aes.ispc
	$(ISPC) $(ISPC_FLAGS) --outfile=$@ $<

libaes_kernels.so: aes_kernels.o
	$(CC) $(CC_FLAGS) $< -o $@

vector: libaes_kernels.so \
        main_vectorised.sml \
        aes_vectorised.sml \
        main.mlb \
        lib-local/*.sml \
        lib-local/*.mlb
	mpl \
	  -default-ann 'allowFFI true' \
	  -default-type int64 -default-type word64 \
	  -link-opt -L. \
	  -link-opt -Wl,-rpath,'$$ORIGIN' \
	  -link-opt -laes_kernels \
	  -output $@ \
	  main.mlb

clean:
	rm -f aes_kernels.o libaes_kernels.so vector
