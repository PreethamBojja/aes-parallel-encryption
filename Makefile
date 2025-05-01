main: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 main.mlb

test: *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-type int64 -default-type word64 test.mlb

# Compile ISPC kernels
aes_kernels.o: aes.ispc
	ispc -O2 --target=avx2 --outfile=aes_kernels.o aes.ispc


# Build vectorized MPL binary using ISPC kernels
vector: aes_kernels.o *.sml *.mlb lib-local/*.sml lib-local/*.mlb
	mpl -default-ann 'allowFFI true' -default-type int64 -default-type word64 \
	  -cc-opt aes_kernels.o \
	  main.mlb

.PHONY: main test vector aes_kernels.o
