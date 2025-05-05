# aes-parallel-encryption

# Sequential AES-128 Encryption Benchmark

The **sequential CPU implementation of AES-128 encryption** in **ECB mode**, designed as a benchmark for the `parallel-encryption-aes` project.

It processes a 100MB binary file block-by-block using the [tiny-AES-c](https://github.com/kokke/tiny-AES-c) library, and measures total encryption time and throughput. This is done by doing the following steps:

    cd sequential-aes
    gcc -std=c99 -O3 main.c aes.c -o aes_benchmark
    ./aes_benchmark

The dataset used is 100MB file test.bin in the sequential-aes folder.

We used a common key for all encryptions i.e 2b7e151628aed2a6abf7158809cf4f3c
# Steps to encrypt a file 

    make vector
    ./vector <filepath>

This would print the encryption in the stdout.

---
