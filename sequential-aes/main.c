#define _POSIX_C_SOURCE 199309L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include "aes.h"

#define BLOCK_SIZE 16
#define KEY_SIZE 16

int main() {
    FILE *file = fopen("../input_100000.hex", "rb");
    if (!file) {
        perror("fopen");
        return 1;
    }

    // Determine file size
    fseek(file, 0, SEEK_END);
    size_t filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (filesize % BLOCK_SIZE != 0) {
        printf("File size must be a multiple of 16 bytes (AES block size).\n");
        fclose(file);
        return 1;
    }

    // Allocate buffer
    uint8_t *data = malloc(filesize);
    if (!data) {
        perror("malloc");
        fclose(file);
        return 1;
    }

    fread(data, 1, filesize, file);
    fclose(file);

    // AES-128 key
    uint8_t key[KEY_SIZE] = {
        0x2b, 0x7e, 0x15, 0x16,
        0x28, 0xae, 0xd2, 0xa6,
        0xab, 0xf7, 0x15, 0x88,
        0x09, 0xcf, 0x4f, 0x3c
    };

    struct AES_ctx ctx;
    AES_init_ctx(&ctx, key);

    // Start timing
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    // Encrypt in-place, block by block
    for (size_t i = 0; i < filesize; i += BLOCK_SIZE) {
        AES_ECB_encrypt(&ctx, data + i);
    }

    // End timing
    clock_gettime(CLOCK_MONOTONIC, &end);
    double elapsed = (end.tv_sec - start.tv_sec) +
                     (end.tv_nsec - start.tv_nsec) / 1e9;

    printf("Encrypted %zu bytes in %.6f seconds (%.2f MB/s)\n",
           filesize, elapsed, (filesize / 1e6) / elapsed);

    // (Optional) write encrypted output to file
    FILE *out = fopen("test_encrypted.bin", "wb");
    if (out) {
        fwrite(data, 1, filesize, out);
        fclose(out);
    }

    free(data);
    return 0;
}
