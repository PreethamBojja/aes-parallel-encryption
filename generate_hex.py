# generate_hex.py
import random

def rand_block():
    return ''.join(random.choices('0123456789abcdef', k=32))

def write_blocks(filename, num_blocks):
    with open(filename, 'w') as f:
        for _ in range(num_blocks):
            f.write(rand_block())

sizes = [100, 1000, 10000, 100000]
for size in sizes:
    write_blocks(f'input_{size}.hex', size)
