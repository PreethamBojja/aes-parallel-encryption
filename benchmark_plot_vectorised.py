import subprocess
import matplotlib.pyplot as plt
import statistics

# ← point this at your vectorized build
EXECUTABLE = "./vector"

def run_once(filename, procs):
    """
    Invoke your vectorized binary once and extract the “Encryption took X.XXX” time.
    """
    try:
        result = subprocess.run(
            [EXECUTABLE, "@mpl", "procs", str(procs), "--", filename],
            capture_output=True, text=True, check=True
        )
        for line in result.stdout.splitlines():
            if "Encryption took" in line:
                return float(line.split()[2])
    except subprocess.CalledProcessError as e:
        print(f"  ⚠️  Command failed: {e}")
    return None

def average_time(filename, procs, trials=10):
    """
    Run `run_once` multiple times and return the average.
    """
    times = []
    for i in range(1, trials+1):
        print(f"  Run {i}/{trials} | file: {filename} | procs: {procs}")
        t = run_once(filename, procs)
        if t is not None:
            times.append(t)
        else:
            print("    ⚠️  Failed to get time")
    if not times:
        print("  ❌ No valid timings collected\n")
        return None

    avg = statistics.mean(times)
    print(f"  ✅ Average encryption time: {avg:.4f}s\n")
    return avg

# -----------------------------------------------------------------------------
# 1) Block-count vs Time @ 4 processes
# -----------------------------------------------------------------------------
input_files = [
    ("input_100.hex",    100),
    ("input_1000.hex",   1000),
    ("input_10000.hex", 10000),
    ("input_100000.hex",100000),
]

block_sizes = []
times_block = []

print("\n======== Vectorized Benchmark: Block Count vs Time (4 Procs) ========")
for filename, blocks in input_files:
    avg = average_time(filename, procs=4)
    if avg is not None:
        block_sizes.append(blocks)
        times_block.append(avg)

plt.figure(figsize=(8, 5))
plt.plot(block_sizes, times_block, marker='o')
plt.xscale('log')
plt.title("AES Encryption (vectorized): Time vs Block Size (4 Procs)")
plt.xlabel("Number of 128-bit AES Blocks (log scale)")
plt.ylabel("Average Time (s)")
plt.grid(True)
plt.savefig("aes_vectorized_time_vs_blocksize.png")
plt.show()

# -----------------------------------------------------------------------------
# 2) Process-count vs Time @ 100000 blocks
# -----------------------------------------------------------------------------
proc_counts     = [1, 2, 4, 6, 8]
times_proc      = []
fixed_filename = "input_100000.hex"

print("\n======== Vectorized Benchmark: Process Count vs Time (100k Blocks) ========")
for p in proc_counts:
    avg = average_time(fixed_filename, procs=p)
    if avg is not None:
        times_proc.append(avg)

plt.figure(figsize=(8, 5))
plt.plot(proc_counts, times_proc, marker='s', linestyle='--')
plt.title("AES Encryption (vectorized): Time vs Number of Processes (100k Blocks)")
plt.xlabel("Number of Processes")
plt.ylabel("Average Time (s)")
plt.xticks(proc_counts)
plt.grid(True)
plt.savefig("aes_vectorized_time_vs_processes.png")
plt.show()

# -----------------------------------------------------------------------------
# 3) Process-count vs Speedup @ 100000 blocks
# -----------------------------------------------------------------------------
if times_proc:
    baseline = times_proc[0]
    speedups = [baseline / t for t in times_proc]

    plt.figure(figsize=(8, 5))
    plt.plot(proc_counts, speedups, marker='^', linestyle='-')
    plt.title("AES Encryption (vectorized): Speedup vs Number of Processes (100k Blocks)")
    plt.xlabel("Number of Processes")
    plt.ylabel("Speedup (relative to 1 process)")
    plt.xticks(proc_counts)
    plt.grid(True)
    plt.savefig("aes_vectorized_speedup_vs_processes.png")
    plt.show()
else:
    print("No valid timings for speedup plot")
