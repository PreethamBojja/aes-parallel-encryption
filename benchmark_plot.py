import subprocess
import matplotlib.pyplot as plt
import statistics

# Run once and extract encryption time from stdout
def run_once(filename, procs):
    try:
        result = subprocess.run(
            ["./main", "@mpl", "procs", str(procs), "--", filename],
            capture_output=True, text=True
        )
        for line in result.stdout.splitlines():
            if "Encryption took" in line:
                return float(line.split()[2])
    except:
        pass
    return None

# Average over multiple trials
def average_time(filename, procs, trials=10):
    times = []
    for i in range(trials):
        print(f"  Run {i+1}/{trials} | file: {filename} | procs: {procs}")
        t = run_once(filename, procs)
        if t is not None:
            times.append(t)
        else:
            print("    ⚠️  Failed to get time")
    if times:
        avg = statistics.mean(times)
        print(f"  ✅ Average encryption time: {avg:.4f}s\n")
        return avg
    else:
        print("  ❌ No valid timings collected\n")
        return None

# 1. Time vs Block Size @ 4 processes
input_files = [
    ("input_100.hex", 100),
    ("input_1000.hex", 1000),
    ("input_10000.hex", 10000),
    ("input_100000.hex", 100000),
]

block_sizes = []
avg_times = []

print("\n======== Benchmark: Block Count vs Time (4 Procs) ========")
for filename, blocks in input_files:
    avg = average_time(filename, procs=4)
    if avg:
        block_sizes.append(blocks)
        avg_times.append(avg)

plt.figure(figsize=(8, 5))
plt.plot(block_sizes, avg_times, marker='o')
plt.xscale('log')
plt.title("AES Encryption: Time vs Block Size (4 Processes)")
plt.xlabel("Number of 128-bit AES Blocks (log scale)")
plt.ylabel("Average Time (s)")
plt.grid(True)
plt.savefig("aes_time_vs_blocksize.png")
plt.show()

# 2. Time vs Number of Processes @ 1000 blocks
proc_counts = [1, 2, 4, 6, 8]
times_per_proc = []
filename_100000 = "input_100000.hex"

print("\n======== Benchmark: Process Count vs Time (100000 Blocks) ========")
for p in proc_counts:
    avg = average_time(filename_100000, procs=p)
    if avg:
        times_per_proc.append(avg)

plt.figure(figsize=(8, 5))
plt.plot(proc_counts, times_per_proc, marker='s', linestyle='--', color='orange')
plt.title("AES Encryption: Time vs Number of Processes (100000 Blocks)")
plt.xlabel("Number of Processes")
plt.ylabel("Average Time (s)")
plt.grid(True)
plt.savefig("aes_time_vs_processes.png")
plt.show()
