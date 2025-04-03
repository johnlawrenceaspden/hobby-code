#!/usr/bin/env python

import numpy as np
import time

# I wanted to calculate vAv for a 1000x1002 image of matrices A and a vector v
# I'd got this far on my own:

def vAvslow(tensor, v):
    result = np.zeros(tensor.shape[0:2])
    for i in range(tensor.shape[0]):
        for j in range(tensor.shape[1]):
            A = tensor[i, j]  # Get the matrix at position (i, j)
            result[i, j] = np.dot(v.T, np.dot(A, v))  # Compute v^T A v for this matrix A
    return result

# and checked that it did what i wanted, so I gave Claude 3.7 Sonnet that function and asked it how
# to vectorize the operation

# It came up with:

def vAv(tensor, v):
    # Reshape v to enable broadcasting
    # Making v a column vector by adding axes: (n,) -> (n, 1)
    v_col = v.reshape(-1, 1)
    
    # Compute v^T A v for each matrix in the tensor
    # First compute A*v for each matrix: (i, j, m, n) @ (n, 1) -> (i, j, m, 1)
    Av = np.matmul(tensor, v_col)
    
    # Then compute v^T * (A*v) for each result: (1, n) @ (i, j, n, 1) -> (i, j, 1, 1)
    result = np.matmul(v.reshape(1, -1), Av)
    
    # Reshape to remove the extra dimensions and get the final (i, j) shape
    return result.reshape(tensor.shape[0:2])

# And it also offered to give me some testing and benchmarking code, which is here (with some edits by me, although it was fine as it was given):

# The same function with some more concise comments:
def vAv(tensor, v):
    # Reshape v to enable broadcasting
    v_col = v.reshape(-1, 1)
    
    # Compute v^T A v for each matrix in the tensor
    Av = np.matmul(tensor, v_col)
    result = np.matmul(v.reshape(1, -1), Av)
    
    # Reshape to remove the extra dimensions
    return result.reshape(tensor.shape[0:2])

# Create test data
tensor_small = np.random.rand(10, 15, 2, 2)
tensor_medium = np.random.rand(60, 50, 2, 2)
tensor_large = np.random.rand(100, 200, 2, 2)
tensor_vlarge = np.random.rand(1000, 1002, 2, 2)
tensor_vvlarge = np.random.rand(4000, 4000, 2, 2)
v = np.random.rand(2)  # 2D vector

# Verify both functions produce the same result
result_slow = vAvslow(tensor_small, v)
result_fast = vAv(tensor_small, v)
print("Results match:", np.allclose(result_slow, result_fast))

result_slow = vAvslow(tensor_medium, v)
result_fast = vAv(tensor_medium, v)
print("Results match:", np.allclose(result_slow, result_fast))

result_slow = vAvslow(tensor_large, v)
result_fast = vAv(tensor_large, v)
print("Results match:", np.allclose(result_slow, result_fast))


result_slow = vAvslow(tensor_vlarge, v)
result_fast = vAv(tensor_vlarge, v)
print("Results match:", np.allclose(result_slow, result_fast))

result_slow = vAvslow(tensor_vvlarge, v)
result_fast = vAv(tensor_vvlarge, v)
print("Results match:", np.allclose(result_slow, result_fast))


# Benchmark function
def benchmark(func, tensor, v, name):
    start = time.time()
    func(tensor, v)
    end = time.time()
    print(f"{name} on shape {tensor.shape}: {end - start:.6f} seconds")

# Run benchmarks on different sized tensors
print("\nSmall tensor benchmarks:")
benchmark(vAvslow, tensor_small, v, "vAvslow")
benchmark(vAv, tensor_small, v, "vAv")

print("\nMedium tensor benchmarks:")
benchmark(vAvslow, tensor_medium, v, "vAvslow")
benchmark(vAv, tensor_medium, v, "vAv")

print("\nLarge tensor benchmarks:")
benchmark(vAvslow, tensor_large, v, "vAvslow")
benchmark(vAv, tensor_large, v, "vAv")

print("\nVLarge tensor benchmarks:")
benchmark(vAvslow, tensor_vlarge, v, "vAvslow")
benchmark(vAv, tensor_vlarge, v, "vAv")

print("\nVVLarge tensor benchmarks:")
benchmark(vAvslow, tensor_vvlarge, v, "vAvslow")
benchmark(vAv, tensor_vvlarge, v, "vAv")



# Calculate speedup
def compare_speedup(tensor, v):
    start = time.time()
    vAvslow(tensor, v)
    slow_time = time.time() - start
    
    start = time.time()
    vAv(tensor, v)
    fast_time = time.time() - start
    
    speedup = slow_time / fast_time
    print(f"\nSpeedup for shape {tensor.shape}: {speedup:.2f}x faster")

print("\nSpeedup comparison:")
compare_speedup(tensor_small, v)
compare_speedup(tensor_medium, v)
compare_speedup(tensor_large, v)
compare_speedup(tensor_vlarge, v)
compare_speedup(tensor_vvlarge, v)


