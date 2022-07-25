import numpy as np
import numpy.random as npr
import matplotlib.pyplot as plt

x = npr.rand(10, 5)
np.savetxt("rand.txt", x)
y = np.loadtxt("rand.txt")
print(x == y)
with open("rand2.txt", "w") as f:
    shape = np.shape(x)
    for i in range(shape[0]):
        for j in range(shape[1]):
            f.write(f"{x[i, j]} ")
        f.write("\n")

with open("rand2.txt", "r") as f:
    lines = f.readlines()
    rows = len(lines)
    cols = len(lines[0].split())
    res = np.zeros((rows, cols))
    for i, line in enumerate(lines):
        for j, word in enumerate(line.split()):
            try:
                res[i, j] = float(word)
            except ValueError:
                res[i, j] = 0
print(res)
print()
print(res.min(axis=0))
print()
print(res.min(axis=1))
print()
# print(res.min(axis=2))
# print()
sums_0 = res.sum(axis=0)
minsum = sums_0.min()
minsum_idx = np.where(sums_0 == minsum)
print(sums_0, "\n", minsum, "\n", minsum_idx)
print(res.sum(axis=1).min())

# new figure (window)
figure = plt.figure()
# new axes
ax = figure.add_subplot(221)
ax2 = figure.add_subplot(222)
ax.plot(np.arange(len(sums_0)), sums_0, 'bo', linestyle="--")
ax.set_xlabel("aaa")
ax.set_ylabel("bbb")
ax.set_title("CCC")
ax.arrow(0, 0, 0.5, 0.5, head_width=0.05, head_length=0.1, fc='k', ec='k')

ax2.semilogy(sums_0)
ax2.set_xlabel("aaa")
ax2.set_ylabel("bbb")
ax2.set_title("CCC")

# plt.show()

try: 
    assert 1 == 2, "AAA"
    # raise Exception("AAA")
except Exception as e:
    print(e)
    
# assert False, "BBB"
def singleNumber(nums):
    for i in range(len(nums)):
        found = False
        for j in range(i+1, len(nums)):
            if nums[i] == nums[j]:
                found = True
                break
        if not found:
            return nums[i]


print(singleNumber([2,1,4,2,4]))

def singleNumber2(nums):
    found = set()
    for i in nums:
        if i not in found:
            found.add(i)
        else:
            found.discard(i)
    return list(found)[0]

print(singleNumber2([2,1,4,2,4]))

def singleNumber3(nums):
    x = 0
    for i in nums:
        x = x ^ i
    return x

print(singleNumber3([2,1,4,2,4]))

def roman(num):
    romans = [(1000, "M"), (500, "D"), (100, "C"), (50, "L"), (10, "X"), (5, "V"), (1, "I")]
    res = ""
    while num:
        for r, v in romans:
            if num - r >= 0:
                res += v
                num -= r
                break
        print(num)
    return res

print(roman(1349))
