from math import sqrt
from joblib import Parallel, delayed
x = Parallel(n_jobs=2)(delayed(sqrt)(i ** 2) for i in range(10))
print(x)
#[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]

#import numpy as np

# numbers
# integers
print(1+1)
# floats
print(2.8)

# booleans
print(True and False)
print(True or False)

# boolean not
print(not True) 
print(not False)

# strings
print("pow'er")
print('po"wer')
print("print" + " print")

"gweg" + "fwe"
'gwegfwe'
b'wtewte'
type(b"wtewte\xff")
type(1)
type(1.4)
type("tet")
"пуцпукпц".encode()

#formatting strings
print("%d" % 4)
print("{}{}{{}}".format(4, 5))
print(f"{4}{{}}")

bin(0xd)
bin(0xb)
0b10000111111
hex(0b10000111111)
x = 1
y = 2
y = 1
x is y
x = bytearray(b"werwer")
x[0] = 1
x = [1,2,3]
y = x 
x[0] = 4
print(x, y)
x = [1,2,3]
y = (1,2,3)
z = {1: 2, 3: 4}
list(z.keys())
list(z.values())
list(z.items())
y = list(z.items())
dict(y)
x = {1,2,3}
type(x)
x = {1,2,3,3}
x
1 in x
1 in x
1 in {1:2, 3:4}
3 in {1:2, 3:4}
3 in [1,2,3]
4 in [1,2,3]
4 in (1,2,3)
# {[3,3]:4}
{(3,3):4}
x = list(range(10))
x.append(12)
x[0:]
x[:4]
x[::2]
x[-1:-4:-1]
x[:]

a = "s"
if a == "s":
    print(10)
elif a == "f":
    print(8)
else:
    print(11)
a = 1
b = 2
c = (1 if a > b else 2)
print(c)
x = (1,2,3)
a, b, c = x

for k in {1:2, 3:4}.items():
    t, p = k
    print(t, p)

for i in [1,2,3]:
    x = i
print(x)

if 1 > 2:
    l = 1
else:
    l = 2
print(l)
print()

a = 1
b = 7
while a < b:
    print(a)
    a += 1 # a = a + 1
    if a > 2:
        print(a+4)

try: 
    x = [1,2,3]
    x[4] = 1
except Exception as e:
    print(e)

#list comprehensions
c = [45, 1, 2]
a = [x + 2 for x in c if x > 1]
print(a)
b = {k: v for k, v in [(1,20), (4,5)]}
print(b)

#None
None == None
print(type(None))

#functions
def f(a, b):
    print(b)
    return a + 2

print(f(1, [1,2,3,4,5]))

def f1(*a):
    print(a)

f1(1,2,3,4)

def f2(a,b):
    print(a+b)

print(f2(a=1,b=2))
f2(a=1,b=2)
f2(1,b=2)

def f3(a,b=4):
    print(a+b)

f3(1)
f3(2,3)

def f4(a=1,b=3,c=4,d=56):
    if a < 0:
        print(b+c)
    else:
        print(d-a)

f4()
f4(a=-1)
f4(d=4)

#arbitrary keyword arguments
def f5(**kw):
    print(kw)

f5(a=1,b=3)

def f6(a):
    r = 1
    try:
        r = int(a)
    except:
        pass
    return r

print(f6("32"))
print(f6("err"))

def f7(a,b):
    return a,b

i = f7(2,3)
print(i[0])

#anonymous functions
f8 = lambda x, y: x + y

print(f8(3, 6))

print(list(map(lambda x: x + 2, [1,2,3])))
print(list(map(f6, [1,2,3])))

#recursive functions
def f9(a):
    if a == 0:
        pass
    else:
        print(a)
        f9(a-1)

f9(10)

# Factorial (recursive + loops) 
# n! 1*2....n-1*n

# Fibonacci numbers (recursive + loops)
# f(n) = f(n-1) + f(n-2), f(0) = 1, f(1) = 1

#n! (n-0) * (n-1) * (n-2) * .... * 3 *  2 *  1
#n! 6 * 5     * 4     * 3     * 2     * 1

def fib(n):
    if n < 2:
        return n 
    a = 1
    b = 0
    for _ in range(2, n+1):
        tmp = a
        a = a + b
        b = tmp
    return a

def fact(n):
    prod = 1
    for i in range(1, n):
        prod *= i + 1
    return prod
print()
#classes
class Base:
    b = 8
    def __init__(self):
        pass
class Test(Base):
    a = 4
    def __init__(self, v):
        self.value = v
        if v > 4:
            self.o = 8
    def add(self, x):
        self.value += x

x = Test(4)
y = Test(5)
print(x.b)
y.a=  6
print(x.value, y.a)
x.add(7)
print(x.value, x.a)

class Figure:
    area = 0

class Circle(Figure):
    def __init__(self, r):
        self.radius = r
        self.area = 3.14159265358 * r ** 2

class Square(Figure):
    def __init__(self, s):
        self.size = s
        self.area = s * s 

def print_area(f):
    print(f.area)

print_area(Circle(5))
print_area(Square(5))