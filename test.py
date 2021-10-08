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
type(b"wtewte")
type(1)
type(1.4)
type("tet")
"пуцпукпц".encode()
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

try: 
    x = [1,2,3]
    x[4] = 1
except Exception as e:
    print(e)



