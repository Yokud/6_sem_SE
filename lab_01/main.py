import numpy as np
import matplotlib.pyplot as plt


def func(x, u):
    return x * x + u * u
        

def integr(func, a, b, h, u):
    I = 0
    n = (b - a) // h
    
    for i in range(n):
        I += func(a + (i + 0.5) * h, u)

    return I * h

def Picar(func, x, s, x0, v0):
    if (s == 0):
        return v0
    else:
        I = v0
        n = 10
        h = (x - x0) / n
    
        for i in range(n):
            I += func(x0 + (i + 0.5) * h, Picar(func, x0 + (i + 0.5) * h, s - 1, x0, v0))

        return I * h

def Euler(func, x0, u0, h, x):
    cur_x = x0
    cur_u = u0

    n = int(abs(x - x0) / h)

    for i in np.arange(n):
        cur_u += h * func(cur_x, cur_u)
        cur_x += h

    return cur_u


#print(Picar(func, 2, 4, 0, 0))
print(Euler(func, 0, 0, 1e-5, 2))

xs = []
ys = []

h = 1e-2

xs = np.arange(-2, 2 + h, h)
n = len(xs)
ys = np.ndarray(n)

for i in range(n):
    ys[i] = (Euler(func, 0, 0, 1e-5, xs[i]))

plt.plot(xs, ys)
plt.show()
