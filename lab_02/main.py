import numpy as np
import matplotlib.pyplot as plt


def T(z, p = 4, Tw = 10000, T0 = 2000):
    return (Tw - T0) * z ** p + T0

def Up(z):
    return 3.084e-4 / (np.exp(4.799e4 / T(z)) - 1)

def K(z, k0 = 0.0008):
    return k0 * (T(z) / 300) ** 2



def func(x, y):
    pass


def RungeKutta(x0, u0, h, x):
    cur_x = x0
    cur_y = u0

    n = int(abs(x - x0) / h)

    is_rev = 1 if x0 < x else -1

    for i in np.arange(n):
        k1 = h * func(cur_x, cur_y)
        k2 = h * func(cur_x + h / 2, cur_y + k1 / 2)
        k3 = h * func(cur_x + h / 2, cur_y + k2 / 2)
        k4 = h * func(cur_x + h, cur_y + k3)
        cur_y += (k1 + k2 + k3 + k4) / 6
        cur_x += h

    return cur_u * is_rev

