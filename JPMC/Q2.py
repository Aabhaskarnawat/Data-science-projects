import math
import numpy as np


def n():
    return np.random.normal(0, 1)


def calc(a, theta, x0, sigma):
    x = x0
    for i in range(1, 101):
        sig = math.sqrt(x / 50.0) * n()
        x += a * (theta - x) / 50.0 + sigma * sig
    return x


def calc2(a, theta, x0, sigma):
    val1 = 0.0
    val2 = 0.0
    val3 = 0.0
    for i in range(1, 100001):
        val1 += max(0.0, x0 + a * (theta - x0) / 50.0 + sigma * math.sqrt(x0 / 50.0) * n() - 100.0)
        val2 += max(0.0, x0 + a * (theta - x0) / 50.0 + (sigma + 0.0001) * math.sqrt(x0 / 50.0) * n() - 100.0)
        val3 += max(0.0, x0 + a * (theta - x0) / 50.0 + (sigma - 0.0001) * math.sqrt(x0 / 50.0) * n() - 100.0)
    val1 /= 100000
    val2 /= 100000
    val3 /= 100000
    d1 = (val2 - val1) / 0.0001
    d2 = (val1 - val3) / 0.0001
    return (d1 + d2) / 2


# Read only region start
class UserMainCode(object):
    @classmethod
    def solution(cls, a, theta, X0, sigma):
        '''
        input1 : double
        input2 : double
        input3 : double
        input4 : double

        Expected return type : List[Double]
        '''
        # Read only region end
        tot1 = 0.0
        tot2 = 0.0
        for i in range(1, 10001):
            x = calc(a, theta, X0, sigma);
            tot1 += x
            tot2 += max(x - 100.0, 0.0)
        tot1 /= 10000.0
        tot2 /= 10000.0
        tot3 = calc2(a, theta, X0, sigma)
        return [tot1, tot2, tot3]