#!/usr/bin/env python3

from timeit import default_timer as timer
from array import *
import random
import math

def fib (n):
    if n <= 1:
        return 1

    return fib (n - 1) + fib (n - 2)

def for_loop_empty (n):
    for i in range (1, n + 1):
        continue

def for_loop_silly (n):
    for i in range (1, n + 1):
        j = i

def for_loop_binop_1(n):
    for i in range (1, n + 1):
        j = 1*2*3*4

def for_loop_sinpi (n):
    for i in range (1, n):
        j = math.sin (math.pi * i)

def for_loop_ifs (n):
    for i in range (1, n + 1):
        if i == 100.0:
            continue
        elif i == 300.0:
            continue

        if i * 2.0 == 3002.0:
            continue

        if i < 0.0:
            break

        if i == -1024.0:
            print ("fooo")

        if i == n + 1.0:
            break

        if not i:
            break

def while_loop_empty (n):
    i = 0.0
    while i < n:
        i += 1

def for_loop_subfun_1 (n):
    for i in range (n):
        suby ()

def suby ():
    return

def qsort_iterative (A):
    l = len (A)
    stack = [0] * 128 # Probably big enough

    top = 0
    stack[top] = 0
    top += 1
    stack[top] = l - 1
    top += 1

    while top > 0:
        top -= 1
        high = stack[top]
        top -= 1
        low = stack[top]

        p = low - 1
        x = A[high]

        for j in range (low, high):
            if A[j] <= x:
                p += 1
                tmp = A[j]
                A[j] = A[p]
                A[p] = tmp
        p += 1
        tmp = A[high]
        A[high] = A[p]
        A[p] = tmp

        if p - 1 > low:
            stack[top] = low
            top += 1
            stack[top] = p - 1
            top += 1
        if p + 1 < high:
            stack[top] = p + 1
            top += 1
            stack[top] = high
            top += 1

    return A

def time_fn_call (fn, arg):
    start = timer()
    fn (arg)
    end = timer()

    return end - start

def randn (rows, cols):
    if rows == 1:
        arr = array('d')
        for i in range(cols):
            arr.append(random.gauss (0, 1))
        return arr

    arr = array()
    for i in range (rows):
        arr_row = array('d')
        for j in range(cols):
          arr_row.append(random.gauss (0, 1))
        arr.append (arr_row)
    return arr

tests = \
   [[for_loop_empty, 206824596],
    [for_loop_silly, 34894840],
    [for_loop_binop_1, 20300088],
    [for_loop_sinpi, 12991066],
    [for_loop_ifs, 5874007],
    [while_loop_empty, 24237997],
    [for_loop_subfun_1, 11930390],
    [fib, 31],
    [qsort_iterative, "rowvec", 344418]]

def main():
    for t in tests:
        if (t[1] == "rowvec"):
            dt = time_fn_call (t[0], randn (1, t[2]))
        else:
            dt = time_fn_call (t[0], t[1])
        print (t[0].__name__ + " in %g s" % dt)

if __name__ == "__main__":
    main ()
