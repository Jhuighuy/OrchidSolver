import sys
sys.setrecursionlimit(1000000000)
def A(m, n):
    if m == 0:
        return n+1
    else:
        if n == 0:
            return A(m-1, 1)
        else:
            return A(m-1, A(m, n-1))
print(A(3, 3))
print(A(3, 4))
#print(A(4, 1))
