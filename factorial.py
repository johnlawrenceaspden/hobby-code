def factorial(n):
    return 1 if (n < 2) else n*factorial(n-1)

def fibonacci(n):
    return 1 if (n < 2) else fibonacci(n-1)+fibonacci(n-2)
