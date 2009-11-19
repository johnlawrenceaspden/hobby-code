def fizzbuzz(n):
    if n:
        if n % 15 == 0: return fizzbuzz(n-1) + 'fizzbuzz ';
        elif n % 5 == 0: return fizzbuzz(n-1) + 'buzz ';
        elif n % 3 == 0: return fizzbuzz(n-1) + 'fizz ';
        else : return fizzbuzz(n-1) + ('%d ' % n)
    return '--'
