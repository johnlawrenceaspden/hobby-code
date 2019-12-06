import math


def get_primes(input_list):
    result_list = list()
    for element in input_list:
        if is_prime(element):
            result_list.append(element)

    return result_list


def is_prime(number):
    if number > 1:
        if number == 2:
            return True
        if number % 2 == 0:
            return False
        for current in range(3, int(math.sqrt(number) + 1), 2):
            if number % current == 0:
                return False
        return True
    return False


print(get_primes([0, 1, 2, 3, 4, 6, 7, 8, 9, 10]))


def simple_generator_function():
    yield 1
    yield 2
    yield 3


our_generator = simple_generator_function()

print(next(our_generator))
print(next(our_generator))
print(next(our_generator))
try:
    print(next(our_generator))
except:
    print("boom")

our_generator = simple_generator_function()

print(next(our_generator))
print(next(our_generator))
print(next(our_generator))
try:
    print(next(our_generator))
except:
    print("boom")

our_generator = simple_generator_function()
print(list(our_generator))



def get_primes(number):
    while True:
        if is_prime(number):
            yield number
        number += 1

g = get_primes(3)
print(next(g))
print(next(g))
print(next(g))
print(next(g))

def solve_number_10():
    # She *is* working on Project Euler #10, I knew it!
    total = 2
    for next_prime in get_primes(3):
        if next_prime < 2000:  # 2000000 takes a while
            total += next_prime
        else:
            print(total)
            return

solve_number_10()


g = get_primes(3)
print(next(g))
g = get_primes(10)
print(next(g))
print(next(g))
g = get_primes(100)
print(next(g))
print(next(g))
g = get_primes(1000)
print(next(g))
print(next(g))


def get_primes(number):
    while True:
        if is_prime(number):
            inval = yield number
            if inval:
                number=inval
        number += 1


# generator goes start-->yield, receive, yield, receive
# so whole execution path goes
#                                         start-->yield,     receive-->yield,       receive-->yield
# create, send (nowhere for value to go!),              send,                send,


g = get_primes(3)
print(next(g))
print(g.send(10))
print(next(g))
print(next(g))
print(g.send(100))
print(next(g))
print(next(g))
print(next(g))
print(g.send(1000))
print(next(g))
print(next(g))
print(next(g))
print(g.send(None))

# Note that next(g) and g.send(None) are the same

g = get_primes(3)
print(g.send(None))
print(g.send(10))
print(g.send(None))
print(g.send(None))
print(g.send(100))
print(g.send(None))
print(g.send(None))
print(g.send(None))
print(g.send(1000))
print(g.send(None))
print(g.send(None))
print(g.send(None))
print(g.send(None))


def print_successive_primes(iterations, base=10):
    prime_generator = get_primes(base)
    prime_generator.send(None)
    for power in range(iterations):
        print(prime_generator.send(base ** power))
        print(next(prime_generator))
        print(next(prime_generator))

print_successive_primes(5)
