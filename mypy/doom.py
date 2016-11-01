#!/usr/bin/env python3

# $ sudo apt-get install python3 python3-pip
# $ python3 -m pip install -U mypy-lang

print("Hello\n")

def greeting(name:str) -> str:
    return 'Hello {}'.format(name)


print(greeting("John"))
print(greeting(2))


