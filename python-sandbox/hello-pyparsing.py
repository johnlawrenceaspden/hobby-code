from pyparsing import Word, alphas

greet = Word(alphas) + "," + Word(alphas) + "!"
hello = "Hello, World!"

print(hello, "->", greet.parseString(hello))

