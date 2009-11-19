class Accumulator:
    def __init__(self, amount):
        self.amount=amount

    def add(self, amount):
        self.amount+=amount
        return self.amount

A=Accumulator(10)
A.add(10)
