class account:
    def __init__(self, balance):
        self.balance=balance

    def withdraw(self, amount):
        if  (amount>self.balance):
            return "Insufficient Funds"
        else:
            self.balance-=amount
            return self.balance

    def deposit(self, amount):
        self.balance+=amount
        return self.balance

W1=account(100)
W1.withdraw(50)
W1.deposit(24)
