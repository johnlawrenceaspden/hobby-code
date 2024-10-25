#!/bin/env python3

# * Arithmetic Coding of a six-sided die into a bitstream

# 1,5,3,2

# ((((0*6+1)*6+5)*6+3)*6+2) = 416

# 416
# >>> 416 % 6
# 2
# >>> (416-2) /6
# 69
# >>> 69 % 6
# 3
# >>> (69-3)/6
# 11
# >>> 11 % 6
# 5
# >>> (11 - 5) / 6
# 1
# >>> 1 % 6
# 1
# >>> (1-1)/6
# 0

# end of message, except that that terminates early if the message starts 0,0,0 etc

message = [0]
for _ in range(random.randint(1, 100)):
    # for _ in range(69):
    message.append(random.randint(0, 5))

print(message)
print(len(message) * math.log2(6))

acc = 1
for i in message:
    acc = acc * 6 + i

print(acc)
print(bin(acc))

transmit = bin(acc)[2:]
print(transmit)
print(len(transmit))

rx = int("0b" + transmit, 2)
print(rx)

acc = []
while rx > 5:
    roll = rx % 6
    acc.append(roll)
    rx = rx - roll
    rx = rx // 6
acc = list(reversed(acc))
print(acc)

assert acc == message
assert len(transmit) > (len(message) * math.log2(6))
# this is not always true, 69 element messages can encode to 181 or 182 bit strings
# but 70*math.log2(6) is 180.94 so there's an extra bit needed sometimes
assert len(transmit) < (len(message) * math.log2(6)) + 1
