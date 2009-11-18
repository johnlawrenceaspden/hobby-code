import pygame, math
pygame.init()
screen = pygame.display.set_mode((200, 200))
for i in range(2000):
    screen.set_at((50*math.cos(i/100.0) + 100, 50*math.sin(i/100.0) + 100), (255, 128, 0))
pygame.display.flip()
while True: pygame.event.poll()
