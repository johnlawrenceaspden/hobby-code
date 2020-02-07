#!/usr/bin/env python

# works with:
# export KIVY_AUDIO=ffpyplayer
# fails with:
# export KIVY_AUDIO=sdl2

from kivy.app import App 
 
from kivy.core.audio import SoundLoader

def playsound(dummy):
    sound = SoundLoader.load("440Hz_44100Hz_16bit_05sec.ogg")
    
    if sound:
        print("Sound found at %s" % sound.source)
        print("Sound is %.3f seconds" % sound.length)
        print("sound state", sound.state)
        sound.play()
        print("sound state", sound.state)
        import time
        time.sleep(5)
        
class TestApp(App):
    playsound(None)
        


    
if __name__ == '__main__':
    TestApp().run()


