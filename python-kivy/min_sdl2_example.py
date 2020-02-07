import sdl2

from ctypes import *

try:
    from sdl2 import *
    from sdl2.ext import Resources
    from sdl2.ext.compat import byteify
except ImportError:
    import traceback
    traceback.print_exc()
    sys.exit(1)

RESOURCES = Resources(__file__, "assets")


class WavSound(object):
    def __init__(self, file):
        super(WavSound, self).__init__()
        self._buf = POINTER(Uint8)()
        self._length = Uint32()
        self._bufpos = 0
        self.spec = SDL_AudioSpec(0, 0, 0, 0)
        self._load_file(file)
        self.spec.callback = SDL_AudioCallback(self._play_next)
        self.done = False

    def __del__(self):
        SDL_FreeWAV(self._buf)

    def _load_file(self, file):
        rw = SDL_RWFromFile(byteify(file, "utf-8"), b"rb")
        sp = SDL_LoadWAV_RW(rw, 1, byref(self.spec), byref(self._buf), byref(self._length))
        if sp is None:
            raise RuntimeError("Could not open audio file: {}".format(SDL_GetError()))

    def _play_next(self, notused, stream, len):
        length = self._length.value
        numbytes = min(len, length - self._bufpos)
        for i in range(0, numbytes):
            stream[i] = self._buf[self._bufpos + i]
        self._bufpos += numbytes

        # If not enough bytes in buffer, add silence
        rest = min(0, len - numbytes)
        for i in range(0, rest):
            stream[i] = 0

        # Are we done playing sound?
        if self._bufpos == length:
            self.done = True


def main():
    if SDL_Init(SDL_INIT_AUDIO) != 0:
        raise RuntimeError("Cannot initialize audio system: {}".format(SDL_GetError()))

    sound_file = RESOURCES.get_path("440Hz_44100Hz_16bit_05sec.wav")
    sound = WavSound(sound_file)
    devid = SDL_OpenAudioDevice(None, 0, sound.spec, None, 0)
    if devid == 0:
        raise RuntimeError("Unable to open audio device: {}".format(SDL_GetError()))

    SDL_PauseAudioDevice(devid, 0)
    while not sound.done:
        SDL_Delay(100)
    SDL_CloseAudioDevice(devid)

    SDL_Quit(SDL_INIT_AUDIO)


if __name__ == '__main__':
    main()
