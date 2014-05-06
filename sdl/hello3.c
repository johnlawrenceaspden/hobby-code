#include "SDL/SDL.h"

// Install sdl with
// sudo apt-get install libsdl1.2-dev libsdl-image1.2-dev libsdl-mixer1.2-dev libsdl-ttf2.0-dev
// although you probably actually need only:
// sudo apt-get install libsdl1.2-dev
// Compile with
// gcc -std=gnu99 hello3.c -lSDL && ./a.out

int main( int argc, char* args[] ) {
  //Start SDL
  SDL_Init( SDL_INIT_EVERYTHING );

  //Set up screen
  SDL_Surface* screen = SDL_SetVideoMode( 640, 480, 32, SDL_SWSURFACE );
  Uint32* pixels = (Uint32*) screen->pixels ;

  // Draw here
  SDL_LockSurface(screen);

  for(int x=50; x<300; x++)
    for (int y=50; y<200; y++)
      pixels[x + y * screen->w] = SDL_MapRGB(screen->format, x, y, x*y);

  SDL_UnlockSurface(screen);

  //Update Screen
  SDL_Flip( screen );

  //Pause to admire the glory
  SDL_Delay( 2000 );

  //Quit SDL
  SDL_Quit();
  return 0;
}
