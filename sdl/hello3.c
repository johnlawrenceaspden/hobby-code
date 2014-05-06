#include "SDL/SDL.h"

// Compile with
// gcc -std=gnu99 hello3.c -lSDL && ./a.out

int main( int argc, char* args[] ) {
  SDL_Surface* screen = NULL;
  //Start SDL
  SDL_Init( SDL_INIT_EVERYTHING );

  //Set up screen
  screen = SDL_SetVideoMode( 640, 480, 32, SDL_SWSURFACE );
  Uint32* pixels = (Uint32*)screen->pixels ;

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
