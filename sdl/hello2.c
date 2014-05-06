#include "SDL/SDL.h"

// Compile with
// gcc -std=gnu99 -o hello hello2.c -lSDL && ./hello

typedef struct  {
  Uint8 r, g, b;
} Color;

int main( int argc, char* args[] ) {

  //Start SDL
  SDL_Init( SDL_INIT_EVERYTHING );
  SDL_WM_SetCaption( "SDL pixel plotting", NULL );

  // srand time
  srand( (unsigned)time(NULL) );

  // Misc variables
  const int wid = 640;
  const int hgt = 480;
  const int bit = 32;

  int tick = SDL_GetTicks();

  int fullscreen = 0;

  Uint8 *key;

  // Surfaces
  SDL_Surface *screen = NULL;

  // Fullscreen?
  if (fullscreen)
    screen = SDL_SetVideoMode(wid, hgt, bit, SDL_SWSURFACE | SDL_FULLSCREEN);
  else
    screen = SDL_SetVideoMode(wid, hgt, bit, SDL_SWSURFACE);

  SDL_Event event;

  // RGB Color
  Color clr;
  clr.r = 255;
  clr.g = 0;
  clr.b = 0;

  // Pixels
  Uint32 *pixels = NULL;

  while (event.type != SDL_QUIT ) {

    pixels = (Uint32*) screen->pixels;

    tick = SDL_GetTicks();
    key = SDL_GetKeyState( NULL );

    if (key[SDLK_ESCAPE])
      event.type = SDL_QUIT;

    // Show pixels
    SDL_LockSurface(screen);
    for(int x=50; x<300; x++)
      for (int y=50; y<200; y++)
        pixels[x + y * screen->w] = SDL_MapRGB(screen->format, clr.r, clr.g, clr.b);
    SDL_UnlockSurface(screen);

    // Poll event and update screen
    SDL_PollEvent( &event );
    SDL_Flip( screen );

    // FPS Regulator
    //    if (SDL_GetTicks() - tick < 1000 / 60)
    //  SDL_Delay( ((1000/60) - (SDL_GetTicks() - tick) ) );

  }

  //Quit SDL
  SDL_Quit();

  return 0;
}

