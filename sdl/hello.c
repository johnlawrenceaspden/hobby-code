#include "SDL/SDL.h"

// compile with:
// g++ -o hello hello.c -lSDL

int main( int argc, char* args[] ) {
        //Start SDL
        SDL_Init( SDL_INIT_EVERYTHING );
        //Quit SDL
        SDL_Quit();
        return 0;
}
