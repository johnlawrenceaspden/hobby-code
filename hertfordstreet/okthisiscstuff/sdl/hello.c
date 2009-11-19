#include <SDL.h>
#include <stdio.h>

#define SCREEN_WIDTH 1280
#define SCREEN_HEIGHT 1024
#define SCREEN_DEPTH 32

SDL_Surface *screen;


void putpixel(int x, int y, int colour)
{
    Uint32 *p;
    p=(Uint32 *)screen->pixels+y*screen->pitch/4 + x;
    *p=colour;
}

void DrawPixel(SDL_Surface *screen, int x, int y, Uint8 R, Uint8 G, Uint8 B)
{
    Uint32 color = SDL_MapRGB(screen->format, R, G, B);

    if ( SDL_MUSTLOCK(screen) ) {
        if ( SDL_LockSurface(screen) < 0 ) {
            return;
        }
    }
    switch (screen->format->BytesPerPixel) {
        case 1: { /* Assuming 8-bpp */
            Uint8 *bufp;

            bufp = (Uint8 *)screen->pixels + y*screen->pitch + x;
            *bufp = color;
        }
        break;

        case 2: { /* Probably 15-bpp or 16-bpp */
            Uint16 *bufp;

            bufp = (Uint16 *)screen->pixels + y*screen->pitch/2 + x;
            *bufp = color;
        }
        break;

        case 3: { /* Slow 24-bpp mode, usually not used */
            Uint8 *bufp;

            bufp = (Uint8 *)screen->pixels + y*screen->pitch + x;
            *(bufp+screen->format->Rshift/8) = R;
            *(bufp+screen->format->Gshift/8) = G;
            *(bufp+screen->format->Bshift/8) = B;
        }
        break;

        case 4: { /* Probably 32-bpp */
            Uint32 *bufp;

            bufp = (Uint32 *)screen->pixels + y*screen->pitch/4 + x;
            *bufp = color;
        }
        break;
    }
    if ( SDL_MUSTLOCK(screen) ) {
        SDL_UnlockSurface(screen);
    }
    //SDL_UpdateRect(screen, x, y, 1, 1);
}
 

void EventPoll(void)
{
    SDL_Event event;

    while ( SDL_PollEvent(&event) ) {
        switch (event.type) {
            case SDL_MOUSEMOTION:
                printf("Mouse moved by %d,%d to (%d,%d)\n", 
                       event.motion.xrel, event.motion.yrel,
                       event.motion.x, event.motion.y);
                break;
            case SDL_MOUSEBUTTONDOWN:
                printf("Mouse button %d pressed at (%d,%d)\n",
                       event.button.button, event.button.x, event.button.y);
                break;
            case SDL_QUIT:
                exit(0);
        }
    }
}


int main(int argc, char *argv[]) {
     
     /* Initialize SDL */
     if(SDL_Init(SDL_INIT_VIDEO)<0)
     {
         printf("Unable to init SDL: %s\n", SDL_GetError());
         exit(1);
     }

     atexit(SDL_Quit);
     
     /* Initialize the screen / window */
     screen = SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_DEPTH, SDL_SWSURFACE);

     if (screen== NULL){
         printf("Unable to set video: %s\n", SDL_GetError());
         return 1;
     }

     for(;;) 
     {
         int x, y, colour;
         for(colour=0; colour<0xffff; colour++)
         {
             for(y=0; y<SCREEN_HEIGHT; y++)
             {
                 for (x=0; x<SCREEN_WIDTH; x++)
                 {
                     //putpixel(x,y,colour);
                     DrawPixel(screen,x,y,y-colour,x,colour); 
                 }
             }
             SDL_Flip(screen);
             EventPoll();

         }
     }
}


