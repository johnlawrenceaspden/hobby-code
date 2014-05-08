#include<stdlib.h>
#include<unistd.h>
#include<X11/Xlib.h>

#define NCOLORS 3

void main (int argc, char *argv[])
{

  /* open the display (connect to the X server) */
  Display *dpy = XOpenDisplay (getenv ("DISPLAY"));
  Window w = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
                                  200, 200, 400, 400, 1,
                                  BlackPixel (dpy, DefaultScreen (dpy)),
                                  WhitePixel (dpy, DefaultScreen (dpy)));
  XStoreName (dpy, w, "This is a name");
  XSelectInput (dpy, w, StructureNotifyMask);
  XMapRaised (dpy, w);
  { XEvent e;
  do {
    XWindowEvent (dpy, w, StructureNotifyMask, &e);
  } while( e.type != MapNotify );}


  /* get attributes of the window */
  XWindowAttributes wa;
  XGetWindowAttributes(dpy, w, &wa);

  /* create a GC for drawing in the window */
  GC g = XCreateGC (dpy, w, 0, NULL);

  char *colors[NCOLORS]={"red", "green", "blue"};
  XColor xcolors[NCOLORS];

  /* allocate colors */
  for(int c=0; c<NCOLORS; c++) {
    XColor xc, sc;

    XAllocNamedColor(dpy,
                     DefaultColormapOfScreen(DefaultScreenOfDisplay(dpy)),
                     colors[c],
                     &sc, &xc);
    xcolors[c]=sc;
  }


  /* draw something */
  while (1)
    {
      /* set a random foreground color */
      XSetForeground(dpy, g, xcolors[random()%NCOLORS].pixel);


      /* draw a square */
      XFillRectangle (dpy, w, g, random()%(wa.width-50),
                      random()%(wa.height-40), 50, 40);


      /* once in a while, clear all */
      if( random()%500<1 )
        XClearWindow(dpy, w);


      /* flush changes and sleep */
      XFlush(dpy);
      usleep (10);
    }


  XCloseDisplay (dpy);
}
