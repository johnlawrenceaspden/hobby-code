#include<stdlib.h>
#include<unistd.h>
#include<X11/Xlib.h>

#include "vroot.h"


#define NCOLORS 3

int main (int argc, char *argv[])
{
  Display *dpy;
  Window w;		/* the window to draw into: it may be root */
  XWindowAttributes wa;
  GC g;
  XEvent e;

  char *colors[NCOLORS]={"red", "green", "blue"};
  XColor xcolors[NCOLORS];
  XColor xc, sc;
  int c;


  /* open the display (connect to the X server) */
  dpy = XOpenDisplay (getenv ("DISPLAY"));


  /* get the window to draw into */
  if( argc<=1 )
    w = DefaultRootWindow (dpy);
  else { 
    /* create, name, and map window */
    w = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
                             200, 200, 400, 400, 1,
                             BlackPixel (dpy, DefaultScreen (dpy)),
                             WhitePixel (dpy, DefaultScreen (dpy)));
    XStoreName (dpy, w, "This is a name");
    XSelectInput (dpy, w, StructureNotifyMask);
    XMapRaised (dpy, w);
    do {
      XWindowEvent (dpy, w, StructureNotifyMask, &e);
    } while( e.type != MapNotify );
  }


  /* get attributes of the window */
  XGetWindowAttributes(dpy, w, &wa);


  /* create a GC for drawing in the window */
  g = XCreateGC (dpy, w, 0, NULL);


  /* allocate colors */
  for(c=0; c<NCOLORS; c++) {
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
