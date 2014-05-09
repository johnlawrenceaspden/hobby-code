#include<stdlib.h>
#include<unistd.h>
#include<X11/Xlib.h>

#define NCOLORS 3

#include "vroot.h" //just including this magically makes it work with xscreensaver too

int main (int argc, char *argv[])
{

  /* connect to the X server and make a window */
  Display *dpy = XOpenDisplay (getenv ("DISPLAY"));
  Window w = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
                                  200, 200, 400, 400, 1,
                                  BlackPixel (dpy, DefaultScreen (dpy)),
                                  WhitePixel (dpy, DefaultScreen (dpy)));
  XStoreName (dpy, w, "hyperbolic.c");

  /* raise it and wait */
  XSelectInput (dpy, w, StructureNotifyMask);
  XMapRaised (dpy, w);
  for(XEvent e; ( e.type != MapNotify ); XWindowEvent (dpy, w, StructureNotifyMask, &e));


  /* look up colours by name (defined in /usr/share/X11/rgb.txt) */
  char *colors[NCOLORS]={"navy", "red", "blue"};
  XColor xcolors[NCOLORS];

  for(int c=0; c<NCOLORS; c++) {
    XColor xc, sc;

    XAllocNamedColor(dpy, DefaultColormapOfScreen(DefaultScreenOfDisplay(dpy)),
                     colors[c], &sc, &xc);
    xcolors[c]=sc;
  }



  /* create a GC for drawing in the window */
  GC g = XCreateGC (dpy, w, 0, NULL);

  /* draw something */
  while (1) {

    /* get attributes of the window */
    XWindowAttributes wa;
    XGetWindowAttributes(dpy, w, &wa);


    /* set a random foreground color */
    XSetForeground(dpy, g, xcolors[random()%NCOLORS].pixel);

    XClearWindow(dpy, w);
    int wscale(double u){
      return wa.width*(u-(-2))/(2-(-2));
    }

    int hscale(double u){
      return wa.height*(u-(-2))/(2-(-2));
    }

    double phix(double u,double v) {return u*u+v*v;}
    double phiy(double u,double v) {return 2*u*v;}

    double psix(double u,double v) {return u;}
    double psiy(double u,double v) {return v;}

    for(double u=-1;u<1;u+=0.1){
      for(double v=-1;v<1;v+=0.1){
        XDrawLine(dpy, w, g,
                  wscale(phix(u,v)),    hscale(phiy(u,v)),
                  wscale(phix(u,v+0.1)),hscale(phiy(u,v+0.1)));
        XDrawLine(dpy, w, g,
                  wscale(phix(u,v))    ,hscale(phiy(u,v)),
                  wscale(phix(u+0.1,v)),hscale(phiy(u+0.1,v)));
        XDrawLine(dpy, w, g,
                  wscale(psix(u,v)),    hscale(psiy(u,v)),
                  wscale(psix(u,v+0.1)),hscale(psiy(u,v+0.1)));
        XDrawLine(dpy, w, g,
                  wscale(psix(u,v))    ,hscale(psiy(u,v)),
                  wscale(psix(u+0.1,v)),hscale(psiy(u+0.1,v)));

        /* flush changes and sleep */
        XFlush(dpy);
        usleep (100000);
      }}
  }

  XDestroyWindow( dpy, w );
  XCloseDisplay (dpy);
}
