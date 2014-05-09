#include<stdlib.h>
#include<unistd.h>
#include<X11/Xlib.h>

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
#define NCOLORS 4
  char *colors[NCOLORS]={"navy", "red", "red", "blue"};
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


    void drawparallelograms(double phix(double,double),
                            double phiy(double,double),
                            double u0,double u1,double v0,double v1,
                            int colour){
        XSetForeground(dpy, g, xcolors[colour].pixel);
        XDrawLine(dpy, w, g,
                  wscale(phix(u0,v0)),hscale(phiy(u0,v0)),
                  wscale(phix(u0,v1)),hscale(phiy(u0,v1)));
        XDrawLine(dpy, w, g,
                  wscale(phix(u0,v0)),hscale(phiy(u0,v0)),
                  wscale(phix(u1,v0)),hscale(phiy(u1,v0)));
    }

    void drawsolidparallelograms(double phix(double,double),
                            double phiy(double,double),
                            double u0,double u1,double v0,double v1,
                            int colour){
      double u01=(u0+u1)/2;
      double v01=(v0+v1)/2;
      drawparallelograms(phix,phiy,u0,u01,v0,v01,colour);
      drawparallelograms(phix,phiy,u01,u1,v01,v1,colour);
    }



    for(double u=-1;u<1;u+=0.1){
      for(double v=-1;v<1;v+=0.1){

        drawsolidparallelograms(phix,phiy,u,u+0.1,v,v+0.1,2);
        drawsolidparallelograms(psix,psiy,u,u+0.1,v,v+0.1,3);

        /* flush changes and sleep */
        XFlush(dpy);
        usleep (100000);

        drawparallelograms(phix,phiy,u,u+0.1,v,v+0.1,0);
        drawparallelograms(psix,psiy,u,u+0.1,v,v+0.1,1);
      }}
  }

  XDestroyWindow( dpy, w );
  XCloseDisplay (dpy);
}
