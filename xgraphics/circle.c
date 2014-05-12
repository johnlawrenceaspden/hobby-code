#include<X11/Xlib.h>
#include<stdlib.h>

/* gcc -std=gnu99 -o circle circle.c -lX11 */

int main (int argc, char *argv[])
{

  /* connect to the X server and make a window */
  Display *dpy = XOpenDisplay (getenv ("DISPLAY"));
  Window w = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy),
                                  100, 100, 640, 480, 1,
                                  BlackPixel (dpy, DefaultScreen (dpy)),
                                  WhitePixel (dpy, DefaultScreen (dpy)));

  /* raise it and wait */
  XSelectInput (dpy, w, StructureNotifyMask);
  XMapRaised (dpy, w);
  for(XEvent e; ( e.type != MapNotify );
      XWindowEvent (dpy, w, StructureNotifyMask, &e));

  /* create a graphics context for drawing in the window */
  GC g = XCreateGC (dpy, w, 0, NULL);

  /* draw a circle */
  XDrawArc(dpy,w,g,200,100,150,150,0,360*64);
  XFlush(dpy);

  /*wait for key press*/
  XSelectInput (dpy, w, KeyReleaseMask);
  for(XEvent e; ( e.type != KeyRelease ); 
      XWindowEvent (dpy, w, KeyReleaseMask, &e));

  /*clean up*/
  XDestroyWindow( dpy, w );
  XCloseDisplay (dpy);
}
