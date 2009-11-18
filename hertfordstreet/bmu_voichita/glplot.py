##
## glplot.py ... combines OpenGL and wxPython to produce quick-and-dirty, zoomable line-plots
##
## Copyright (c) Gary Strangman, All Rights Reserved
## This software is provided AS-IS. Improvements are welcome. strang@nmr.mgh.harvard.edu
##
## NOTE:  left button and drag creates a zoom box, right button returns to full-plot view
##
## Requires PyOpenGL, Numeric, and wxPython, and Python 2.2+
## Tested on Linux and Windoze platforms. Does what I need it to do on both.
##

try:
    import im    # im module only required to save the generated bitmaps
except:
    pass

import glob, os, sys, string
import Numeric as N
from wxPython.wx import *


from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL import GLUT
from wxPython.wx import *
from wxPython.glcanvas import *
from Numeric import *
import math, os, sys

glplotcolors = [(0.,0.,1.),    # blue
                (0.,1.,0.),    # green
                (1.,0.,0.),    # red
                (0.,1.,1.),    # cyan
                (1.,0.,1.),    # magenta
                (1.,1.,0.)]    # yellow


#---------------------------------------------------------------------------------------

class RawOpengl(wxGLCanvas):
  def __init__(self, parent,*args,**kw):
    apply(wxGLCanvas.__init__,(self,parent),kw)
    EVT_SIZE(self,self.wxSize)
    EVT_PAINT(self,self.wxPaint)
    EVT_ERASE_BACKGROUND(self, self.wxEraseBackground)
    
  def wxSize(self, event):
    ### Size callback
    size = self.GetClientSize()
    if self.GetContext():
      self.SetCurrent()
      glViewport(0, 0, size.width, size.height)

  def wxEraseBackground(self, event):
    pass # Do nothing, to avoid flashing.

  def wxPaint(self,*dummy):
    dc = wxPaintDC(self)
    self.wxRedraw(None)

  def wxRedraw(self, *dummy):
    ### Capture rendering context
    dc = wxClientDC(self)
#    dc = wxPaintDC(self)
    self.SetCurrent()

    _mode = glGetDouble(GL_MATRIX_MODE)
    glMatrixMode(GL_PROJECTION)

    glPushMatrix()
    self.redraw()
    glFlush()
    glPopMatrix()

    ### Swap buffers
    self.SwapBuffers()

    glMatrixMode(_mode)

  def wxExpose(self, *dummy):
    self.wxRedraw()


#---------------------------------------------------------------------------------------
class OpenglMultiLinePlot(RawOpengl):
    """
    A class for drawing line plots on an openGL canvas.
    """
    def __init__(self, parent=None, autospin_allowed=1, xs=None, ys=None, errors=None, **kw):
        apply(RawOpengl.__init__, (self, parent), kw)
        self.parent = parent
        if ys is None:
            self.ys = None
            self.xs = None
        else: # len(ys.shape) == 1:
            self.set_ys(ys)
            self.set_xs(xs)
        self.errors = errors
        self.arrow = 0
        self.font = GLUT.GLUT_BITMAP_HELVETICA_12
#        self.font = WGL.
#        self.font = GLTTwxFont.GLTTFont('arialbd',9)
        self.parent = parent
        self.drawcount = 0
        self.redraw = self.paintit
        self.xscale = 1
        self.yscale = 1
        self.lineweight = 1.0
        self.bkgdcolor = [0., 0., 0., 0.]
        self.settingbackground = 0
        self.xminusflag = 0
        self.yminusflag = 0
        self.box = None
        self.dataxmin = min(ravel(self.xs))
        self.dataymin = min(ravel(self.ys))
        self.dataxmax = max(ravel(self.xs))
        self.dataymax = max(ravel(self.ys))
        self.plotxmin = self.dataxmin
        self.plotymin = self.dataymin
        self.plotxmax = self.dataxmax
        self.plotymax = self.dataymax
        EVT_MOUSE_EVENTS(self, self.OnMouseEvent)
        EVT_CHAR(self,self.OnChar)

#    def wxPaint(self,*dummy):
#        dc = wxPaintDC(self)
#        self.paintit()

    def OnChar(self, event):
#        print event.KeyCode()
        if event.KeyCode() < 256:
            key = string.upper(chr(event.KeyCode()))
            if key == 'L':
                popup = wxFileDialog(NULL, "Choose LOG filename ...", "",
                                     "", "*", wxSAVE, wxPoint(100,100))
                popup.ShowModal()
                # @@@need to make "enter" default to Save, somehow
                a = glReadPixels(0,0,self.GetSize().x,self.GetSize().y,GL_RGB,GL_UNSIGNED_BYTE)
                size = self.GetClientSizeTuple()
                a = array(size,Int).tostring() + a
                f=open(popup.GetFilename(),'wb')
                f.write(a)
                f.close()

    def OnMouseEvent(self,event):
        size = self.GetSize()
        # determine where (in proportions) on screen the click happened
        xr = float(event.GetX())/size.x  # GetX=0 at left
        yr = float(event.GetY())/size.y  # GetY=0 at top
        # scale this location to where WITHIN THE PLOT the click happened (in proportions)
        # ... with 0,0 at lower left of PLOT area
        xrs = (xr-(1-self.xscale)/2.)/float(self.xscale)    # scale to the plot area
        yrs = 1-(yr-(1-self.yscale)/2.)/float(self.yscale)  # invert Y and scale to plot area
        if event.LeftDown():
            self.xminusflag = 0  #was selection box dragged LEFT?
            self.yminusflag = 0  #was selection box dragged UP?
            self.box = [(xrs*(self.plotxmax-self.plotxmin)+self.plotxmin),
                        (yrs*(self.plotymax-self.plotymin)+self.plotymin)]
            self.xstart = xr
            self.ystart = yr
        elif self.box and event.LeftIsDown() and not event.LeftDown():
            # compute position of other box-corner within plot
            nxrs = (xrs*(self.plotxmax-self.plotxmin)+self.plotxmin)
            nyrs = (yrs*(self.plotymax-self.plotymin)+self.plotymin)
            if nxrs < self.box[0]:
                self.xminusflag = 1
            else:
                self.xminusflag = 0
            if nyrs < self.box[1]:
                self.yminusflag = 1
            else:
                self.yminusflag = 0
            if self.box[0]<>nxrs or self.box[1]<>nyrs:
                self.box = [self.box[0], self.box[1], nxrs, nyrs]
            else:  # may need to convert a 4-element box to a 2-element box
                self.box = [nxrs, nyrs]
            self.xend = xr
            self.yend = yr
            self.paintit()
        elif event.LeftUp():
            if len(self.box)>2:
                # if dragged up or left, exchange value-pairs
                if self.box[0] > self.box[2]:
                    self.box[0],self.box[2] = self.box[2],self.box[0]
                if self.box[1] > self.box[3]:
                    self.box[1],self.box[3] = self.box[3],self.box[1]
                self.plotxmin = self.box[0]
                self.plotymin = self.box[1]
                self.plotxmax = self.box[2]
                self.plotymax = self.box[3]
            self.xminusflag = 0
            self.yminusflag = 0
            self.box = None
            self.paintit() # can't use wxRedraw for some reason
        if event.RightUp():
            self.plotxmin = self.dataxmin
            self.plotymin = self.dataymin
            self.plotxmax = self.dataxmax
            self.plotymax = self.dataymax
            self.box = None
            self.paintit() # can't use wxRedraw for some reason

    def OnSize(self, event):
        size = self.GetClientSize()
        if self.GetContext() != 'NULL':
            self.SetCurrent()
            glViewport(0, 0, size.width, size.height)

    def changelineweight(self,step):
        self.lineweight += step
        if self.lineweight <= 0:
            self.lineweight = 0.1
        self.paintit()

    def save_colorpixelmap(self):
        string = glReadPixels(0,0,self.GetSize().x,self.GetSize().y,GL_RGB,GL_UNSIGNED_BYTE)
        size = list(self.GetClientSizeTuple())
        a = fromstring(string,Int8)         # convert pixels to array
        print a.shape, size
        size[0],size[1] = size[1],size[0]   # swap x,y dimensions for proper unraveling
        r = a[0::3]+0
        g = a[1::3]+0
        b = a[2::3]+0
        r.shape = size
        g.shape = size
        b.shape = size
        carray = array([r[::-1,:],g[::-1,:],b[::-1,:]]) # up-down flip the image
        print carray.shape, type(carray), carray.typecode(), min(ravel(carray)), max(ravel(carray))
        im.ashow(carray)

    def save_graypixelmap(self):
        string = glReadPixels(0,0,self.GetSize().x,self.GetSize().y,GL_LUMINANCE,GL_FLOAT)
        size = list(self.GetClientSizeTuple())
        a = fromstring(string,Float32)         # convert pixels to array
        print a.shape, size
        size[0],size[1] = size[1],size[0]   # swap x,y dimensions for proper unraveling
        carray = reshape(a,size)*255        # must be a luminance map
        print carray.shape, type(carray), carray.typecode(), min(ravel(carray)), max(ravel(carray))
        im.ashow(carray[::-1,:])

    def setbackground(self,color):
        if self.settingbackground:
            return
        if len(color) == 3:
            color = list(color) + [0.]
        apply(glClearColor,color)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        self.bkgdcolor = color
        self.settingbackground = 1
        self.paintit()
        self.settingbackground = 0

    def set_xs(self, xs=None):
        if self.ys is None:
            xs = None
            return
        elif xs is None:
            xs = arange(self.ys.shape[0])
        self.xs = xs
        self.x_offset = -xs[0]
        self.x_scale = 1.0/(max(xs)-min(xs))
        self.dataxmin = min(ravel(self.xs))
        self.dataxmax = max(ravel(self.xs))
        self.plotxmin = self.dataxmin
        self.plotxmax = self.dataxmax

    def transform(self, ys):
        # should convert to a rank-2 array
        return add.reduce(ys)

    def set_ys(self, ys):
        if ys is None:
            self.ys = None
            return
        while len(ys.shape) > 2:
            ys = self.transform(ys)
        self.ys = ys
        self.y_offset = -ys[0]
        try:
            self.y_scale = 1.0/(max(ys)-min(ys))
        except ZeroDivisionError:
            self.y_scale = 1.0
        self.dataymin = min(ravel(self.ys))
        self.dataymax = max(ravel(self.ys))
        self.plotymin = self.dataymin
        self.plotymax = self.dataymax

    def set_errors(self, errors):
        if errors is None:
            self.errors = None
            return
        while len(errors.shape) > 2:
            errors = self.transform(errors)
        self.errors = errors
        self.dataymin = min(ravel(self.ys-abs(self.errors)))
        self.dataymax = max(ravel(self.ys+abs(self.errors)))
        self.plotymin = self.dataymin
        self.plotymax = self.dataymax

    def paintit(self):#, event):
        ### PREPARE FOR DRAWING AND CLEAR WINDOW
        self.setbackground(self.bkgdcolor)
        if self.ys is None:
            return

        ### SET UP FOR REDRAWING
        if not self.xs:
            self.set_xs()
        size = self.GetClientSize()
        w,h = size.x, size.y
        WZ = float(w) / len(self.xs)
        HZ = float(h) / len(self.ys)
        glLoadIdentity()
        glEnable(GL_LINE_SMOOTH)
        glEnable(GL_BLEND)
        glHint(GL_LINE_SMOOTH_HINT, GL_NICEST)
        glHint(GL_POINT_SMOOTH_HINT, GL_NICEST)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        # IMPORTANT COORDINATE TRANSFORMATIONS
        self.xscale = 0.84
        self.yscale = 0.8
        glScale(self.xscale, self.yscale, 1.0) # scale everything hereafter in this matrix
        glOrtho(self.plotxmin, self.plotxmax,
                self.plotymin, self.plotymax,
                0, 1)

        # Make sure both are 2D, so plot code can be general for multi and single lines
        if len(self.ys.shape) == 1:
            self.ys = self.ys[:,NewAxis]
            if self.errors:
                self.errors.shape = (len(self.errors),1)

        ### PLOT ERRORBARS (SAME COLOR AS ASSOCIATED TIMESERIES)
        if hasattr(self, 'errors') and self.errors:
            # loop through all timeseries'
            for i in range(self.errors.shape[1]):
                if self.errors.shape[1] > 1:
                    colortrio = glplotcolors[i%len(glplotcolors)]
                    apply(glColor3f,colortrio)
                else:
                    glColor3f(1.,1.,0.)
                glLineWidth(1.0)
                lower = self.ys[:,i] - self.errors[:,i]
                upper = self.ys[:,i] + self.errors[:,i]
                glBegin(GL_LINES)
                for x,yl, yu in transpose(array([self.xs, lower, upper])):
                    if x>=self.plotxmin and x<=self.plotxmax:
                        glVertex2f(x,yl)
                        glVertex2f(x,yu)
                glEnd()


        ### PLOT TIMESERIES (after/ON-TOP-OF ERRORBARS)
        # loop through all timeseries'
        for i in range(self.ys.shape[1]):
            glLineWidth(self.lineweight)
            if self.ys.shape[1] > 1:
                colortrio = glplotcolors[i%len(glplotcolors)]
                apply(glColor3f,colortrio)
            else:
                glColor3f(1.,1.,1.)
            d = array((self.xs+0.0, self.ys[:,i]))
            t = transpose(d)
            glBegin(GL_LINE_STRIP)
            for vert in t:
                if vert[0]>=self.plotxmin and vert[0]<=self.plotxmax:
                    glVertex(vert[0],vert[1])
            glEnd()

        ### PLOT X/Y-AXIS LINES (white)
        glColor3f(1.,1.,1.)
        glLineWidth(1.5)
        glBegin(GL_LINES)
        glVertex2i(self.plotxmin, 0)
        glVertex2i(self.plotxmax, 0)
        glVertex2i(0, self.plotymin)
        glVertex2i(0, self.plotymax)
        glEnd()

        ###
        ### TEXT PLOTTING CODE ... USED TO USE PyGLTT; NOW USES GLUT (until GLTT/FTGL works again)
        ###
        self.textcolor = (1,1,1)

        # Pick round numbers to be displayed
        xrange_sigfig = log10(self.plotxmax-self.plotxmin)
        yrange_sigfig = log10(self.plotymax-self.plotymin)
#        print self.plotymax, self.plotymin, yrange_sigfig
        if xrange_sigfig<=1:
            xrounddigits = int(xrange_sigfig)+3
        else:
            xrounddigits = 0
        if yrange_sigfig<=1:
            yrounddigits = int(yrange_sigfig)+3
        else:
            yrounddigits = 0
#        print self.plotymax, self.plotymin, yrange_sigfig

        # And properly format the numeric text strings to be dispalyed
        if xrounddigits:
            xminstr = str(round(self.plotxmin,xrounddigits))
            xmaxstr = str(round(self.plotxmax,xrounddigits))
        else:
            xminstr = str(int(round(self.plotxmin,xrounddigits)))
            xmaxstr = str(int(round(self.plotxmax,xrounddigits)))
        if yrounddigits:
            yminstr = str(round(self.plotymin,yrounddigits))
            ymaxstr = str(round(self.plotymax,yrounddigits))
        else:
            yminstr = str(int(round(self.plotymin,yrounddigits)))
            ymaxstr = str(int(round(self.plotymax,yrounddigits)))
        
        # Figure out where to place the numerical labels
        # NOTE: Though we are using an Identity matrix, bitmap font locations apparently
        # want to be localized in pixel-coordinates (hence all the GetSize() calls)
        glPushMatrix()
        glLoadIdentity()
        xaxis_yoffset = -0.93*self.GetSize().y
        yaxis_xoffset = -0.94*self.GetSize().x
        xaxis_xmin = (-self.xscale-0.01)*self.GetSize().x
        xaxis_xmax = (self.xscale-0.01)*self.GetSize().x
        yaxis_ymin = -0.86*self.GetSize().y
        yaxis_ymax = 0.78*self.GetSize().y

#        print
#        print self.GetSize(), self.GetClientSize()
#        print "X-axis min: ",xaxis_xmin, xaxis_yoffset, ' / ', xminstr
#        print "X-axis max: ",xaxis_xmax, xaxis_yoffset, ' / ', xmaxstr
#        print "Y-axis min: ",yaxis_xoffset, yaxis_ymin, ' / ', yminstr
#        print "Y-axis max: ",yaxis_xoffset, yaxis_ymax, ' / ', ymaxstr

        ### y-axis maximum
        self.draw_text(self,
                       yaxis_xoffset, #self.GetSize().x*xoffset,
                       yaxis_ymax, #self.GetSize().y*ymaxoffset,
                       ymaxstr,None,None)
        ### y-axis minimum
        self.draw_text(self,
                       yaxis_xoffset, #self.GetSize().x*xoffset,
                       yaxis_ymin, #self.GetSize().y*yminoffset,
                       yminstr,None,None)
#                       GLTTwxFont.ALIGN_RIGHT, GLTTwxFont.VALIGN_BOTTOM)
        ### x-axis maximum
        self.draw_text(self,
                       xaxis_xmax, #self.GetSize().x*xoffset,
                       xaxis_yoffset, #self.GetSize().y*ymaxoffset,
                       xmaxstr,None,None)
        ### x-axis minimum
        self.draw_text(self,
                       xaxis_xmin, #self.GetSize().x*xoffset,
                       xaxis_yoffset, #self.GetSize().y*yminoffset,
                       xminstr,None,None)
#                       GLTTwxFont.ALIGN_RIGHT, GLTTwxFont.VALIGN_BOTTOM)
        ### arrow value
#        self.draw_text(self,
#                       xarrowoffset,
#                       self.GetSize().y*ymaxoffset,
#                       ' '+str(round(self.ys[self.arrow],1)), 0, 0) #,
#                       GLTTwxFont.ALIGN_LEFT, GLTTwxFont.VALIGN_BOTTOM)
        ### arrow timepoint
#        self.draw_text(self,
#                       xarrowoffset,
#                       self.GetSize().y*yminoffset,
#                       ' '+str(self.arrow),None,None)
#                       GLTTwxFont.ALIGN_LEFT, GLTTwxFont.VALIGN_BOTTOM)

        # Finally, draw a bounding-box (bottom/top left/right)
        # NOTE: No need to use GetSize() here; we have an Identity matrix and are
        # drawing normal (non-bitmap-text) stuff
        BL = [-self.xscale,-self.yscale]
        TL = [-self.xscale, self.yscale]
        TR = [self.xscale, self.yscale] 
        BR = [self.xscale, -self.yscale] 

        #print BL, TL, TR, BR
        glPointSize(1.0)
        glColor3f(0.3,0.3,0.3)
        glBegin(GL_LINE_STRIP)
        glVertex2f(BL[0],BL[1])
        glVertex2f(TL[0],TL[1])
        glVertex2f(TR[0],TR[1])
        glVertex2f(BR[0],BR[1])
        glVertex2f(BL[0],BL[1])
        glEnd()

        glPopMatrix()

        ### LAST, BUT NOT LEAST, DRAW SELECTION-BOX ... (RED)
        if self.box and len(self.box)==4:
            glPointSize(2.0)
            glColor3f(1.,0.,0.)
            glBegin(GL_LINE_STRIP)
            glVertex2f(self.box[0], self.box[1])
            glVertex2f(self.box[2], self.box[1])
            glVertex2f(self.box[2], self.box[3])
            glVertex2f(self.box[0], self.box[3])
            glVertex2f(self.box[0], self.box[1])
            glEnd()

        # FINALLY, CLIP VIEW TO SPECIFIED SUB-PORTION OF WINDOW
#        glEnable(GL_CLIP_PLANE1)
#        glEnable(GL_CLIP_PLANE2)
#        glEnable(GL_CLIP_PLANE3)
#        glEnable(GL_CLIP_PLANE4)
#        glClipPlane(GL_CLIP_PLANE1, [0., 1., 0., -self.plotymin]) # clips off the bottom
#        glClipPlane(GL_CLIP_PLANE2, [0., -1., 0., self.plotymax]) # clips off the top
#        glClipPlane(GL_CLIP_PLANE3, [1., 0., 0., -self.plotxmin]) # clips off the left
#        glClipPlane(GL_CLIP_PLANE4, [-1., 0., 0., self.plotxmax]) # clips off the right

        self.SwapBuffers()  # NECESSARY, or screen doesn't redraw

    def draw_text(self, canvas, x,y,text,align,valign):
        apply(glColor3f, self.textcolor)
        size = self.GetClientSize()
        w,h = float(size.x), float(size.y)
        glRasterPos2f(x/w,y/h)
        for char in text:
            print x,y,self.font,char
#            GLUT.glutBitmapCharacter(self.font,ord(char)) #text[0]) #self.font,text)
#        self.font.write_string(canvas, x, y, text, align, valign)

    def getpropX(self, x):
        w = self.GetClientSize().x
        p = (x - w*.1) / (w*.8)
        return p

    def TimeToQuit(self, event):
        ### REMAKE LINEPLOT WHEN SELF.BOX IS RE-CREATED
        self.Close(true)


def glplot(yvals=None,xvals=None,errors=None):
    """
Create a plot using a wxGLCanvas.

Usage:   glplot(
                x=None,             x-axis data
                y=None,             y-axis data, skip x and use y=[data] for x=range(len(y))
                errors=None,       y-axis errorbar data
"""
    if not xvals and not yvals:
        return
    if not xvals:
        xvals = N.arange(yvals.shape[0])

    class MyApp(wxApp):
        def OnInit(self): #,x=None,y=None,errors=None):
            windowXpixels = 8  # 8 pixels of frame OUTSIDE the canvas
            windowYpixels = 27 # 27 pixels of frame plus title-bar OUTSIDE the canvas
            self.frame = wxFrame(NULL, -1, "wxPython Context",
                                 wxPoint(0,0),
                                 wxSize(1200+windowXpixels,400+windowYpixels))

            self.mainmenu = wxMenuBar()
            filemenu = wxMenu()
            cimgID = wxNewId()
            gimgID = wxNewId()
            exitID = wxNewId()
            filemenu.Append(cimgID, 'Save C&olor\tAlt-C', 'Save color pixelmap using IC.exe')
            filemenu.Append(gimgID, 'Save G&ray\tAlt-G', 'Save gray pixelmap using IC.exe')
            filemenu.Append(exitID, 'E&xit\tAlt-X', 'Quit')
            EVT_MENU(self, cimgID, self.OnCImgSave)
            EVT_MENU(self, gimgID, self.OnGImgSave)
            EVT_MENU(self, exitID, self.OnFileExit)
            self.mainmenu.Append(filemenu, '&File')
            propmenu = wxMenu()
            fontID = wxNewId()
            lineweightupID = wxNewId()
            lineweightdnID = wxNewId()
            bkgdID = wxNewId()
            propmenu.Append(fontID, 'F&onts\tAlt-F', 'Change font for all text items')
            propmenu.Append(lineweightupID, 'I&ncrease lineweight\tAlt-I', 'Increase plotting line weight')
            propmenu.Append(lineweightdnID, 'D&ecrease lineweight\tAlt-D', 'Decrease plotting line weight')
            propmenu.Append(bkgdID, 'B&ackground color\tAlt-B', 'Change plot background color')
            EVT_MENU(self, fontID, self.OnFont)
            EVT_MENU(self, lineweightupID, self.OnLineweightup)
            EVT_MENU(self, lineweightdnID, self.OnLineweightdn)
            EVT_MENU(self, bkgdID, self.OnBkgd)
            self.mainmenu.Append(propmenu, '&Edit')
            self.frame.SetMenuBar(self.mainmenu)

            # Now, create the line-plot part
            self.win = OpenglMultiLinePlot(self.frame,autospin_allowed=0)
            self.frame.Show(TRUE)
            self.SetTopWindow(self.frame)
            return TRUE
        def OnCImgSave(self,event):
            self.win.save_colorpixelmap()
        def OnGImgSave(self,event):
            self.win.save_graypixelmap()
        def OnFileExit(self,event):
            sys.exit()
        def OnFont(self,event):
            data = wxFontData()
            dlg = wxFontDialog(self.frame, data)
            if dlg.ShowModal() == wxID_OK:
                data = dlg.GetFontData()
                font = data.GetChosenFont()
                print 'You selected: ',font.GetFaceName(),', ',str(font.GetPointSize()),', color ',data.GetColour().Get()
                self.win.fontname = font.GetFaceName()
                self.win.fontstype = font.GetStyle()
                self.win.fontsize = font.GetPointSize()
                self.win.fontcolor = data.GetColour().Get()
            dlg.Destroy()
        def OnLineweightup(self,event):
            self.win.changelineweight(+1)
        def OnLineweightdn(self,event):
            self.win.changelineweight(-1)
        def OnBkgd(self,event):
            data = wxColourData()
            dlg = wxColourDialog(self.frame, data)
            if dlg.ShowModal() == wxID_OK:
                data = dlg.GetColourData()
                wxcolor = data.GetColour()
            dlg.Destroy()
            color = N.array([wxcolor.Red(), wxcolor.Green(), wxcolor.Blue()])
            newcolor = color / 255.
            self.win.setbackground(newcolor)

    app = MyApp(0)
    app.win.set_xs(xvals)
    app.win.set_ys(yvals)
    app.win.set_errors(errors)
    app.MainLoop()
