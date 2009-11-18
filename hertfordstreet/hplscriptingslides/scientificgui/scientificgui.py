#!/usr/bin/env python

from Tkinter import *
from ScrolledText import *
import math

def comp_s(Event='none'):
    global s
    s.set('%g' % math.sin(float(r.get())))


root=Tk()
root.title('hello')

root.maxsize(800,300) #don't need these!
root.minsize(200,150) #they're just to annoy the user


top=Frame(root)
top.pack()



hwtext = Label(top, text='Hello, World!', font='times 18 bold')
hwtext.pack(side='top', pady=20)

rframe = Frame(top)
rframe.pack(side='top', padx=20)

stext = Label(rframe, text='the sine of')
stext.pack(side='left')

r=StringVar()
r.set('1.2')
s=StringVar()

r_entry = Entry(rframe, width=6, relief='sunken', textvariable=r)
r_entry.pack(side='left')
r_entry.bind('<Return>', comp_s)

s_label = Label(rframe, textvariable=s, width = 18)
s_label.pack(side='right')

quit_button = Button(top,
                     text='goodbye',
                     background='red',
                     command=root.destroy)
quit_button.pack(side='bottom',ipadx=20,ipady=20, padx=20,pady=5)





root.mainloop()
