from Tkinter import *

class Application(Frame):

    def createWidgets(self):
        self.QUIT=Button(self, text="doom", fg="red", bg="blue")

        self.QUIT["command"] = self.quit
        self.QUIT.pack({"side": "right"})
        self.QUIT.pack(expand=1)

        self.hi_there=Button(self)
        self.hi_there["text"] = "Hello",
        self.hi_there["command"] = self.say_hi
        self.hi_there.pack(side="right")

        self.entrythingy = Entry()
        self.entrythingy.pack(expand=1)

        self.contents=StringVar()
        self.contents.set("this is a variable")
        self.entrythingy["textvariable"]=self.contents
        self.entrythingy.bind('<Key-Return>', self.say_hi)

    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.pack(expand=1)
        self.createWidgets()

    def say_hi(self, str="hello"):
        print self.contents.get()
        self.contents.set( str)


root=Tk()
app = Application(master=root)
app.mainloop()
root.destroy()










