#!/usr/bin/env python

import sys
import pygtk
import gtk
import gtk.glade

gladefile="pywine.glade"

class Wine:
    def __init__(self, wine="", winery="", grape="", year=""):
        self.wine=wine
        self.winery=winery
        self.grape=grape
        self.year=year

    def getList(self):
        return self.wine, self.winery, self.grape, self.year

class wineDialog:
    def __init__(self, wine="",  winery="", grape="", year=""):
        self.wine=Wine(wine, winery, grape,year)
        
    def run(self):
        self.wTree = gtk.glade.XML(gladefile, "wineDlg")
        self.dlg = self.wTree.get_widget("wineDlg")

        #abortion 1
        self.enWine = self.wTree.get_widget("enWine")
        self.enWine.set_text(self.wine.wine)
        self.enWinery = self.wTree.get_widget("enWinery")
        self.enWinery.set_text(self.wine.winery)
        self.enGrape = self.wTree.get_widget("enGrape")
        self.enGrape.set_text(self.wine.grape)
        self.enYear = self.wTree.get_widget("enYear")
        self.enYear.set_text(self.wine.year)	

        self.result=self.dlg.run()

        #abortion 2
        self.wine.wine = self.enWine.get_text()
        self.wine.winery = self.enWinery.get_text()
        self.wine.grape = self.enGrape.get_text()
        self.wine.year = self.enYear.get_text()
        
        self.dlg.destroy()
        return self.result, self.wine
        

class pyWine:
    def __init__(self):
        self.wTree = gtk.glade.XML(gladefile, "mainWindow")
        
        dic={ "on_mainWindow_destroy" : gtk.main_quit,
              "on_AddWine" : self.OnAddWine}
        self.wTree.signal_autoconnect(dic)

        self.wineView = self.wTree.get_widget("wineView")
        self.AddWineListColumn("Wine", 0)
        self.AddWineListColumn("Wine", 1)
        self.AddWineListColumn("Wine", 2)
        self.AddWineListColumn("Wine", 3)
       
        self.wineList = gtk.ListStore(str, str, str, str)
        self.wineView.set_model(self.wineList)

    def AddWineListColumn(self, title, columnId):
        column=gtk.TreeViewColumn(title, gtk.CellRendererText(), text=columnId)
        self.wineView.append_column(column)
        

    def OnAddWine(self, widget):
        print "OnAddWine"
        self.a=wineDialog("flaps")
        result, wine =self.a.run()
        print result, wine
        if(result==gtk.RESPONSE_OK):
            self.wineList.append(wine.getList())

if __name__=="__main__":
    pyw=pyWine()
    gtk.main()
        
        
