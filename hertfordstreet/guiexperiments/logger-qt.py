#!/usr/bin/env python
import sys, time
from qt import * # Generally advertised as safe

class Logger(QWidget):
    def __init__(self, *args):
        QWidget.__init__(self, *args)
        self.setCaption("Timestamp logging application")
        self.layout = QGridLayout(self, 3, 2, 5, 10)
        self.tsdisp = QTextEdit(self)
        self.tsdisp.setMinimumSize(250, 300)
        self.tsdisp.setTextFormat(Qt.PlainText)
        self.tscount = QLabel("", self)
        self.tscount.setFont(QFont("Sans", 24))
        self.log = QPushButton("&Log Timestamp", self)
        self.quit = QPushButton("&Quit", self)
        self.layout.addMultiCellWidget(self.tsdisp, 0, 2, 0, 0)
        self.layout.addWidget(self.tscount, 0, 1)
        self.layout.addWidget(self.log, 1, 1)
        self.layout.addWidget(self.quit, 2, 1)
        self.connect(self.log, SIGNAL("clicked()"),
                     self.log_timestamp)
        self.connect(self.quit, SIGNAL("clicked()"),
                     self.close)
    def log_timestamp(self):
        stamp = time.ctime()
        self.tsdisp.append(stamp)
        self.tscount.setText(str(self.tsdisp.lines()))
if __name__ == "__main__":
    app = QApplication(sys.argv)
    app.connect(app, SIGNAL('lastWindowClosed()'), app,
                  SLOT('quit()'))
    logger = Logger()
    logger.show()
    app.setMainWidget(logger)
    app.exec_loop()
