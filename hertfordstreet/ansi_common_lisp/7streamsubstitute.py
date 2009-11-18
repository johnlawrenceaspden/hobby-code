def stringstream(str):
        for i in str:
                yield(i)
        yield("eof")

def listtostring(list):
        a=""
        for i in list:
            a=a+i
        return a

class buffer(object):
        contents=[]

        def add(self,char):
            self.contents.append(char)

        def pop(self):
            return self.contents.pop(0)

        def equal(self, string):
            #print "compare ",string, listtostring(self.contents)
            return string==listtostring(self.contents)

        def clear(self):
            self.contents=[]

        def flush(self):
            return listtostring(self.contents)
                

def streamsubstitute(search, replace, stream):
    buf=buffer()
    for i in search:
        buf.add(stream.next())

    while(True):
        if(buf.equal(search)):
            print replace,
            buf.clear()
            for i in search:
                buf.add(stream.next())
        else:
            c=stream.next()
            if c=="eof":
                print buf.flush(),
            else:
                print buf.pop(),
                buf.add(c)
                
    


    
streamsubstitute("baro","baric", stringstream("barbarous barbarism barometer"))
    
