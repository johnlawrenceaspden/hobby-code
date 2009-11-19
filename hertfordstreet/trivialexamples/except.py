try:
    fsock=open("/home/xxxx/wget-logs")
    fsock.close()
except:
    print "not there"
else:
    print "there"
    
print "always"


