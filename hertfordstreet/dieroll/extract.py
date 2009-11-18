#turn a directory hierarchy of .url files into a page of hyperlinks
import glob
import os
import re

def parse(file):
    '''read a .url file and output an html hyperlink'''
    filetitle=os.path.splitext(os.path.split(file)[1])[0]
    f = open(file, 'r')
    lines = f.readlines()
    for l in lines:
        urls=re.findall(pattern, l)
        for u in urls:
            u=re.sub(r'URL=','',u)
            print '<p><a href=\"'+u+'">'+filetitle+' </a></p>'
    f.close()
    


def recurse (directory):
    '''parse every file in a directory and all subdirectories'''
    print "<h1>recursing " + directory+"</h1>"

    list= [os.path.join(directory, f) for f in os.listdir(directory)]
    dirs=[f for f in list if os.path.isdir(f)]
    files=[f for f in list if os.path.isfile(f)]

    for f in files:
        parse(f)
    for d in dirs:
        recurse(d)
  

pattern=re.compile("^URL=[^\r]*")

print "<html>"
recurse(".")
print "</html>"
