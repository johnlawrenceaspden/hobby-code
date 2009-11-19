from xml.dom import minidom, Node


def recurse(node):
    print node
    if node.nodeType == Node.ELEMENT_NODE:
        print 'Element name: %s' % node.nodeName
        for(name, value) in node.attributes.items():
            print 'Name %s Value %s' % (name, value)
        if node.attributes.get('ID') is not None:
            print ' ID: %s' % node.attributes.get('ID').value
                
    if node.nodeType==node.TEXT_NODE:
                print(node.wholeText)

    for n in node.childNodes:
         recurse(n)


dom=minidom.parse("test.xml")

recurse(dom)

title=dom.getElementsByTagName("title")[0]
print "<title>%s</title>" % title.childNodes[0].wholeText




dom.writexml(open("test2.xml", "w"))
