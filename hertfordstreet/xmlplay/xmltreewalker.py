import sys, string
from xml.dom import minidom, Node

def walk(parent, outFile, level):                               # [1]
    for node in parent.childNodes:
        if node.nodeType == Node.ELEMENT_NODE:
            # Write out the element name.
            printLevel(outFile, level)
            outFile.write('Element: %s\n' % node.nodeName)
            # Write out the attributes.
            attrs = node.attributes                             # [2]
            for attrName in attrs.keys():
                attrNode = attrs.get(attrName)
                attrValue = attrNode.nodeValue
                printLevel(outFile, level + 2)
                outFile.write('Attribute -- Name: %s  Value: %s\n' % \
                    (attrName, attrValue))
            # Walk over any text nodes in the current node.
            content = []                                        # [3]
            for child in node.childNodes:
                if child.nodeType == Node.TEXT_NODE:
                    content.append(child.nodeValue)
            if content:
                strContent = string.join(content)
                printLevel(outFile, level)
                outFile.write('Content: "')
                outFile.write(strContent)
                outFile.write('"\n')
            # Walk the child nodes.
            walk(node, outFile, level+1)

def printLevel(outFile, level):
    for idx in range(level):
        outFile.write('    ')

def run(inFileName):                                            # [5]
    outFile = sys.stdout
    doc = minidom.parse(inFileName)
    rootNode = doc.documentElement
    level = 0
    walk(rootNode, outFile, level)

def main():
    args = sys.argv[1:]
    if len(args) != 1:
        run("searches.xml")
    run(args[0])


if __name__ == '__main__':
    main()
