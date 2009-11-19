import re, sys

words, lines, chars = 0,0,0
sentences, clauses = 0,0
tags=0
paras = 1

sentencemarks = ".?!"
clausemarks = sentencemarks + "&(),;:\-"
sentenceRE = re.compile('[%s] +' % sentencemarks)
clauseRE = re.compile('[%s] + ' % clausemarks)
htmltagsRE = re.compile("<[^<]*>")

def resetCounters():
    chars, words, lines, sentences, clauses = 0,0,0,0,0
    paras = 1



def count(re, line):
    exps=re.findall(line)
    print exps
    return len(exps)

if len(sys.argv) != 2:
    name = "wordcount.py"
else:
    name=sys.argv[1]
    
inp = open(name, "r")

for line in inp:
        print line.strip()
        lines += 1
        if line.strip() == "":
            paras += 1
        list = line.split()
        words += len(list)
        chars  += len (line)
        sentences += count(sentenceRE, line)
        clauses += count(clauseRE, line)
        tags += count(htmltagsRE, line)

  
print '''
The file %s has:
    %d\t characters
    %d\t words
    %d\t lines in
    %d\t paragraphs with
    %d\t sentences
    %d\t clauses and
    %d\t tags.
''' % (inp.name, chars, words, lines, paras, sentences, clauses, tags)        


inp.close()


