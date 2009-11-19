class Employee:
    "one of those tedious boilerplate object oriented example classes"
    def __init__(self,
                 argEmployeeName,
                 argTaxDeductions = 1,
                 argMaritalStatus = "single"):

        self.EmployeeName  = argEmployeeName
        self.TaxDeductions = argTaxDeductions
        self.MaritalStatus = argMaritalStatus
        


a=Employee("fred")
b=Employee("missy")
c=Employee("jack",0,'double')


def cat(o):
    'freaky metaclass sorcery!'
    print o
    for f in dir(o):
            print f,
            print ":",
            print getattr(o,f)


if __name__=='__main__':
    cat(a)
    cat(b)
    cat(c)
