#!/usr/bin/env python
from sqlalchemy import *
from sqlalchemy.ext.declarative import declarative_base


import pdb
# Can use pdb.set_trace() to get a breakpoint

Base = declarative_base()

class User(Base):
    __tablename__ = 'users'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    fullname = Column(String)
    password = Column(String)

    def __init__(self, name, fullname, password):
        self.name = name
        self.fullname = fullname
        self.password = password
        
    def __repr__(self):
        return "<User('%s', '%s', '%s')>" % (self.name, self.fullname, self.password)


engine = create_engine('sqlite:///:memory:', echo=True)
Base.metadata.create_all(bind=engine)

from sqlalchemy.orm import sessionmaker, scoped_session

Session = scoped_session(sessionmaker(engine))
session = Session()

ed_user = User('ed', 'Ed Jones', 'edspassword')
session.add(ed_user)

our_user = session.query(User).filter_by(name='ed').first()


session.add_all([ User('wendy', 'Wendy Williams', 'foobar')])

session.query(User).all()

session.add_all([
    User(name=a, fullname=b, password=c) for a,b,c in
    [('mary','Mary Contrary','foobar'),
     ('fred', 'Fred Flintstone', 'blah')]
])

session.query(User).all()

ed_user.password = 'f8s7ccs'

session.flush()

for instance in session.query(User).order_by(User.id):
    print instance.name, instance.fullname

for name, fullname in session.query(User.name, User.fullname):
    print name, fullname


# Example of creating an object and committing it.
# notice that the id doesn't get created until the transaction commits
jla = User('john', 'John Lawrence Aspden', 'hello')
jla.id
session.add(jla)
jla.id
session.commit()
jla.id

# Search for me
session.query(User).filter_by(name='john').all()

jla2=session.query(User).filter_by(name='john').first()

# Change my password
jla2.password="joqpah4io"

print '================================================'
print jla.password
session.query(User).filter_by(name='john').all()

#rolling back a transaction
jla.name='Edwardo'
session.dirty
session.rollback
session.rollback()
session.dirty

session.query(User).all()

for u in session.query(User).all():
    print u.id, ":" , u.name

#order by name
for u in session.query(User).order_by(User.name).all():
    print u.id, ":" , u.name

#don't have to get the whole record
[ (x,y) for (x,y) in session.query(User.name, User.fullname) ]

#what on earth is going on here?
for row in session.query(User, User.name).all():
    print row.User, row.name


