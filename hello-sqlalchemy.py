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


from sqlalchemy.orm import aliased
user_alias = aliased(User, name='user_alias')
for row in session.query(user_alias, user_alias.name.label('name_label')).all():
    print row.user_alias, row.name_label


for u in session.query(User).order_by(User.id)[0:6]:
    print u

for u in session.query(User.name).filter_by(name='john'):
    print u

for u in session.query(User.name).filter(User.name=='john'):
    print u

for u in session.query(User).filter(User.name=='ed').filter(User.fullname=='Ed Jones'):
    print u

q=session.query(User)
eq=q.filter(User.name == 'ed')

weq=q.filter(or_(User.name == 'ed', User.name == 'wendy'))


for u in weq: 
    print u

try:
    weq.one()
except Exception as e:
    print e

for u in session.query(User).filter("id<3").order_by('id').all():
    print u, u.id

session.query(User).from_statement("SELECT * FROM users where name=:name").params(name='ed').all()

session.query(User).filter(User.name.like('%e%')).all()


from sqlalchemy.orm import relation, backref

class Address(Base):
    __tablename__='addresses'
    id = Column(Integer, primary_key=True)
    email_address = Column(String, nullable=False)
    user_id = Column(Integer, ForeignKey('users.id'))

    user = relation(User, backref=backref('addresses', order_by=id))

    def __init__(self, email_address):
        self.email_address = email_address

    def __repr__(self):
        return "<Address('%s')>" % self.email_address


jack = User('jack', 'Jack Bean', 'gqohfqop')

jack.addresses = [Address(email_address='jack@google.com'), Address(email_address='j25@yahoo.com')]

print jack.addresses
print jack.addresses[1]
print jack.addresses[1].user


session.query(User).all()

session.add(jack)
session.dirty
session.new

session.query(User).all()
