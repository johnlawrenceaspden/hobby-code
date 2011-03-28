#!/usr/bin/env python
from sqlalchemy import *
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, scoped_session

engine = create_engine('sqlite:///:memory:', echo=True)
Base = declarative_base(bind=engine)
Session = scoped_session(sessionmaker(engine))

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

Base.metadata.create_all()

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