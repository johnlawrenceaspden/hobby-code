def glplot(x):
    import glplot
    import Numeric
    glplot.glplot(Numeric.array(x))


x=range(100)
glplot(x)
