import numpy as np
from pprint import pprint

mSize = 2

def generate_test_matrix(mSize,plot=True):
    eigenValues=np.exp(np.random.rand(mSize)*2*np.pi*1.j)*np.exp(4*np.random.rand(mSize)**2-2)
    eigenValues = eigenValues[np.argsort(np.abs(eigenValues))]
    eigenVectorsR = np.random.rand(mSize,mSize)*np.exp(np.random.rand(mSize,mSize)*2*np.pi*1.j)
    eigenVectorsR /= np.sum(eigenVectorsR*np.conj(eigenVectorsR),axis=1)[:,np.newaxis]
    eigenVectorsL = np.linalg.inv(eigenVectorsR)
    testInput=np.matmul(eigenVectorsR,eigenValues[:,np.newaxis]*eigenVectorsL)
    
    #test that random eigenvectors are not parallel
    assert np.max(np.abs(np.matmul(eigenVectorsL,eigenVectorsR)-np.eye(mSize)))<10**11
    #test that right eigenvectors behave as intended
    assert np.max(np.abs(np.matmul(testInput,eigenVectorsR)/eigenValues[np.newaxis]-eigenVectorsR))<10**-7
    #test that lapack agrees with our construction
    egvc,_ = np.linalg.eig(testInput)
    egvcS=egvc[np.argsort(np.abs(egvc))]
    assert np.max(np.abs(egvcS-eigenValues))<10**-12
    return testInput,eigenValues

A,eigenValues=generate_test_matrix(mSize,False)

print(A)
print()
print(A.T@A)
print(A@A.T-A.T@A)

A=np.array([[ 0, 1],
            [-1, 0]])

pprint(A)
print(A.T@A)
print(A@A.T-A.T@A)
print(np.linalg.eig(A).eigenvalues)
print(np.linalg.eig(A).eigenvectors)

v=np.array([1,1])
print(v, A@v, A@A@v,A@A@A@v, A@A@A@A@v)



print(np.linalg.qr(A))
