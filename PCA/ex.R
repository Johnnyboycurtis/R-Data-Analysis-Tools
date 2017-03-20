## this is example 8.1 from applied multivariate statistical analysis


S = matrix(data=c(1, -2, 0,
                  -2, 5, 0,
                  0, 0, 2), ncol = 3, byrow=TRUE)


cv = cov(S) ## covariance matrix


## using eigen function
print("using eigen function")
Eigenvalues = eigen(cv) ## eigen values and vecotrs

evals = Eigenvalues$values ## eigen values

evec = Eigenvalues$vectors ## eigen vectors

print("eigen values and vectors")
print(Eigenvalues)



print("using SVD: X = U D V'")
egv = svd(S)

print("eigen values (D)")
print(egv$d)

print("eigen vectors (U)")
print(egv$u)



## another example using the iris data set
s = iris[,-5]
print("iris data (first four columns")
head(s)



print("run PCA using princomp with cor=FALSE")
ex = princomp(s)
print(ex)
print(summary(ex))


## what does princomp do?
## first, it computes z matrix which is complete data matrix (no nulls)
## then, assumming covmat param is null, calculates dn = dim(z); nxm matrix
## then, covmat = cov.wt(z) which has cov matrix, center vals and n.obs
## then, cv <- covmat$cov * (1 - 1/n.obs)# for S-PLUS compatibility
## then, edc = eigen(cv, symmetric = TRUE) since cov matrix is symmetric
## then, ev = edc$values eigven values
## then, it checks to see if any eigen values are negative, this is a problem to handle since eigen values are supposed to represent variance and variance can't be negative!
## then, scales the data, if cor=FALSE, then it uses a mx1 vector of 1s -> sc
## however, if cor=TRUE, then it uses the diagonals of the cor matrix; mx1 vector
## in v = scale(z, center, scale = sc)
## the,  PCA scores are computed by v %*% edc$vector (eigen vectors)
## then returns standard dev = sqrt(eigenvals), 
##              loadings = eigen vectors,
##              centers,
##              and scores in a list
## end!


## now by hand
print("PCA using SVD")
c = cov(s)


print("using SVD: X = U D V'")
egv = svd(c)

print("eigen values (D)")
print(sqrt(egv$d))

print("eigen vectors (U)")
print(egv$u)










