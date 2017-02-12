
import matplotlib.pyplot as plt
import numpy as np
from scipy import stats

data = np.random.normal(1,2,(100,1))


def wGaussian(x):
    """Gaussian kernel density estimator"""
    w = stats.norm.pdf(x)
    return w


x = np.arange(-6, 6, 0.01)
m = len(x)
n = len(data)

h = 0.1

fhat = np.zeros(m)

for i in range(m):
    S = 0
    for j in range(n):
        S = S + (1.0/h) * wGaussian((data[j] - x[i]) / h)
    
    fhat[i] = (1.0/n) * S


plt.plot(x, fhat)
plt.show()


#plot(x = x, y = fhat, type = "s", xlab = "Eruption length",
#             ylab = "Density Estimate", main = "Gaussian Density Estimator")

#lines(density(faithful$eruptions), col = "blue")
#lines(density(faithful$eruptions, kernel="biweight"), col = "red")
