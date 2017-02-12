## functions for density estimation



## basically a histogram...
my.w <- function(x){
    if(abs(x) < 1){
        w <- 1/2
    }else{
        w <- 0
    }
    return(w)
}



## below is an example using the faithful eruptions data
data(faithful)

x <- seq(from=0, to=6, by=0.2)
m = length(x)
n = length(faithful$eruptions)
h = 0.2
fhat <- numeric(m) ## just zeros m times


for(i in 1:m){
    S <- 0
    for(j in 1:n){
        S <- S + (1/h) * my.w((faithful$eruptions[j] - x[i])/h)
    }
    fhat[i] <- (1/n) * S
}



plot(x = x, y = fhat, type = "s", xlab = "Eruption length",
     ylab = "Density Estimate", main = "Naive Density Estimator")

lines(density(faithful$eruptions), col = "blue")
lines(density(faithful$eruptions, kernel="biweight"), col = "red")




## gaussian kernel density estimator
my.w.Gaussian = function(x){
    w = dnorm(x)
    return(w)
}



x <- seq(from=0, to=6, by=0.01)
m = length(x)
n = length(faithful$eruptions)
h = 0.1
fhat <- numeric(m) ## just zeros m times


for(i in 1:m){
    S <- 0
    for(j in 1:n){
        S <- S + (1/h) * my.w.Gaussian((faithful$eruptions[j] - x[i])/h)
    }
    fhat[i] <- (1/n) * S
}




plot(x = x, y = fhat, type = "s", xlab = "Eruption length",
     ylab = "Density Estimate", main = "Gaussian Density Estimator")

lines(density(faithful$eruptions), col = "blue")
lines(density(faithful$eruptions, kernel="biweight"), col = "red")
