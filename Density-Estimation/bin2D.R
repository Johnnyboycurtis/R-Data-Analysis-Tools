## bivariate density estimation

x = rnorm(1000)
y = rnorm(1000)

mydata = matrix(cbind(x,y), ncol = 2)


bin2D <- function(x, breaks1 = "Sturges", breaks2 = "Sturges"){
    histg1 = hist(x[,1], breaks = breaks1, plot = FALSE)
    histg2 = hist(x[,2], breaks = breaks2, plot = FALSE)
    brx = histg1$breaks
    bry = histg2$breaks
    freq = table(cut(x[, 1], brx), cut(x[, 2], bry))

    return(list(call = match.call(),
                freq = freq,
                breaks1 = brx, breaks2 = bry,
                mids1 = histg1$mids, mids2 = histg2$mids))
}


fit = bin2D(mydata)
