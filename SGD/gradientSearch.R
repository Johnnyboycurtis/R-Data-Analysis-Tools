## https://spin.atomicobject.com/2014/06/24/gradient-descent-linear-regression/

#set.seed(123)


plot(x,y, type = "p", col = "blue",
     main = "x and y points", xlab = "x", ylab = "y")



computeError <- function(b, m, x, y){
    totalError = 0
    n = length(points)
    for(i in 1:n){
        totalError = totalError + (y[i] - (m * x[i] + b))^2
    }

    return(totalError/n)
}


stepGradient <- function(b_current, m_current, x, y, learningRate){
    b_gradient = 0
    m_gradient = 0
    N = length(points)
    for(i in 1:N){
        b_gradient = b_gradient + (2/N) * (y[i] - ((m_current * x[i]) + b_current)) * runif(1)
        m_gradient = m_gradient + -(2/N) * (y[i] - ((m_current * x[i]) + b_current)) * runif(1)
    }
    new_b = b_current - (learningRate * b_gradient)
    new_m = m_current - (learningRate + m_gradient)
    params = matrix(c(new_b, new_m), ncol = 2, dimnames = list(NULL, c("b", "m")))
    return(params)
}



# gradient_descent_ex <- function(x, y, initial_b, initial_m, learningRate, iterations = 1000){
#     b = initial_b
#     m = initial_m
#     for(i in 1:2){
#         gradientParams = stepGradient(b, m, x, y, learningRate)
#         print(gradientParams)
#         b = gradientParams[,'b']
#         m = gradientParams[,'m']
#     }
#     return(gradientParams)
# }



run <- function(initial_b, initial_m, learningRate, trueb=1, truem = 1.5){
    x = seq(from=0, to=20, by=0.5)

    y = truem*x + trueb #+ rnorm(length(x))
    
    b = initial_b
    m = initial_m
    for(i in 1:10){
      results = stepGradient(b, m, x, y, learningRate)
      print(results)
      b = results[,'b'] 
      m = results[,'m'] 
    }
    
    #plot(x,y, type = "o", col = "blue",   
    #     main = "x and y points", xlab = "x", ylab = "y")

    fity = results[,'m']*x + results[,'b']
    #lines(x, fity, col = "red")
    print(paste("b = ", trueb))
    print(paste("m = ", truem))
    return(results)
}

run(initial_b = 1, initial_m = 2, learningRate = 0.005)
 


run(initial_b = 7, initial_m = 6, trueb = 5, truem = -5, learningRate = 0.005)

