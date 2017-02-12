
n=30 ## samples
set.seed(123)
x = c(rnorm(n/2, 10, 2), rnorm(n/2, 0, 2))
y = as.factor(c(rep(0, 20), rep(1, 10)))
y


#library(e1071)
#nb = naiveBayes(x, y, laplace = 0)
#nb

#nb_predictions = predict(nb, x[1], type='raw')
#nb_predictions



library(dplyr)

nbc <- function(x, y){
  df <- as.data.frame(cbind(x,y))
  a_priori <- table(y) #/length(y)
  
  cond_probs <- df %>% group_by(y) %>% summarise(means = mean(x), var = sd(x))
  
  print("A Priori Probabilities")
  print(a_priori/sum(a_priori))
  
  print("conditional probabilities \n")
  print(cond_probs)
  
  return(list(apriori = a_priori, tables = cond_probs))
}




fit = nbc(x,y)

fit

predict_nbc <- function(model, new_x){
  apriori = as.matrix(model$apriori)
  a = log(apriori/sum(apriori))
  msd = as.matrix(model$tables)[,c(2,3)] ## creates 3 columsn; first is junk
  probs = sapply(new_x, function(v) dnorm(x = v, mean = msd[,1], sd = msd[,2]))
  b = log(probs)
  #L = a + b ## works for 1 new obs
  L = apply(X = b, MARGIN = 2, FUN = function(v) a + v)

  results <- apply(X = L, MARGIN = 2, function(x){
                   sapply(x, function(lp){ 1/sum(exp(x - lp)) }) ## numerically stable
  })
  return(results)
}


myres = predict_nbc(fit, new_x = x[1:4])
myres







