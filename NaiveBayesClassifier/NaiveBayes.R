
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


nbc <- function(x, ...)
  UseMethod("nbc")


library(dplyr)

nbc <- function(x, y){
  call <- match.call()
  
  a_priori <- table(y) #/length(y)
  x <- as.data.frame(x)
  
  ## estimation-function for conditional probabilities
  cond_probs <- function(var)
    if (is.numeric(var)) {
      mu_sigma = cbind(
          tapply(var, y, mean, na.rm = TRUE),
          tapply(var, y, sd, na.rm = TRUE)
          )
      colnames(mu_sigma) <- c("mean", "sd")
      return(mu_sigma)
    } else {
      tab <- table(y, var)
      return(tab / rowSums(tab))
    }
  
  ## create tables
  cond_prob_tables <- lapply(X = x, FUN = cond_probs)  
  
  #return(list(apriori = a_priori, tables = cond_prob_tables))
  
  
  structure(list(apriori = a_priori,
                 tables = cond_prob_tables,
                 levels = levels(y),
                 call   = call ),
            class = "nbc" )
}



print.nbc <- function(x, ...) {
  cat("\nNaive Bayes Classifier for Discrete Predictors\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nA-priori probabilities:\n")
  print(x$apriori / sum(x$apriori))
  
  cat("\nConditional probabilities:\n")
  for (i in x$tables) {print(i); cat("\n")}
  
}






nbc.formula <- function(formula, data){
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

predict.nbc <- function(model, new_x){
  apriori = as.matrix(model$apriori) ## apriori counts
  cols = names(model$tables) ## list of names of cond probs
  msd = model$tables ## list of cond probs
  a = log(apriori/sum(apriori)) ## log apriori probs
  
  new_x = as.data.frame(new_x)
  
  densities = function(name){
    ## takes name of column to use
    dat = new_x[name] ## gets column from new data.frame
    params = msd[[name]]
    prob = sapply(dat, function(v) dnorm(x = v, mean = msd[[name]][,1], sd = msd[[name]][,2])) ## function to calculate densities
    return(prob)
  }
  
  result = sapply(X = cols, FUN = densities)
  print(result)
  #b = log(probs)

  #L = apply(X = b, MARGIN = 2, FUN = function(v) a + v)

  #results <- apply(X = L, MARGIN = 2, function(x){
  #                 sapply(x, function(lp){ 1/sum(exp(x - lp)) }) ## numerically stable
  #})
  #return(results)
}


myres = predict_nbc(fit, new_x = x[1:4])
myres










data(mtcars)
df = mtcars
df$cyl <- as.factor(df$cyl)
df$vs <- as.factor(df$vs)  
df$am <- as.factor(df$am)
df$gear <- as.factor(df$gear)
df$carb <- as.factor(df$carb)
str(df)


## fitting the model
fit = nbc(x = df[, colnames(df) != "am"], y = df$am)
fit


predict(fit, new_x = df[, colnames(df) != "am"])

