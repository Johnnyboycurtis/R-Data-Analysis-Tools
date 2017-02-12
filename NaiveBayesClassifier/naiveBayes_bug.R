naiveBayes <- function(x, ...)
  UseMethod("naiveBayes")

naiveBayes.default <- function(x, y, laplace = 0, ...) {
  call <- match.call()
  Yname <- deparse(substitute(y))
  x <- as.data.frame(x)
  
  ## estimation-function
  est <- function(var)
    if (is.numeric(var)) {
      cbind(tapply(var, y, mean, na.rm = TRUE),
            tapply(var, y, sd, na.rm = TRUE))
    } else {
      tab <- table(y, var)
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
    }
  
  ## create tables
  apriori <- table(y)
  tables <- lapply(x, est)
  
  ## fix dimname names
  for (i in 1:length(tables))
    names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
  names(dimnames(apriori)) <- Yname
  
  structure(list(apriori = apriori,
                 tables = tables,
                 levels = levels(y),
                 call   = call
  ),
  
  class = "naiveBayes"
  )
}


print.naiveBayes <- function(x, ...) {
  cat("\nNaive Bayes Classifier for Discrete Predictors\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nA-priori probabilities:\n")
  print(x$apriori / sum(x$apriori))
  
  cat("\nConditional probabilities:\n")
  for (i in x$tables) {print(i); cat("\n")}
  
}

predict.naiveBayes <- function(object,
                               newdata,
                               type = c("class", "raw"),
                               threshold = 0.001,
                               eps = 0,
                               ...) {
  type <- match.arg(type)
  newdata <- as.data.frame(newdata)
  attribs <- match(names(object$tables), names(newdata))
  isnumeric <- sapply(newdata, is.numeric)
  newdata <- data.matrix(newdata)
  L <- sapply(1:nrow(newdata), function(i) {
    ndata <- newdata[i, ]
    L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),
                                                function(v) {
                                                  nd <- ndata[attribs[v]]
                                                  if (is.na(nd)) rep(1, length(object$apriori)) else {
                                                    prob <- if (isnumeric[attribs[v]]) {
                                                      msd <- object$tables[[v]]
                                                      msd[, 2][msd[, 2] <= eps] <- threshold
                                                      dnorm(nd, msd[, 1], msd[, 2])
                                                    } else object$tables[[v]][, nd]
                                                    prob[prob <= eps] <- threshold
                                                    prob
                                                  }
                                                })), 1, sum)
    print("apply(log(sapply(...dnorm calc...))) where I suspect there is a bug: ")
    print(apply(log(sapply(seq_along(attribs),
                           function(v) {
                             nd <- ndata[attribs[v]]
                             if (is.na(nd)) rep(1, length(object$apriori)) else {
                               prob <- if (isnumeric[attribs[v]]) {
                                 msd <- object$tables[[v]]
                                 msd[, 2][msd[, 2] <= eps] <- threshold
                                 dnorm(nd, msd[, 1], msd[, 2])
                               } else object$tables[[v]][, nd]
                               prob[prob <= eps] <- threshold
                               prob
                             }
                           })), 1, sum))
    if (type == "class")
      L
    else {
      ## Numerically unstable:
      ##            L <- exp(L)
      ##            L / sum(L)
      ## instead, we use:
      sapply(L, function(lp) {
        1/sum(exp(L - lp))
      })
    }
  })
  if (type == "class")
    factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
  else t(L)
}







#########
# Example
#########


n=30 ## samples
set.seed(123)
x = c(rnorm(n/2, 10, 2), rnorm(n/2, 0, 2))
y = as.factor(c(rep(0, 20), rep(1, 10)))



nb = naiveBayes(x, y, laplace = 0)
nb

nb_predictions = predict(nb, x[1], type='raw')
nb_predictions

