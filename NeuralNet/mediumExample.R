## example taken from 
## https://medium.com/technology-invention-and-more/how-to-build-a-simple-neural-network-in-9-lines-of-python-code-cc8f23647ca1#.pv7v2tstp



trainingSet = matrix(c(0,0,1, ## three inputs per row
                       1,1,1,
                       1,0,1,
                       0,1,1), ncol = 3, byrow=TRUE)


trainingOut = matrix(c(0, 1, 1, 0), ncol=1, byrow=TRUE) ## output per row; 4 x 1 matrix

set.seed(123)

synapticWeights = matrix(data = 2 * runif(3) - 1, ## why -1?
                         ncol = 1, byrow = TRUE)

#logisticPDF = function(x, synapticWeights = synapticWeights) 1 / (1 + exp(- (x %*% synapticWeights)))

for( i in 1:10^4){
    output = 1 / (1 + exp(-(trainingSet %*% synapticWeights))) ## sigmoid function
    
    #Adjust weights by error * input * output*(1-output), where out*(1-out) is the derivative of the logistic/sigmoid function
    synapticWeights = synapticWeights +  t(trainingSet) %*% ( (trainingOut - output) * output * (1-output) )
}


testSet = c(1, 0, 0)

print(1 / (1 + exp(- ( testSet %*% synapticWeights ))))

