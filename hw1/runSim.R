## parsing command arguments

for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

## MSE calculation
mseCalc = function (x,trueM) {
  
mse = 0

  for(m in x)
  {
    mse = (m-trueM)^2 + mse
  }
  return(mse/length(x))
}


# simulate data

#set seed
set.seed(seed)

#create mean vectors
primeMean= numeric(rep)
sampleMean = numeric(rep)

#loop through reps
for (i in 1:rep){

#set distribution
if(dist == "gaussian"){
x = rnorm(n)
} else if(dist =="t1"){
  x = rt(n,1)
} else if(dist =="t5"){
  x = rt(n,5)
} 

# estimate means
primeMean[i]=estMeanPrimes(x)
sampleMean[i]=mean(x)

}


print(mseCalc(primeMean,0))
print(mseCalc(sampleMean,0))