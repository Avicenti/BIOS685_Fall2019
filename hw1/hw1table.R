#set nVals and distribution types to loop over
nVals = seq(100, 500, by=100)
distTypes = c("t1", "t5","gaussian")

#looping variable for output matrix
i=1
j=1

#create Output Matrix
tableOut = matrix(0,10,3)
colnames(tableOut) <- distTypes
rName = NULL

#loop through all the files and populate Output Matrix

for (n in nVals) {
  for(t in distTypes){
    
    oFile = paste("n", n,t,".txt", sep="")
    tableOut[i,j] = read.table(oFile)[1,2] 
    tableOut[i+1,j] = read.table(oFile)[2,2]
    j=j+1
  }
  rName = cbind(rName,paste(n," PrimeAverage"))
  rName = cbind(rName,paste(n," SampleAverage"))
  j=1
  i=i+2
}

rownames(tableOut)<- rName
print(tableOut)
