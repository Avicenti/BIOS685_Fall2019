# autoSim.R

nVals = seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")

for (n in nVals) {
  for(t in distTypes){
  oFile = paste("n", n,t,".txt", sep="")
  arg = paste("n=", n, " seed=280 ","rep=50 ", "dist=",t, sep="")
  sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile)
  system(sysCall)
  print(paste("sysCall=", sysCall, sep=""))
  }
}


