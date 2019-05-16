path="D:/Desktop/AhmetArca__/"


dt = read.csv(paste(path,"UretimMiktari_clean.csv",sep=""))

lines =c()

for(i in c(1:81)){
  line = c()
  for(j in c(1:16)){
    if(j == 1){
      line = c(line,1)
      if(dt[i,j]==0)
        line = c(line,4)
      line = c(line,-1)
      next()
    }
    
    if(dt[i,j]>dt[i,j-1])
      line = c(line,2)
    if(dt[i,j]<dt[i,j-1])
      line = c(line,3)
    if(dt[i,j]==0)
      line = c(line,4)
    if(dt[i,j]==dt[i,j-1]){
      line = c(line,5)
      #print(dt[i,j])
      #print(dt[i,j-1])
    }
    line = c(line,-1)
  }
  line = c(line,-2)
  lines = c(lines, line)
}
write(lines,paste(path,"BIDE+ input.txt",sep=""),sep=" ")
