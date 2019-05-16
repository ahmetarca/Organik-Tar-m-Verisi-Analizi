install.packages("readtext")
path="D:/Desktop/AhmetArca__/"
library("readtext")

dt = readtext(paste(path, "BIDE+ output 0.1-true.txt", sep = ""))
dt = unlist(strsplit(dt$text, "[\n]"))


IDler = vector(mode="list", length=6)
names(IDler) = c("1", "2", "3", "4", "5", "-1")
IDler[[1]] = "Baslangic"; IDler[[2]] = "Artis"; IDler[[3]] = "Azalis"; IDler[[4]] = "Yokluk"; IDler[[5]] = "Tekrar"; IDler[[6]] = "/"
IDler

dt2=""
for(line in dt){
  control = TRUE
  l = ""
  for(i in strsplit(line, " ")[[1]]){
    if(i == "#SUP:")
      control = FALSE
    if(control){
      l = paste(l, IDler[[i]], sep = " ")
    }else
      l = paste(l, i)
  }
  dt2 = paste(dt2, l, sep="\n")
}
write(dt2,paste(path,"deneme.txt",sep=""))

DT=read.csv(paste(path,"BIDE+ output 0.1.csv",sep=""))
DT$asd
