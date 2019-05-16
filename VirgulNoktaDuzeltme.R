dosyalar = c("D:/Desktop/FIT4/Bitirme Projesi/ciktilar/CiftciSayisi.csv", "D:/Desktop/FIT4/Bitirme Projesi/ciktilar/UretimAlani.csv", "D:/Desktop/FIT4/Bitirme Projesi/ciktilar/UretimMiktari.csv")

for(dosya in dosyalar){
  file = read.csv(dosya,stringsAsFactors = FALSE)
  names(file)=c(2002:2017)
  for(j in c(1:length(file[1,]))){
    i=1
    for(f in as.vector(file[,j])){
      if(is.na(f)){}
      else if(grepl("-",f)){
        file[i,j] = 0
      }
      else{
        file[i,j] = as.numeric(gsub(",", "\\.", gsub("\\.", "", gsub(" ", "", as.character(f)))))
      }
      i = i+1
    }
  }
  name=paste(substr(dosya,1,nchar(dosya)-4),as.character(2),sep="")
  name=paste(name,".csv",sep="")
  print(name)
  write.csv(file, name, row.names = F)
}