isim = "2017_tarımsal.csv"
file = read.csv2(paste("D:/Desktop/BiodiversiteVerileri/",isim,sep=""))
ilListesi = readLines("D:/Desktop/BiodiversiteVerileri/iller2.txt")              #2016-2017: 2
ilListesi2 = readLines("D:/Desktop/BiodiversiteVerileri/iller.txt")           #2016-2017

file = as.data.frame(file[(which(grepl("Toplam",file[,1]))),])
file = file[1:(length(file[,1])-1),]                                            #2002-2003: 3
# file
iller = as.data.frame(file[,1])

for (i in (1:length(iller[,1]))){
  iller[i,1] = strsplit(as.character(iller[i,1]), "Toplam ")[[1]][2]
}
# 2010dan sonra degisir
ciftciSayisi = as.data.frame(file[,3])
uretimAlani = as.data.frame(file[,4])
uretimMiktari = as.data.frame(file[,8])                                       #2010-2017: 8

f2002 = data.frame(iller, ciftciSayisi, uretimAlani, uretimMiktari)
names(f2002)= list("iller", "Ciftci Sayisi", "Uretim Alani(ha)", "Uretim Miktari(ton)")

# f2002

temp = data.frame(ilListesi, rep(NA, 81), rep(NA, 81), rep(NA, 81))
names(temp) = list("iller", "Ciftci Sayisi", "Uretim Alani(ha)", "Uretim Miktari(ton)")
# temp

for(i in c(1:length(f2002[,1]))){
  index=match(f2002[i,1], as.data.frame(temp)[,1])
  temp[index,2] = as.character(f2002[i,2])
  temp[index,3] = as.character(f2002[i,3])
  temp[index,4] = as.character(f2002[i,4])
}
# temp
temp$iller = ilListesi2                                                       # 2016-2017
write.csv(temp,paste("D:/Desktop/Ciktilar/",isim,sep=""), row.names = F)

read.csv(paste("D:/Desktop/Ciktilar/",isim,sep=""))
