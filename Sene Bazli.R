# DATA <- as.data.frame(rep(as.data.frame(rep(NA,81)),17))


# names(DATA) = list("Iller", 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)

for(sene in c(2017:2002)){
  name = paste("D:/Desktop/ciktilar/", sene, sep="")
  name = paste(name, "_tarımsal.csv", sep="")
  file = read.csv(name)
  
  if(sene == 2017){
    DATA1 = data.frame(file$Ciftci.Sayisi)
    DATA2 = data.frame(file$Uretim.Alani.ha.)
    DATA3 = data.frame(file$Uretim.Miktari.ton.)
  }
  else{
    DATA1 = data.frame(file$Ciftci.Sayisi, DATA1)
    DATA2 = data.frame(file$Uretim.Alani.ha., DATA2)
    DATA3 = data.frame(file$Uretim.Miktari.ton., DATA3)
  }
}
names(DATA1) = list(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
names(DATA2) = list(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
names(DATA3) = list(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
DATA1
DATA2
DATA3
write.csv(DATA1, "D:/Desktop/ciktilar/CiftciSayisi.csv", row.names = F)
write.csv(DATA2, "D:/Desktop/ciktilar/UretimAlani.csv", row.names = F)
write.csv(DATA3, "D:/Desktop/ciktilar/UretimMiktari.csv", row.names = F)

read.csv("D:/Desktop/ciktilar/UretimMiktari.csv")
