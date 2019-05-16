dosyalar = c("D:/Desktop/FIT4/Bitirme Projesi/ciktilar/CiftciSayisi.csv", "D:/Desktop/FIT4/Bitirme Projesi/ciktilar/UretimAlani.csv", "D:/Desktop/FIT4/Bitirme Projesi/ciktilar/UretimMiktari.csv")
CiftciSayisi=data.frame()
UretimAlani=data.frame()
UretimMiktari=data.frame()
Sum=data.frame()
Mean=data.frame()
toplamlar=c()
ortalamalar=c()
for(dosya in dosyalar){
  file = read.csv(dosya)
  if (length(CiftciSayisi)==0)
    CiftciSayisi=file
  else if (length(UretimAlani)==0)
    UretimAlani=file
  else if (length(UretimMiktari)==0)
    UretimMiktari=file
    
  toplamlar=c()
  ortalamalar=c()
  for(i in c(1:16)){
    toplam=0
    count = 0
    for(j in file[,i]){
      if(!is.na(j)){
        toplam = toplam + j
        if (j!= 0)
          count = count + 1
      }
    }
    toplamlar=c(toplamlar,toplam)
    ortalamalar=c(ortalamalar,toplam/count)
  }
  if(length(Sum)==0){
    Sum=as.data.frame(toplamlar)
    Mean=as.data.frame(ortalamalar)
  }else{
    Sum=data.frame(Sum,toplamlar)
    Mean=data.frame(Mean,ortalamalar)
  }
}

names(Sum) = list("Ciftci Sayisi", "Uretim Alani", "Uretim Miktari")
names(Mean) = list("Ciftci Sayisi", "Uretim Alani", "Uretim Miktari")
Sum
Mean

CiftciSayisi
UretimAlani
UretimMiktari

par(yaxs="i",las=1)
hist(Sum$`Ciftci Sayisi`,
     prob=TRUE,col="black",border="white",
     xlab="Toplam Ciftci Sayisi",
     main="Toplam Ciftci Sayisi Dagilimi")
box(bty="l")
lines(density(Sum$`Ciftci Sayisi`,na.rm=T),col="red",lwd=4)
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
legend(locator(1), legend=c("sum"), col=c("red"), lty=1, cex=0.8)


par(yaxs="i",las=1)
hist(Sum$`Uretim Alani`,
     prob=TRUE,col="black",border="white",
     xlab="Toplam Uretim Alani",
     main="Toplam Uretim Alani Dagilimi")
box(bty="l")
lines(density(Sum$`Uretim Alani`,na.rm=T),col="red",lwd=4)
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
legend(locator(1), legend=c("sum"), col=c("red"), lty=1, cex=0.8)



par(yaxs="i",las=1)
hist(Sum$`Uretim Miktari`,
     prob=TRUE,col="black",border="white",
     xlab="Toplam Uretim Miktari",
     main="Toplam Uretim Miktari Dagilimi")
box(bty="l")
lines(density(Sum$`Uretim Miktari`,na.rm=T),col="red",lwd=4)
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
legend(locator(1), legend=c("sum"), col=c("red"), lty=1, cex=0.8)

par(yaxs="i",las=1)
box(bty="l")
plot(density(Mean$`Ciftci Sayisi`,na.rm=T),col="blue",lwd=4,xlab="Ortalama Ciftci Sayisi",main="Ortalama Ciftci Sayisi Dagilimi")
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
#legend(locator(1), legend=c("mean"), col=c("blue"), lty=1, cex=0.8)


par(yaxs="i",las=1)
box(bty="l")
plot(density(Mean$`Uretim Alani`,na.rm=T),col="blue",lwd=4,xlab="Ortalama Uretim Alani",main="Ortalama Uretim Alani Dagilimi")
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
#legend(locator(1), legend=c("mean"), col=c("blue"), lty=1, cex=0.8)


par(yaxs="i",las=1)
box(bty="l")
plot(density(Mean$`Uretim Miktari`,na.rm=T),col="blue",lwd=4,xlab="Ortalama Uretim Miktari",main="Ortalama Uretim Miktari Dagilimi")
grid(nx=NA,ny=NULL,lty=1,lwd=1,col="gray")
#legend(locator(1), legend=c("mean"), col=c("blue"), lty=1, cex=0.8)


boxplot(Sum$`Uretim Alani`, notch=TRUE, col=(c("gold","darkgreen")))

asd = read.csv("D:/Desktop/FIT4/Bitirme Projesi/ciktilar/CiftciSayisi.csv")
asd=data.frame()
for(i in Sum$`Ciftci Sayisi`){
  if(length(asd)==0)
    asd = data.frame(i)
  else
    asd = data.frame(asd,i)
}
names(asd)=c(2002:2017)
par(mfrow =c(1,2))
boxplot(asd$`2002`[which(asd$'2002'>0)], notch=TRUE, 
        col=(c("gold","darkgreen")))

boxplot(asd$`2002`, notch=TRUE, 
        col=(c("gold","darkgreen")))

par(mfrow =c(1,2))
boxplot(asd$`2017`[which(asd$'2002'>0)], notch=TRUE, 
        col=(c("gold","darkgreen")))

boxplot(c(2,2,2,2,1), notch=TRUE, 
        col=(c("gold","darkgreen")))
boxplot(c(rep(NA,16),2,2,2,2,1), notch=TRUE, 
        col=(c("gold","darkgreen")))