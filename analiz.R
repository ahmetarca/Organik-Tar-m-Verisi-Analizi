path="D:/Desktop/AhmetArca__/"

dt=read.csv(paste(path,"CiftciSayisi_clean.csv",sep=""))
dt2=read.csv(paste(path,"UretimMiktari_clean.csv",sep=""))
dt3=read.csv(paste(path,"UretimAlani_clean.csv",sep=""))

#pdf("boxplotlar3.pdf",15,10)
#par(mfcol=c(2,3))

ciftci=c()
yillar=c()

for ( i in 1:ncol(dt)){
  ciftci=append(ciftci,dt[,i])
  yillar=append(yillar,array(data = i+2001,dim=nrow(dt)))
}

r_ind=which(ciftci==0)
ciftci=ciftci[-r_ind]
yillar=yillar[-r_ind]

#boxplot(ciftci~yillar, main="Y?llara g?re ?iftci say?lar?", xlab="Y?llar", ylab="?ift?i Say?s?", ylim=c(0,1000))
#boxplot(ciftci~yillar, main="Y?llara g?re ?iftci say?lar?", xlab="Y?llar", ylab="?ift?i Say?s?")



miktar=c()
yillar=c()

for ( i in 1:ncol(dt2)){
  miktar=append(miktar,dt2[,i])
  yillar=append(yillar,array(data = i+2001,dim=nrow(dt2)))
}

r_ind=which(miktar==0)
miktar=miktar[-r_ind]
yillar=yillar[-r_ind]

#boxplot(miktar~yillar, main="Y?llara g?re ?retim Miktar?(ton)", xlab="Y?llar", ylab="Ton", ylim=c(0,22000))
#boxplot(miktar~yillar, main="Y?llara g?re ?retim Miktar?(ton)", xlab="Y?llar", ylab="Ton")




alan=c()
yillar=c()

for ( i in 1:ncol(dt3)){
  alan=append(alan,dt3[,i])
  yillar=append(yillar,array(data = i+2001,dim=nrow(dt3)))
}

r_ind=which(alan==0)
alan=alan[-r_ind]
yillar=yillar[-r_ind]

#boxplot(alan~yillar, main="Y?llara g?re ?retim Alan?(ha)", xlab="Y?llar", ylab="ha", ylim=c(0,7000))
#boxplot(alan~yillar, main="Y?llara g?re ?retim Alan?(ha)", xlab="Y?llar", ylab="ha")
#dev.off()

#pdf("ortalamalar.pdf",7,7)
#par(mfrow=c(2,2))

ort=unlist(lapply(1:ncol(dt),function(x) mean(dt[which(dt[,x]>0),x])))
#plot(y=colMeans(dt),x=c(2002:2017),type="o",col="red", main="Y?llara g?re ortalama ?iftci say?lar?", xlab="Y?llar", ylab="Ortalama ?ift?i Say?s?",ylim=c(0,700))
#lines(y=ort,x=c(2002:2017),type="o",col="blue")
#legend("topleft", legend=c("0'lar dahil", "0'lar olmadan"), col=c("red", "blue"), lty=1, cex=0.8)


ort2=unlist(lapply(1:ncol(dt2),function(x) mean(dt2[which(dt2[,x]>0),x])))
#plot(y=colMeans(dt2),x=c(2002:2017),type="o",col="red", main="Y?llara g?re ortalama ?retim Miktar?", xlab="Y?llar", ylab="Ton",ylim=c(0,22000))
#lines(y=ort2,x=c(2002:2017),type="o",col="blue")
#legend("topleft", legend=c("0'lar dahil", "0'lar olmadan"), col=c("red", "blue"), lty=1, cex=0.8)


ort3=unlist(lapply(1:ncol(dt3),function(x) mean(dt3[which(dt3[,x]>0),x])))
#plot(y=colMeans(dt3),x=c(2002:2017),type="o",col="red", main="Y?llara g?re ortalama ?retim Alan?", xlab="Y?llar", ylab="ha",ylim=c(0,7000))
#lines(y=ort3,x=c(2002:2017),type="o",col="blue")
#legend("topleft", legend=c("0'lar dahil", "0'lar olmadan"), col=c("red", "blue"), lty=1, cex=0.8)

#plot.new()
#dev.off()


sehirler=read.csv(paste(path,"sehirler.csv",sep=""))


pdf("sehirBazli.pdf",11,9)
par(mfrow=c(2,2))
for( i in 1:nrow(dt)){
  plot(y=dt[i,],x=c(2002:2017),type="o",col="red", main=paste(sehirler[i,2]," i?in Y?llara g?re ?iftci say?s?",sep=""), xlab="Y?llar", ylab="?ift?i Say?s?",ylim=c(0,max(dt[i,])))
  plot(y=dt2[i,],x=c(2002:2017),type="o",col="red", main=paste(sehirler[i,2]," i?in Y?llara g?re ?retim Miktar?",sep=""), xlab="Y?llar", ylab="ton",ylim=c(0,max(dt2[i,])))
  plot(y=dt3[i,],x=c(2002:2017),type="o",col="red", main=paste(sehirler[i,2]," i?in Y?llara g?re ?retim Alan?",sep=""), xlab="Y?llar", ylab="ha",ylim=c(0,max(dt3[i,])))
  plot.new()
}
dev.off()

sehirler = sehirler[,2]
sehirSayisi = length(sehirler)

renks=rainbow(sehirSayisi)
renks=colors()[1:(sehirSayisi+1)]

pdf("tumSehirlerBirlikte.pdf",20,20)
par(mfrow=c(2,2))
plot(y=dt[1,],x=c(2002:2017),type="o",col=renks[1], main="Yıllara Göre çiftci Sayısı", xlab="Yıllar", ylab="Çiftçi Sayısı",ylim=c(0,max(dt)))
for ( i in 2:sehirSayisi){
  lines(y=dt[i,],x=c(2002:2017),type="o",col=renks[i])
}
plot(y=dt2[1,],x=c(2002:2017),type="o",col=renks[1], main="Yıllara Göre Üretim Miktarı", xlab="Yıllar", ylab="ton",ylim=c(0,max(dt2)))
for ( i in 2:sehirSayisi){
  lines(y=dt2[i,],x=c(2002:2017),type="o",col=renks[i])
}
plot(y=dt3[1,],x=c(2002:2017),type="o",col=renks[1], main="Yıllara Göre Üretim Alanı", xlab="Yıllar", ylab="ha",ylim=c(0,max(dt3)))
for ( i in 2:sehirSayisi){
  lines(y=dt3[i,],x=c(2002:2017),type="o",col=renks[i])
}
plot.new()
legend("topleft", legend=sehirler[1:27], col=renks[1:27], lty=1, cex=1.75)
legend("top", legend=sehirler[28:54], col=renks[28:54], lty=1, cex=1.75)
legend("topright", legend=sehirler[55:81], col=renks[55:81], lty=1, cex=1.75)
dev.off()
sehirinRenkleri = data.frame(sehirler, substr(renks,1,7))

#data.frame(ort,ort3,ort2)