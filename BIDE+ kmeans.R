path = "D:/Desktop/AhmetArca__/"
dt = read.csv(paste(path, "BIDE+ output 0.1.csv", sep = ""))

asd = dt$asd
N = length(asd)
SUP = dt$SUP
SID = dt$SID
sehirler = (1:81)
matris = (matrix(nrow = N, ncol = 81))
for(i in (1:N)){
  for(sehir in sehirler){
    if(as.character((sehir-1)) %in% strsplit(as.vector(SID[i]), " ")[[1]])
      matris[i, sehir] = 1
    else
      matris[i, sehir] = 0
  }
}

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 20.
k.max <- 20
data <- matris
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
pdf(paste(path,"Kmeans elbow.pdf", sep = ""),10,10)
plot(1:k.max, wss,
     type="b", pch = 19, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
axis(1, at=1:20, 1:20)
dev.off()


write.table(matris, paste(path, "4339 X 81 binary matris.csv",sep = ""),sep = ",", col.names = T, row.names = F)
read.csv(paste(path, "4339 X 81 binary matris.csv",sep = ""))

library (cluster)
library (vegan)

dis = vegdist(matris)
pdf(paste(path,"Kmeans silhouette 8.pdf", sep = ""),10,10)
res = kmeans(dis,8)
sil = silhouette (res$cluster,dis) # or use your cluster vector
windows() # RStudio sometimes does not display silhouette plots correctly
plot(sil)
dev.off()

euclidean_dist = dist(matris,method = "euclidean")
manhattan_dist = dist(matris,method = "manhattan")
binary_dist = dist(matris,method = "binary")
minkowski_dist = dist(matris,method = "minkowski")

km8 = kmeans(matris,8)
km9 = kmeans(matris,9)

pdf(paste(path,"Kmeans/silhouette.pdf", sep = ""),15,8)
par(mfrow =c(1,2))
plot(silhouette (km8$cluster,euclidean_dist), col = (1:8), border = NA)
plot(silhouette (km9$cluster,euclidean_dist), col = (1:9), border = NA)
plot(silhouette (km8$cluster,manhattan_dist), col = (1:8), border = NA)
plot(silhouette (km9$cluster,manhattan_dist), col = (1:9), border = NA)
plot(silhouette (km8$cluster,binary_dist), col = (1:8), border = NA)
plot(silhouette (km9$cluster,binary_dist), col = (1:9), border = NA)
plot(silhouette (km8$cluster,minkowski_dist), col = (1:8), border = NA)
plot(silhouette (km9$cluster,minkowski_dist), col = (1:9), border = NA)
dev.off()


pdf(paste(path,"Hierarchical/silhouette.pdf", sep = ""),7.5,8)
plot(silhouette (h6,euclidean_dist), col = (1:6), border = NA)
dev.off()


# vary parameters for most readable graph
library(cluster)
library(fpc)
par(mfrow =c(2,2))
clusplot(matris, km8$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions

plotcluster(matris, km8$cluster)

clusplot(matris, km9$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(matris, km9$cluster)



h6 = cutree(hclust(euclidean_dist, method = "ward"), k = 6)
hc=hclust(euclidean_dist, method = "ward")
plot(hc, hang = -1, cex = 0.6)
rect.hclust(hc, h=6, border="red")

par(mfrow =c(1,2))
clusplot(matris, h6, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(matris, h6)

# Ward Hierarchical Clustering
pdf(paste(path,"Hierarchical/a.pdf", sep = ""))
plot(hclust(euclidean_dist, method = "ward")) # display dendogram cutree-6
plot(hclust(manhattan_dist, method = "ward"))           #6
plot(hclust(binary_dist, method = "ward"))        #6
plot(hclust(minkowski_dist, method = "ward"))

groups = cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

################################# model based #################################

library(mclust)
fit <- Mclust(matris)
par(mfrow =c(2,2))
plot(fit) # plot results
summary(fit) # display the best model


################################# DBSCAN ######################################

set.seed(123)
df=data.frame(c(1,0,1),c(0,0,1))
names(df)=c("a","b","c")
db <- fpc::dbscan(df, eps = 0.15, MinPts = 8)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)



sonoutput = read.csv(paste(path,"BIDE+ output 0.1.csv",sep = ""))
SID = sonoutput$SID

DBS = c()
for(line in SID){
  l = ""
  for(i in strsplit(line, " ")[[1]]){
    if(l != "")
      l = paste(l, (as.integer(i)+1), sep = " ")
    else
      l = as.character(as.integer(i)+1)
  }
  if(length(DBS) != 0)
    DBS = c(DBS, l)
  else
    DBS = c(l)
}

SID = DBS

SO = data.frame(sonoutput$asd, sonoutput$SUP, SID, km8$cluster, km9$cluster, h6)
names(SO)=c("asd","SUP", "SID", "Kmeans8", "Kmeans9", "Hclust")
write.csv(SO, paste(path, "BIDE+ output with clusters.csv", sep = ""), row.names = F)

