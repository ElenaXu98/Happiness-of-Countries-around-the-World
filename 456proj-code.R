library(MVA)
library(dplyr)
library(tidyr)
library(class)
library(MASS)
library(ggdendro)
library(ape)

################################################    data convertion   ################################################################
whr <- read.table(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/Chapter2OnlineData-2019.csv"),sep = ",",header = TRUE)

whr12.18 <- whr %>% group_by(Country.name) %>% summarise(mean(Log.GDP.per.capita[which(Year>=2012)]),mean(Social.support[which(Year>=2012)])
                                                         ,mean(Healthy.life.expectancy.at.birth[which(Year>=2012)])
                                                         ,mean(Freedom.to.make.life.choices[which(Year>=2012)])
                                                         ,mean(Generosity[which(Year>=2012)])
                                                         ,mean(Perceptions.of.corruption[which(Year>=2012)]))
colnames(whr12.18) <- c("Country.name","Log.GDP.per.capita","Social.support","Healthy.life.expectancy.at.birth", "Freedom.to.make.life.choices","Generosity","Perceptions.of.corruption" )
##check for NA values
na.index <- c()
for (i in 2:dim(whr12.18)[2]) {
  cat(sep = " ",colnames(whr12.18)[i],"has",sum(is.na(whr12.18[,i])),"NA values","\n")
  cat(sep = " ","index are", which(is.na(whr12.18[,i])),"\n")
  #  cat(sep = " ",colnames(whr12.18)[i],"has",sum(is.nan(whr12.18[,i])),"NaN values","\n")
  #  cat(sep = " ","index are", which(is.nan(as.matrix(whr12.18[,i]))),"\n")
  cat(sep = " ","\n")
  na.index <- c(na.index,which(is.na(whr12.18[,i])))
}
(na.index <- unique(na.index))
length(na.index)
CA.data12.18 <- whr12.18[-c(na.index),]
sum(is.na(CA.data12.18))  #balanced data!!!

whr05.11 <- whr %>% group_by(Country.name) %>% summarise(mean(Log.GDP.per.capita[which(Year<2012)])
                                                         ,mean(Social.support[which(Year<2012)])
                                                         ,mean(Healthy.life.expectancy.at.birth[which(Year<2012)])
                                                         ,mean(Freedom.to.make.life.choices[which(Year<2012)])
                                                         ,mean(Generosity[which(Year<2012)])
                                                         ,mean(Perceptions.of.corruption[which(Year<2012)]))
colnames(whr05.11) <- c("Country.name","Log.GDP.per.capita","Social.support","Healthy.life.expectancy.at.birth", "Freedom.to.make.life.choices","Generosity","Perceptions.of.corruption" )
na.index <- c()
for (i in 2:dim(whr05.11)[2]) {
  cat(sep = " ",colnames(whr05.11)[i],"has",sum(is.na(whr05.11[,i])),"NA values","\n")
  cat(sep = " ","index are", which(is.na(whr05.11[,i])),"\n")
  #  cat(sep = " ",colnames(whr12.18)[i],"has",sum(is.nan(whr12.18[,i])),"NaN values","\n")
  #  cat(sep = " ","index are", which(is.nan(as.matrix(whr12.18[,i]))),"\n")
  cat(sep = " ","\n")
  na.index <- c(na.index,which(is.na(whr05.11[,i])))
}
(na.index <- unique(na.index))
length(na.index)
CA.data05.11 <- whr05.11[-c(na.index),]
sum(is.na(CA.data05.11))  #balanced data!!!

#############################################  data visulization  ###############################################################
###scatterplot matrix between 7 variables
panel.box <- function(x){
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(usr[1:2],0,1.5),new=T)
  s <- boxplot(x)
}

upper.panel <- function(x,y){
  data <- data.frame(cbind(x,y))
  par(new=T)
  den <- bvbox(data)
}
lower.panel <- function(x,y){
  data <- data.frame(cbind(x,y))
  par(new=T)
  den <- plot(x,y,pch=16)
}
pairs(CA.data12.18[,2:7],
      diag.panel = panel.box,
      upper.panel = upper.panel, lower.panel= lower.panel)

cor <- cor(CA.data12.18[,c(2:7)])
#life.ladder and logGDP, social support, happy life expectancy birth are positively linearly related.

plot(CA.data12.18$Perceptions.of.corruption,ylab = "perception of corruption")
text(x=c(38,33,88,83,109),y=CA.data12.18$Perceptions.of.corruption[c(38,33,88,83,109)],labels = c("Finland","Denmark","Norway","Netherlands","Switzerland"),col="red")
which(CA.data12.18$Country.name=="Finland")
which(CA.data12.18$Country.name=="Denmark")
which(CA.data12.18$Country.name=="Norway")
which(CA.data12.18$Country.name=="Netherlands")
which(CA.data12.18$Country.name=="Switzerland")
## outliers: Australia, Canada, Hong Kong, Ireland, SAR of China,
## Netherlands, Norway, United Kingdom, Switzerland, Sweden, New Zealand,
## Rwanda, Denmark, Finland
## low!!!!

boxplot(CA.data12.18$Social.support)
plot(CA.data12.18$Social.support)
text(x=1:124,y=CA.data12.18$Social.support,labels = CA.data12.18$Country.name)
## Central African Republic, Togo, Benin
## low!!!

colnames(CA.data12.18)

###star plots###
which(CA.data12.18$Country.name=="United States")
which(CA.data12.18$Country.name=="India")
which(CA.data12.18$Country.name=="Central African Republic")


par(mfrow=c(1,1))
stars(CA.data12.18[c(38,33,88,120,52,23),c(2:7)],nrow=2,ncol=3,labels=c("Finland","Denmark","Norway","United States","India","Central African Republic"))
a <- CA.data12.18[c(38,33,88,120,52,23),]

a$Country.name[which(a$Log.GDP.per.capita==min(a$Log.GDP.per.capita))]
a$Country.name[which(a$Social.support==min(a$Social.support))]
a$Country.name[which(a$Healthy.life.expectancy.at.birth==min(a$Healthy.life.expectancy.at.birth))]
a$Country.name[which(a$Freedom.to.make.life.choices==min(a$Freedom.to.make.life.choices))]
a$Country.name[which(a$Generosity==min(a$Generosity))]
a$Country.name[which(a$Perceptions.of.corruption==min(a$Perceptions.of.corruption))]


##################################  canonical correlation analysis  ##########################################3
###############data convertion for CCA######################
elec.rural <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/access-to-elec-rural.csv"),skip = 4,header = TRUE)
elec.rural <- elec.rural[,c(1,50:62)]  #2005-2017
ado.fert.rate <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/ado-fert-rate.csv"),skip = 4,header = TRUE)
ado.fert.rate <- ado.fert.rate[,c(1,50:62)]   #2005-2017
agr.land <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/agricultural-land.csv"),skip = 4,header = TRUE)
agr.land <- agr.land[,c(1,50:61)]    #2005-2016
birth.rate <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/birth-rate.csv"),skip = 4,header = TRUE)
birth.rate <- birth.rate[,c(1,50:62)]   #2005-2017
cereal.yield <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/cereal-yield.csv"),skip = 4,header = TRUE)
cereal.yield <- cereal.yield[,c(1,50:62)]   #2005-2017
fe.employ.ratio <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/female-to-male-employ-ratio.csv"),skip = 4,header = TRUE)
fe.employ.ratio <- fe.employ.ratio[,c(1,50:64)]  #2005-2019
mat.mor <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/maternal-mortality-rate.csv"),skip = 4,header = TRUE)
mat.mor <- mat.mor[,c(1,50:63)]  #2005-2018
popu.total <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/pop-total.csv"),skip = 4,header = TRUE)
popu.total <- popu.total[,c(1,50:63)]  #2005-2018
popu.grow <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/pop-grow.csv"),skip = 4,header = TRUE)
popu.grow <- popu.grow[,c(1,50:63)]  #2005-2018
profile.tax <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/profile-tax.csv"),skip = 4,header = TRUE)
profile.tax <- profile.tax[,c(1,50:64)]  #2005-2019
total.tax <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/total-tax-rate.csv"),skip = 4,header = TRUE)
total.tax <- total.tax[,c(1,50:64)]   #2005-2019
se.secondary <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/school-enrollment-secondary.csv"),skip = 4,header = TRUE)
se.secondary <- se.secondary[,c(1,50:63)]  #2005-2018
suic.rate <- read.csv(c("/Users/elenaxu/Library/Mobile Documents/com~apple~CloudDocs/VISP courses/STAT456/456project/456proj-data/suicide-mortality-rate.csv"),skip = 4,header = TRUE)
suic.rate <- suic.rate[,c(1,50:64)]   #2005, 2010, 2016, 2016
#we use the average of  latest three years' data, since we may have NA for some of those years
elec.rural.m <- elec.rural[,"X2017"]
ado.fert.rate.m <- ado.fert.rate[,"X2017"]
agr.land.m <- agr.land[,"X2016"]
birth.rate.m <- birth.rate[,"X2017"]
cereal.yield.m <- cereal.yield[,"X2017"]
fe.employ.ratio.m <- rowMeans(fe.employ.ratio[,c("X2017","X2018","X2019")])
mat.mor.m <- rowMeans(mat.mor[,c("X2017","X2018")])
popu.total.m <- rowMeans(popu.total[,c("X2017","X2018")])
popu.grow.m <- rowMeans(popu.grow[,c("X2017","X2018")])
profile.tax.m <- rowMeans(profile.tax[,c("X2017","X2018","X2019")])
total.tax.m <- rowMeans(total.tax[,c("X2017","X2018","X2019")])
se.secondary.m <- rowMeans(se.secondary[,c("X2017","X2018")])
suic.rate.m <- rowMeans(suic.rate[,c("X2015","X2016")])
wdi.var <- as.data.frame(cbind(levels(elec.rural$Country.Name),elec.rural.m,ado.fert.rate.m,agr.land.m,birth.rate.m,cereal.yield.m,
                               fe.employ.ratio.m,mat.mor.m,popu.total.m,popu.grow.m,profile.tax.m,total.tax.m,se.secondary.m,suic.rate.m))
colnames(wdi.var)[1] <- "Country.name"
##check NA values
for (i in 2:dim(wdi.var)[2]) {
  cat(sep = " ",colnames(wdi.var)[i],"has",sum(is.na(wdi.var[,i])),"NA values","\n")
  cat(sep = " ","index are", which(is.na(wdi.var[,i])),"\n")
  cat(sep = " ","\n")
}
##since variable school enrollment secondary level has 170 NA values, too many!!!
## we drop this variable
wdi.var <- wdi.var[,-13]
na.index <- c()
for (i in 2:dim(wdi.var)[2]) {
  cat(sep = " ",colnames(wdi.var)[i],"has",sum(is.na(wdi.var[,i])),"NA values","\n")
  cat(sep = " ","index are", which(is.na(wdi.var[,i])),"\n")
  cat(sep = " ","\n")
  na.index <- c(na.index,which(is.na(wdi.var[,i])))
}
na.index <- unique(na.index)
cat(sep = " ","total number of NA values is ",length(na.index),"\n")
cat(sep = " ","Index for all countries has NA values are ",na.index,"\n")
## we first delete all of them, and then merge it with whr2018 todo CCA
wdi.var <- wdi.var[-na.index,]   # we have 209 observations left!!!
sum(is.na(wdi.var))  #wdi.var becomes a balanced data!!!
for (i in 2:13) {
  wdi.var[,i] <- as.numeric(wdi.var[,i])
}

CCA.data <- left_join(CA.data12.18,wdi.var,by="Country.name")
sum(is.na(CCA.data$suic.rate.m))
CCA.data.2 <- CCA.data[-which(is.na(CCA.data$suic.rate.m)),]
sum(is.na(CCA.data.2))
str(CCA.data.2)


######conduct CCA#########
cormat <- cor(CCA.data.2[,-1])
r11 <- cormat[1:6,1:6]
r12 <- cormat[1:6,7:18]
r22 <- cormat[7:18,7:18]
r21 <- t(r12)
e1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21
e2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12
eigen(e1)
eigen(e2)
x <- as.matrix(CCA.data.2[,c(2:7)])
y <- as.matrix(CCA.data.2[,c(8:19)])
u <- x %*% eigen(e1)$vectors
v <- as.matrix(y %*% eigen(e2)$vectors)
v <- matrix(as.numeric(v[,c(1:6)]),ncol = 6,byrow = F)
cor(u,x)
cor(v,y[,c(1:6)])
cor(u,v)

colnames(CCA.data.2)


########################################## clustering analysis ################################################
par(mfrow=c(1,3))
country <- CA.data12.18$Country.name
dm12.18 <- dist(CA.data12.18[,-1])
ct <- hclust(dm12.18,method = "centroid")
plot(ct,main = "centroid")  #k=4
mt <- hclust(dm12.18,method = "median")
plot(mt,main = "centroid with midpoint")   #k=3
ward <- hclust(dm12.18,method = "ward.D2")
plot(ward,main = "Ward's")   #k=2


stree <- hclust(dm12.18,method="single")

plot(stree ,main="Single linkage",hang=-1,label.offset = 1,rotate=F)  # k= 4
#ggdendrogram(stree,rotate = T,size=4,theme_dendro = F,color="tomato")
ctree <- hclust(dm12.18,method="complete")
plot(ctree,ylab="Distance",main="Complete linkage",rotate=T)  # k=2
mtree <- hclust(dm12.18,method="average")
plot(mtree,ylab="Distance",main="Average linkage")  # k=2 or 4
#scree plot
par(mfrow=c(1,1))
k <- 6
n <- dim(CA.data12.18)[1]
wss <- rep(0,k)
zmean <- apply(CA.data12.18[,-1],2,mean)
for(i in 1:n){
  wss[1] <- wss[1]+sum((CA.data12.18[,-1]-zmean)^2)
}
for(i in 2:k){
  model <- kmeans(CA.data12.18[,-1],i)
  wss[i] <- sum(model$withinss)
}
par(mfrow=c(1,2),pty="s")
plot(1:k,wss,type="b",xlab="Number of clusters",
     ylab="Within cluster sum of squares",main=)
title ("Scree plot")  # scree plot suggest 2 cluster


############# PCA for clustering analysis 05-11 v.s. 12-18####################
###only conduct PCA on the first three variables
whr05.11.pca <- princomp(CA.data05.11[,-1],cor=TRUE)
###scree plot for PCA
par(cex.lab=1.5,cex.axis=1.5)
eigvals <- whr05.11.pca$sdev^2
k <- length(eigvals)
plot(1:k,eigvals,type="b",xlab="i",ylab=expression(lambda[i]))
pc05.11.overall <- whr05.11.pca$scores[,1:2]
summary(whr05.11.pca,loadings = T)

###conduct PCA separately
whr12.18.pca.1 <- princomp(CA.data12.18[,c(2:4)],cor=TRUE)
whr12.18.pca.2 <- princomp(CA.data12.18[,c(5:7)],cor=TRUE)
###scree plot for PCA separately
par(mfrow=c(1,2),cex.lab=1.5,cex.axis=1.5)
eigvals.1 <- whr12.18.pca.1$sdev^2
k.1 <- length(eigvals.1)
plot(1:k.1,eigvals.1,type="b",xlab="i",ylab=expression(lambda[i]))
eigvals.2 <- whr12.18.pca.2$sdev^2
k.2 <- length(eigvals.2)
plot(1:k.2,eigvals.2,type="b",xlab="i",ylab=expression(lambda[i]))
## #of PC is 1
pc12.18 <- cbind(whr12.18.pca.1$scores[,1],whr12.18.pca.2$scores[,1])

summary(whr12.18.pca.1,loadings = T)
summary(whr12.18.pca.2,loadings = T)
summary(whr12.18.pca,loadings = T)


###conduct PCA overally
whr12.18.pca <- princomp(CA.data12.18[,-1],cor=TRUE)
eigvals <- whr12.18.pca$sdev^2
k <- length(eigvals)
plot(1:k,eigvals,type="b",xlab="i",ylab=expression(lambda[i]))
pc12.18.overall <- whr12.18.pca$scores[,1:2]

# choose 2 clusters
par(mfrow=c(1,3))
k <- 2
km2.12.18 <- kmeans(CA.data12.18[,-1],k)
plot(pc12.18.overall[,1],pc12.18.overall[,2],type="n",xlab="PC1",ylab="PC2")
colors <- c("red","blue","magenta","black")
sym <- 1:4
for(i in 1:k){
  gp <- km2.12.18$cluster == i
  points(pc12.18.overall[gp,1],pc12.18.overall[gp,2],pch=sym[i],col=colors[i])
}
title("2-means clustering")   # fairly GOOD JOB!!!

# choose 3 clusters
k <- 3
km3.12.18 <- kmeans(CA.data12.18[,-1],k)
plot(pc12.18.overall[,1],pc12.18.overall[,2],type="n",xlab="PC1",ylab="PC2")
colors <- c("red","blue","magenta","black")
sym <- 1:4
for(i in 1:k){
  gp <- km3.12.18$cluster == i
  points(pc12.18.overall[gp,1],pc12.18.overall[gp,2],pch=sym[i],col=colors[i])
}
title("3-means clustering for 2012-2018")   # NOT BAD
# choose 4 clusters
k <- 4
km4 <- kmeans(CA.data12.18[,-1],k)
plot(pc12.18.overall[,1],pc12.18.overall[,2],type="n",xlab="PC1",ylab="PC2")
colors <- c("red","blue","magenta","black")
sym <- 1:4
for(i in 1:k){
  gp <- km4$cluster == i
  points(pc12.18.overall[gp,1],pc12.18.overall[gp,2],pch=sym[i],col=colors[i])
}
title("4-means clustering")
######## 2 or 3 clusters for 2012-2018!!!!!!!!!


###########clustering 2005-2011############

# choose 2 clusters
par(mfrow=c(1,3))
k <- 2
km2.05.11 <- kmeans(CA.data05.11[,-1],k)
plot(pc05.11.overall[,1],pc05.11.overall[,2],type="n",xlab="PC1",ylab="PC2")
colors <- c("red","blue","magenta","black")
sym <- 1:4
for(i in 1:k){
  gp <- km2.05.11$cluster == i
  points(pc05.11.overall[gp,1],pc05.11.overall[gp,2],pch=sym[i],col=colors[i])
}
title("2-means clustering")   # fairly GOOD JOB!!!

######### compare 05-11 and 12-18 2-means clustering  ############3
km2.12.cluster <- c()
for (i in 1:124) {
  if(km2.12.18$cluster[i] == 1){km2.12.cluster[i] <- 2}
  else{km2.12.cluster[i] <- 1}
}
cluster12.18 <- as.data.frame(cbind(CA.data12.18[,1],km2.12.cluster))
colnames(cluster12.18) <- c("Country.name","cluster")
cluster05.11 <- as.data.frame(cbind(CA.data05.11[,1],km2.05.11$cluster))
colnames(cluster05.11) <- c("Country.name","cluster")
country.merge <- inner_join(cluster05.11,cluster12.18,by="Country.name")
dim(country.merge)[1]
country.change <- c()
for (i in 1:92) {
  if(country.merge[i,2] != country.merge[i,3]){
    country.change <- c(country.change,country.merge[i,1])
  }
}
country.change


# choose 3 clusters
k <- 3
km3.05.11 <- kmeans(CA.data05.11[,-1],k)
plot(pc05.11.overall[,1],pc05.11.overall[,2],type="n",xlab="PC1",ylab="PC2")
colors <- c("red","blue","magenta","black")
sym <- 1:4
km3.05.11.cluster <- c()
for (i in 1:99) {
  if(km3.05.11$cluster[i] == 1){km3.05.11.cluster[i] <- 3}
  else if(km3.05.11$cluster[i] == 3) {km3.05.11.cluster[i] <- 1}
  else{km3.05.11.cluster[i] <-2}
}
for(i in 1:k){
  gp <- km3.05.11$cluster == i
  points(pc05.11.overall[gp,1],pc05.11.overall[gp,2],pch=sym[i],col=colors[i])
}
title("3-means clustering for 2005-2011")   # NOT BAD
######### compare 05-11 and 12-18 2-means clustering  #############

cluster12.18 <- as.data.frame(cbind(CA.data12.18[,1],km3.12.18$cluster))
colnames(cluster12.18) <- c("Country.name","cluster")
cluster05.11 <- as.data.frame(cbind(CA.data05.11[,1],km3.05.11$cluster))
colnames(cluster05.11) <- c("Country.name","cluster")
country.merge <- inner_join(cluster05.11,cluster12.18,by="Country.name")
dim(country.merge)[1]
country.change <- c()
for (i in 1:92) {
  if(country.merge[i,2] != country.merge[i,3]){
    country.change <- c(country.change,country.merge[i,1])
  }
}
country.merge[country.change,1]














