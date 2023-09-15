library(Morpho)
library(shapes)
library(Arothron)
library(shapes)
library(geomorph)
library(geosphere)
library(vegan)


rm(list=ls())

readset<-function(folder)
{
files<-length(list.files(folder))
seti<-as.matrix(read.table(paste(folder,list.files(folder)[1],sep="/")))
Array<-array(NA,dim=c(dim(seti)[1],dim(seti)[2],files))
for(i in 1:dim(Array)[3]){
Array[,,i]<-as.matrix(read.table(paste(folder,list.files(folder)[i],sep="/")))
}
return(Array)
}

#Load landmarks+semilandmarks
Sliding<-readset("./Data/NewSliding") # sliding TPS
ICP<-readset("./Data/ICP")   # LS&ICP
NICP<-readset("./Data/NICP") # TPS&NICP

#compute mean configuration
gpaSliding<- gpagen(Sliding) #GPA-alignment
gpaICP<- gpagen(ICP) #GPA-alignment
gpaNICP<- gpagen(NICP) #GPA-alignment


#compute PRD betwee means
OPA <- rotonto(gpaSliding$consensus,gpaICP$consensus) #sliding vs LS&ICP
distSliding_ICP<-procdist(OPA$X,OPA$Y,type="full")

OPA <- rotonto(gpaSliding$consensus,gpaNICP$consensus)#sliding vs TPS&NICP
distSliding_NICP<-procdist(OPA$X,OPA$Y,type="full")

OPA <- rotonto(gpaICP$consensus,gpaNICP$consensus)#LS&ICP vs TPS&NICP
distICP_NICP<-procdist(OPA$X,OPA$Y,type="full")


#compute correlation of vector of PRD
size<-dim(Sliding)
meanSliding<-c() #PRD vector of sliding
for(i in  1:size[3])
{
  meanSliding<-c(meanSliding,procdist(gpaSliding$coords[,,i],gpaSliding$consensus,type="full"))
}
meanICP<-c() #PRD vector of ICP
for(i in  1:size[3])
{
  meanICP<-c(meanICP,procdist(gpaICP$coords[,,i],gpaICP$consensus,type="full"))
}
meanNICP<-c()  #PRD vector of NICP
for(i in  1:size[3])
{
  meanNICP<-c(meanNICP,procdist(gpaNICP$coords[,,i],gpaNICP$consensus,type="full"))
}

corr_S_I<-cor(meanSliding,meanICP) #sliding vs LS&ICP
corr_S_N<- cor(meanSliding,meanNICP)#sliding vs TPS&NICP
corr_N_I<- cor(meanNICP,meanICP)#LS&ICP vs TPS&NICP


#compute correlation of PC
PCASliding<- gm.prcomp(gpaSliding$coords) #PCA of sliding
PCAICP<- gm.prcomp(gpaICP$coords)         #PCA of LS&ICP
PCANICP<- gm.prcomp(gpaNICP$coords)       #PCA of TPS&NICP

#PC1
cor(PCASliding$x[,1],PCAICP$x[,1]) #sliding vs LS&ICP
cor(PCASliding$x[,1],PCANICP$x[,1]) #sliding vs TPS&NICP
cor(PCAICP$x[,1],PCANICP$x[,1]) #LS&ICP vs TPS&NICP

cor(PCASliding$x[,2],PCAICP$x[,2]) #sliding vs ICP
cor(PCASliding$x[,2],PCANICP$x[,2]) #sliding vs NICP
cor(PCAICP$x[,2],PCANICP$x[,2]) #ICP vs NICP

#anlges of allometric vector 
Array<-bindArr(Sliding,ICP,NICP,along=3)
gpa<- gpagen(Array)
pca<-gm.prcomp(gpa$coords) #shape space
 
gmd1=geomorph.data.frame(coords=pca$x[1:size[3],],size=log(gpa$Csize[1:size[3]]))
fit1=procD.lm(coords~size,data=gmd1,iter = 999)
gmd2=geomorph.data.frame(coords=pca$x[(size[3]+1):(2*size[3]),],size=log(gpa$Csize[(size[3]+1):(2*size[3])]))
fit2=procD.lm(coords~size,data=gmd2,iter = 999)
gmd3=geomorph.data.frame(coords=pca$x[(2*size[3]+1):(3*size[3]),],size=log(gpa$Csize[(2*size[3]+1):(3*size[3])]))
fit3=procD.lm(coords~size,data=gmd3,iter = 999)

value<-c()
coeffg1=fit1$coefficients[2,]
coeffg2=fit2$coefficients[2,]
coeffg3=fit3$coefficients[2,]

radangle1<-angleTest(coeffg1,coeffg2) #sliding vs LS&ICP
angles1<-radangle1$angle*180/pi
value<-c(value,angles1)
value<-c(value,radangle1$p.value)

radangle2<-angleTest(coeffg1,coeffg3) #sliding vs TPS&NICP
angles2<-radangle2$angle*180/pi
value<-c(value,angles2)
value<-c(value,radangle2$p.value)

radangle3<-angleTest(coeffg2,coeffg3) #LS&ICP vs TPS&NICP
angles3<-radangle3$angle*180/pi
value<-c(value,angles3)
value<-c(value,radangle3$p.value)
value

