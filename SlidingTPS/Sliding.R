#upload libraries
library(Morpho)
library(rgl)
library(Arothron)
library(shapes)
library(geomorph) 


setwd("D:/SlidingTPS/Data")

rm(list=ls()) 
n=41   # number of landmarks
k=200  # number of semilandmarks
#upload surfaces
refsur<-file2mesh("D:/SlidingTPS/Data/Source.obj") #reference model
landmarks<-as.matrix(read.table("D:/SlidingTPS/Data/Source-Lm.txt",skip=0))

reflset<-matrix(landmarks[1:n,],n,3) # number of landmarks
refslset<-as.matrix(read.table("D:/SlidingTPS/Data/Source-Semi.txt",skip=0))

Atlas<-createAtlas(refsur,reflset,patch = refslset)

Tarsur<-file2mesh("D:/SlidingTPS/Data/Target.obj")
tarlset_k<-as.matrix(read.table("D:/SlidingTPS/Data/Target-Lm.txt",skip=0))

mesh2ply(refsur, filename="meanFace")
mesh2ply(Tarsur, filename="Target")

Atlas_k<-createAtlas(refsur,reflset,patch = refslset)
data_k <- bindArr(reflset,tarlset_k, along=3)
dimnames(data_k)[[3]] <- c("meanFace", "Target")
patched_k <- placePatch(Atlas_k, data_k, path="./", fileext = ".ply",inflate=NULL)  

dim(patched_k)
fix_k<-c(1:n) 
surp_k<-c(1:nrow(patched_k))[-fix_k]
slide_k <- slider3d(patched_k, SMvector=fix_k, deselect=TRUE, surp=surp_k, iterations=0, sur.path = ".",sur.type = "ply", mc.cores=1,fixRepro = FALSE) #Perform Sliding

write.table(slide_k$dataslide[(n+1):(n+k),,2],"D:/SlidingTPS/Data/Target-semilandmarks.txt", row.names = FALSE, col.names = FALSE) #save the semilandmarks
