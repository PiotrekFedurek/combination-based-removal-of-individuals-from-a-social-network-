




library(bipartite)
library(tnet)
library(combinat)
#creating a simulated network (N=25, JUVENILES 1:10, ADULTS 15:24)

N <- 6#number of nodes
dyads <- expand.grid(ID1=1:25,ID2=1:25) #creating ties
dyads <- dyads[which(dyads$ID1 != dyads$ID2),]#self-loops not allowed
weights <- rbeta(nrow(dyads),1,15)#generating weights
network <- matrix(0, nrow=N, ncol=N)#creating matrix
network[cbind(dyads$ID1,dyads$ID2)] <- weights#assigning weights to the corresponding ties
network# resulting directed network 
M<-as.matrix(network)
b<-web2edges(M, webName=NULL, weight.column=TRUE, both.directions=FALSE,is.one.mode=T, out.files=c("edges", "names", "groups")[1:2],return=T, verbose=T)
b<- as.tnet(b, type="weighted one-mode tnet")



v
v[1,]
f<-v[1,]
e<-f[6]
e

c<- combn(10, 10)
v<-t(c)
newrow = c(1:10)
v= rbind(v,newrow)
newcol = c(1:1)
v= cbind(v,newcol)
v

cluster_vect2 <- ""    

for (i in 1:1)    
{
  f<-v[1,]  
  e<-f[1]
  n<-b[b$i!=e & b$j!=e, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e2<-f[2]
  n<-n[n$i!=e2 & n$j!=e2, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e3<-f[3]
  n<-n[n$i!=e3 & n$j!=e3, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e4<-f[4]
  n<-n[n$i!=e4 & n$j!=e4, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e5<-f[5]
  n<-n[n$i!=e5 & n$j!=e5, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e6<-f[6]
  n<-n[n$i!=e6 & n$j!=e6, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e7<-f[7]
  n<-n[n$i!=e7 & n$j!=e7, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e8<-f[8]
  n<-n[n$i!=e8 & n$j!=e8, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e9<-f[9]
  n<-n[n$i!=e9 & n$j!=e9, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e10<-f[10]
  n<-n[n$i!=e10 & n$j!=e10, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  n<-symmetrise_w(n, method="SUM")
  t2<-clustering_local_w(n,measure="am")
  t2[is.nan(t2)] <- 0 
  t2<-as.data.frame(t2)
  t2<-t2[t2$node!=e, ]
  t2<-t2[t2$node!=e2, ]
  t2<-t2[t2$node!=e3, ]
  t2<-t2[t2$node!=e4, ]
  t2<-t2[t2$node!=e5, ]
  t2<-t2[t2$node!=e6, ]
  t2<-t2[t2$node!=e7, ]
  t2<-t2[t2$node!=e8, ]
  t2<-t2[t2$node!=e9, ]
  t2<-t2[t2$node!=e10, ]
  t2<-t2[,-1]
  tmpval2<-mean(t2)     
  cluster_vect2 <- c(cluster_vect2, tmpval2)
  v<-v[-1,]
}
cluster_vect2 
g<-as.numeric(cluster_vect2) 
g<-g[-1]	
m<-mean(g)
m




c<-combn(15:24, 9)
v<-t(c)
newrow = c(1:9)
v= rbind(v,newrow)
newcol = c(1:1)
v= cbind(v,newcol)

cluster_vect2 <- ""    

for (i in 1:10)    
{
  f<-v[1,]  
  
  e<-f[1]
  n<-b[b$i!=e & b$j!=e, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e2<-f[2]
  n<-n[n$i!=e2 & n$j!=e2, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e3<-f[3]
  n<-n[n$i!=e3 & n$j!=e3, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e4<-f[4]
  n<-n[n$i!=e4 & n$j!=e4, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e5<-f[5]
  n<-n[n$i!=e5 & n$j!=e5, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e6<-f[6]
  n<-n[n$i!=e6 & n$j!=e6, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e7<-f[7]
  n<-n[n$i!=e7 & n$j!=e7, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e8<-f[8]
  n<-n[n$i!=e8 & n$j!=e8, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e9<-f[9]
  n<-n[n$i!=e9 & n$j!=e9, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  
  
  t2<-clustering_local_w(n,measure="am")  
  t2[is.nan(t2)] <- 0 
  t2<-as.data.frame(t2)
  t2<-t2[t2$node!=e, ]
  t2<-t2[t2$node!=e2, ]
  t2<-t2[t2$node!=e3, ]
  t2<-t2[t2$node!=e4, ]
  t2<-t2[t2$node!=e5, ]
  t2<-t2[t2$node!=e6, ]
  t2<-t2[t2$node!=e7, ]
  t2<-t2[t2$node!=e8, ]
  t2<-t2[t2$node!=e9, ]
  t2<-t2[,-1]
  tmpval2<-mean(t2)      
  cluster_vect2 <- c(cluster_vect2, tmpval2)
  v<-v[-1,]
}
cluster_vect2 
gg<-as.numeric(cluster_vect2) 
gg<-gg[-1]
mm<-mean(gg)
mm


c<-combn(1:25, 1)
v<-t(c)
v
newrow = c(1:1)
v= rbind(v,newrow)
newcol = c(1:1)
v= cbind(v,newcol)

cluster_vect2 <- ""    

for (i in 1:25)    
{
  f<-v[1,]  
  
  e<-f[1]
  n<-b[b$i!=e & b$j!=e, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  
  
  
  
  t2<-clustering_local_w(n,measure="am")  
  t2[is.nan(t2)] <- 0 
  t2<-as.data.frame(t2)
  t2<-t2[t2$node!=e, ]
  
  
  t2<-t2[,-1]
  tmpval2<-mean(t2)      
  cluster_vect2 <- c(cluster_vect2, tmpval2)
  v<-v[-1,]
}
cluster_vect2 
ggg<-as.numeric(cluster_vect2) 
ggg<-ggg[-1]
mmm<-mean(ggg)
mmm


cluster_vect2 <- ""    

for (i in 1:500)    
{
  f<-sample(1:25,10,replace=F)    
  e<-f[1]
  n<-b[b$i!=e & b$j!=e, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  
  e2<-f[2]
  n<-n[n$i!=e2 & n$j!=e2, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e3<-f[3]
  n<-n[n$i!=e3 & n$j!=e3, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e4<-f[4]
  n<-n[n$i!=e4 & n$j!=e4, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e5<-f[5]
  n<-n[n$i!=e5 & n$j!=e5, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e6<-f[6]
  n<-n[n$i!=e6 & n$j!=e6, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e7<-f[7]
  n<-n[n$i!=e7 & n$j!=e7, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e8<-f[8]
  n<-n[n$i!=e8 & n$j!=e8, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e9<-f[9]
  n<-n[n$i!=e9 & n$j!=e9, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  e10<-f[10]
  n<-n[n$i!=e10 & n$j!=e10, ]
  row.has.na <- apply(n, 1, function(x){any(is.na(x))})
  n<-n[!row.has.na,]
  n<- as.tnet(n, type="weighted one-mode tnet")
  
  
  
  t2<-clustering_local_w(n,measure="am")  
  t2[is.nan(t2)] <- 0 
  t2<-as.data.frame(t2)
  t2<-t2[t2$node!=e, ]
  t2<-t2[t2$node!=e2, ]
  t2<-t2[t2$node!=e3, ]
  t2<-t2[t2$node!=e4, ]
  t2<-t2[t2$node!=e5, ]
  t2<-t2[t2$node!=e6, ]
  t2<-t2[t2$node!=e7, ]
  t2<-t2[t2$node!=e8, ]
  t2<-t2[t2$node!=e9, ]
  t2<-t2[t2$node!=e10, ]
  
  t2<-t2[,-1]
  tmpval2<-mean(t2)      
  cluster_vect2 <- c(cluster_vect2, tmpval2)
}
cluster_vect2 
ggg<-as.numeric(cluster_vect2) 
ggg<-ggg[-1]
mmm<-mean(ggg)
mmm

