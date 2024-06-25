require(abcrf)
ntrees<-500

m1<-read.table("m1_50k_sim.txt",h=T)
m2<-read.table("m2_50k_sim.txt",h=T)
m3<-read.table("m3_50k_sim.txt",h=T)
s<-rbind(m1[,(15:34)],m2[,(15:34)],m3[,(15:34)])
i<-factor(c(rep(1,50000),rep(2,50000),rep(3,50000)))
f<-apply(s,2,var)!=0

da<-data.frame(i,s[,f])

a<-abcrf(i~.,data=da,lda=T,ntree=ntrees,paral=T,ncores=8)
plot(a, training=da, obs=obs, pdf=TRUE)

obs<-read.table("halys_1.obs",h=T)
obs<-obs[,f]

b<-predict(object=a,obs=rbind(obs,obs),training=da,ntree=ntrees,paral=T,paral.predict=T,ncores=4,ncores.predict=4)
