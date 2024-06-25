require(abcrf)
ntrees<-500
m1<-read.table("eu_us_si_50k_sim.txt",h=T)
m2<-read.table("eu_us_mi_50k_sim.txt",h=T)
m3<-read.table("eu_us_bd_50k_sim.txt",h=T)
obs<-read.table("eu_us.obs",h=T)
s<-rbind(m1[,(30:85)],m2[,(33:88)],m3[,(31:86)])
i<-factor(c(rep(1,50000),rep(2,50000),rep(3,50000)))


f<-apply(s,2,var)!=0
da<-data.frame(i,s[,f])
obs<-obs[,f]

a<-abcrf(i~.,data=da,lda=T,ntree=ntrees,paral=T,ncores=8)

b<-predict(object=a,obs=rbind(obs,obs),training=da,ntree=ntrees,paral=T,paral.predict=T,ncores=4,ncores.predict=4)