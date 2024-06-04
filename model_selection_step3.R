require(abcrf)
ntrees<-500
m1<-read.table("multiple_cl_ne_50k_sim.txt",h=T)
m2<-read.table("multiple_ne_cl_50k_sim.txt",h=T)
obs<-read.table("all_pops.obs",h=T)
s<-rbind(m1[,(45:134)],m2[,(45:134)])
i<-factor(c(rep(1,50000),rep(2,50000)))
f<-apply(s,2,var)!=0
da<-data.frame(i,s[,f])
obs<-obs[,f]
a<-abcrf(i~.,data=da,lda=T,ntree=ntrees,paral=T,ncores=8)
plot(a, training=da, obs=obs, pdf=TRUE)
b<-predict(object=a,obs=rbind(obs,obs),training=da,ntree=ntrees,paral=T,paral.predict=T,ncores=4,ncores.predict=4)