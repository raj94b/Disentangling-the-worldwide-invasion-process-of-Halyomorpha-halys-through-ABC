#Parameter estimation using ABC Random Forest, this is an example based on one specific parameter.
#These commands have been used for all parameter estimation.
require(abcrf)
ntrees<-500

M<-read.table("multiple_ne_cl_100k_sim.txt",h=T)
obs<-read.table("all_pops.obs",h=T)

M1<-cbind(M[22],M[,45:134]) #Table with the parameter of interest as the first columns

Forest<-regAbcrf(T5~.,data=M1,ntree=ntrees,paral=T,ncores=8)

b<-predict(object=Forest,obs=rbind(obs,obs),training=M1,ntree=ntrees,paral=T,paral.predict=T,ncores=4,ncores.predict=4)

pdf("T5_estimate_lim.pdf")
densityPlot(object=Forest,obs=rbind(obs,obs),training=M1,main="",ylab="Density",xlab="T")
dev.off()

