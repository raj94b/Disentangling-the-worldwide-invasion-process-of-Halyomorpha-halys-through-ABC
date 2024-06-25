arp<-readLines("halys_sep_china.arp")

pop1L <- grep("1-", arp)
pop1Lt<- sort(c(pop1L,pop1L+1))
pop2L <- grep("2-", arp)
pop2Lt<- sort(c(pop2L,pop2L+1))
pop3L <- grep("3-", arp)
pop3Lt<- sort(c(pop3L,pop3L+1))

pop1<-arp[pop1Lt]
pop2<-arp[pop2Lt]
pop3<-arp[pop3Lt]


for (j in 1: length(pop1))
{
a<-unlist(strsplit(pop1[j], split="\t"))
b<- unlist(strsplit(a[3],split=" "))
if (j==1) {mat1<-matrix(nrow=length(pop1),ncol=(length(b)))
		mat1[j,]<-b[1:length(b)]}

else {mat1[j,]<-b[1:length(b)]}
}

vet1<-c()
for (p in 1: length(mat1[1,])){
vet1[p]<-length(unique(mat1[,p]))!=1}

stato1<-c()
for (i in 1:length(vet1))
	{if (length(unique(mat1[,i]))==1){
	stato1[i]<-as.character(unique(mat1[,i]))}
	else {stato1[i]=0}}




for (j in 1: length(pop2))
{
a<-unlist(strsplit(pop2[j], split="\t"))
b<- unlist(strsplit(a[3],split=" "))
if (j==1) {mat2<-matrix(nrow=length(pop2),ncol=(length(b)))
		mat2[j,]<-b[1:length(b)]}

else {mat2[j,]<-b[1:length(b)]}
}

vet2<-c()
for (p in 1: length(mat2[1,])){
vet2[p]<-length(unique(mat2[,p]))!=1}
stato2<-c()
for (i in 1:length(vet2))
	{if (length(unique(mat2[,i]))==1){
	stato2[i]<-as.character(unique(mat2[,i]))}
	else {stato2[i]=0}}




for (j in 1: length(pop3))
{
a<-unlist(strsplit(pop3[j], split="\t"))
b<- unlist(strsplit(a[3],split=" "))
if (j==1) {mat3<-matrix(nrow=length(pop3),ncol=(length(b)))
		mat3[j,]<-b[1:length(b)]}

else {mat3[j,]<-b[1:length(b)]}
}

vet3<-c()
for (p in 1: length(mat3[1,])){
vet3[p]<-length(unique(mat3[,p]))!=1}
stato3<-c()
for (i in 1:length(vet3))
	{if (length(unique(mat3[,i]))==1){
	stato3[i]<-as.character(unique(mat3[,i]))}
	else {stato3[i]=0}}



pri<-rbind(vet1,vet2,vet3)
siti_seg<-apply(pri,1,sum)
statotot<-rbind(stato1,stato2,stato3)


fixed<-c()
for (i in 1:2){
	for (j in (i+1):3){
		fix<-0
					
			for (m in 1:length(statotot[1,])) {if ((statotot[i,m]!=statotot[j,m])&&(statotot[i,m]!="0")&&(statotot[j,m]!="0")){fix<-fix+1
			print (i)
			print (j)
			print(m)} }
fixed<-c(fixed,fix)
}

}



priv<-c()
for (m in 1:length(vet1)){priv[m]<-sum(pri[,m])}

posi<-c()
for (i in 1: length(priv)){if (priv[i]==1) {posi<-c(posi,i)}}
sub<-pri[,c(posi)]
siti_priv<-apply(sub,1,sum)
siti<-matrix(nrow=1,ncol=length(c(siti_seg,siti_priv,fixed)))
siti[1,]<-c(siti_seg,siti_priv,fixed)
write.table(siti,"sites_step1.obs",col.names=c("ss1","ss2","ss3","sp1","sp2","sp3","sfo12","sfo13","sfo23"),row.names=F,quote=F)


