arp<-readLines("eu_us.arp")

pop1L <- grep("1-", arp)
pop1Lt<- sort(c(pop1L,pop1L+1))
pop2L <- grep("2-", arp)
pop2Lt<- sort(c(pop2L,pop2L+1))
pop3L <- grep("3-", arp)
pop3Lt<- sort(c(pop3L,pop3L+1))
pop4L <- grep("4-", arp)
pop4Lt<- sort(c(pop4L,pop4L+1))
pop5L <- grep("5-", arp)
pop5Lt<- sort(c(pop5L,pop5L+1))
pop6L <- grep("6-", arp)
pop6Lt<- sort(c(pop6L,pop6L+1))

pop1<-arp[pop1Lt]
pop2<-arp[pop2Lt]
pop3<-arp[pop3Lt]
pop4<-arp[pop4Lt]
pop5<-arp[pop5Lt]
pop6<-arp[pop6Lt]


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



for (j in 1: length(pop4))
{
a<-unlist(strsplit(pop4[j], split="\t"))
b<- unlist(strsplit(a[3],split=" "))
if (j==1) {mat4<-matrix(nrow=length(pop4),ncol=(length(b)))
		mat4[j,]<-b[1:length(b)]}

else {mat4[j,]<-b[1:length(b)]}
}

vet4<-c()
for (p in 1: length(mat4[1,])){
vet4[p]<-length(unique(mat4[,p]))!=1}
stato4<-c()
for (i in 1:length(vet4))
	{if (length(unique(mat4[,i]))==1){
	stato4[i]<-as.character(unique(mat4[,i]))}
	else {stato4[i]=0}}



for (j in 1: length(pop5))
{
a<-unlist(strsplit(pop5[j], split="\t"))
b<- unlist(strsplit(a[3],split=" "))
if (j==1) {mat5<-matrix(nrow=length(pop5),ncol=(length(b)))
		mat5[j,]<-b[1:length(b)]}

else {mat5[j,]<-b[1:length(b)]}
}

vet5<-c()
for (p in 1: length(mat5[1,])){
vet5[p]<-length(unique(mat5[,p]))!=1}
stato5<-c()
for (i in 1:length(vet5))
	{if (length(unique(mat5[,i]))==1){
	stato5[i]<-as.character(unique(mat5[,i]))}
	else {stato5[i]=0}}



for (j in 1: length(pop6))
{
a<-unlist(strsplit(pop6[j], split="\t"))
b<- unlist(strsplit(a[3],split=" "))
if (j==1) {mat6<-matrix(nrow=length(pop6),ncol=(length(b)))
		mat6[j,]<-b[1:length(b)]}

else {mat6[j,]<-b[1:length(b)]}
}

vet6<-c()
for (p in 1: length(mat6[1,])){
vet6[p]<-length(unique(mat6[,p]))!=1}
stato6<-c()
for (i in 1:length(vet6))
	{if (length(unique(mat6[,i]))==1){
	stato6[i]<-as.character(unique(mat6[,i]))}
	else {stato6[i]=0}}





pri<-rbind(vet1,vet2,vet3,vet4,vet5,vet6)
siti_seg<-apply(pri,1,sum)
statotot<-rbind(stato1,stato2,stato3,stato4,stato5,stato6)


fixed<-c()
for (i in 1:5){
	for (j in (i+1):6){
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
for (i in 1: length(priv)){if (priv[i]==1) {posi<-c(posi,i)}} ###posi=colonne di pri in cui abbiamo siti segreganti privati
sub<-pri[,c(posi)]
siti_priv<-apply(sub,1,sum)
siti<-matrix(nrow=1,ncol=length(c(siti_seg,siti_priv,fixed)))
siti[1,]<-c(siti_seg,siti_priv,fixed)
write.table(siti,"sites_step2.obs",col.names=c("ss1","ss2","ss3","ss4","ss5","ss6","sp1","sp2","sp3","sp4","sp5","sp6","sfo12","sfo13","sfo14","sfo15","sfo16","sfo23","sfo24","sfo25","sfo26","sfo34","sfo35","sfo36","sfo45","sfo46","sfo56"),row.names=F,quote=F)


