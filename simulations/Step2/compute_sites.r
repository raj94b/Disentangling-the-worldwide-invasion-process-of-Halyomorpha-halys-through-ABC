arp<-readLines("./eu_us_si-temp/eu_us_si-temp_1_1.arp")

pop1L <- grep("1_", arp)
pop1L<- sort(c(pop1L,pop1L+1))
pop2L <- grep("2_", arp)
pop2L<- sort(c(pop2L,pop2L+1))
pop3L <- grep("3_", arp)
pop3L<- sort(c(pop3L,pop3L+1))
pop4L <- grep("4_", arp)
pop4L<- sort(c(pop4L,pop4L+1))
pop5L <- grep("5_", arp)
pop5L<- sort(c(pop5L,pop5L+1))
pop6L <- grep("6_", arp)
pop6L<- sort(c(pop6L,pop6L+1))

pop1<-arp[pop1L]
pop2<-arp[pop2L]
pop3<-arp[pop3L]
pop4<-arp[pop4L]
pop5<-arp[pop5L]
pop6<-arp[pop6L]

for (j in 1: length(pop1))
{
  r<-unlist(strsplit(pop1[j], split="\t"))
  a<-unlist(strsplit(r[3], split=""))
  if (j==1) {mat1<-matrix(nrow=length(pop1),ncol=(length(a)-1))
  mat1[j,]<-a[2:length(a)]}
  
  else {mat1[j,]<-a[2:length(a)]}
}
vet1<-c()
for (p in 1: length(mat1[1,])){
vet1[p]<-length(unique(mat1[,p]))!=1}
stato1<-c()
for (i in 1:length(vet1))
	{if (length(unique(mat1[,i]))==1){
	stato1[i]<-as.numeric(unique(mat1[,i]))}
	else {stato1[i]=4}}





for (j in 1: length(pop2))
{
  r<-unlist(strsplit(pop2[j], split="\t"))
  a<-unlist(strsplit(r[3], split=""))
  if (j==1) {mat2<-matrix(nrow=length(pop2),ncol=(length(a)-1))
  mat2[j,]<-a[2:length(a)]}
  
  else {mat2[j,]<-a[2:length(a)]}
}

vet2<-c()
for (p in 1: length(mat2[1,])){
vet2[p]<-length(unique(mat2[,p]))!=1}
stato2<-c()
for (i in 1:length(vet2))
	{if (length(unique(mat2[,i]))==1){
	stato2[i]<-as.numeric(unique(mat2[,i]))}
	else {stato2[i]=4}}




for (j in 1: length(pop3))
{
  r<-unlist(strsplit(pop3[j], split="\t"))
  a<-unlist(strsplit(r[3], split=""))
  if (j==1) {mat3<-matrix(nrow=length(pop3),ncol=(length(a)-1))
  mat3[j,]<-a[2:length(a)]}
  
  else {mat3[j,]<-a[2:length(a)]}
}

vet3<-c()
for (p in 1: length(mat3[1,])){
vet3[p]<-length(unique(mat3[,p]))!=1}
stato3<-c()
for (i in 1:length(vet3))
	{if (length(unique(mat3[,i]))==1){
	stato3[i]<-as.numeric(unique(mat3[,i]))}
	else {stato3[i]=4}}



for (j in 1: length(pop4))
{
  r<-unlist(strsplit(pop4[j], split="\t"))
  a<-unlist(strsplit(r[3], split=""))
  if (j==1) {mat4<-matrix(nrow=length(pop4),ncol=(length(a)-1))
  mat4[j,]<-a[2:length(a)]}
  
  else {mat4[j,]<-a[2:length(a)]}
}

vet4<-c()
for (p in 1: length(mat4[1,])){
vet4[p]<-length(unique(mat4[,p]))!=1}

stato4<-c()
for (i in 1:length(vet4))
	{if (length(unique(mat4[,i]))==1){
	stato4[i]<-as.numeric(unique(mat4[,i]))}
	else {stato4[i]=4}}


for (j in 1: length(pop5))
{
  r<-unlist(strsplit(pop5[j], split="\t"))
  a<-unlist(strsplit(r[3], split=""))
  if (j==1) {mat5<-matrix(nrow=length(pop5),ncol=(length(a)-1))
  mat5[j,]<-a[2:length(a)]}
  
  else {mat5[j,]<-a[2:length(a)]}
}

vet5<-c()
for (p in 1: length(mat5[1,])){
vet5[p]<-length(unique(mat5[,p]))!=1}
stato5<-c()
for (i in 1:length(vet5))
	{if (length(unique(mat5[,i]))==1){
	stato5[i]<-as.numeric(unique(mat5[,i]))}
	else {stato5[i]=4}}


for (j in 1: length(pop6))
{
  r<-unlist(strsplit(pop6[j], split="\t"))
  a<-unlist(strsplit(r[3], split=""))
  if (j==1) {mat6<-matrix(nrow=length(pop6),ncol=(length(a)-1))
  mat6[j,]<-a[2:length(a)]}
  
  else {mat6[j,]<-a[2:length(a)]}
}

vet6<-c()
for (p in 1: length(mat6[1,])){
vet6[p]<-length(unique(mat6[,p]))!=1}
stato6<-c()
for (i in 1:length(vet6))
	{if (length(unique(mat6[,i]))==1){
	stato6[i]<-as.numeric(unique(mat6[,i]))}
	else {stato6[i]=4}}


pri<-rbind(vet1,vet2,vet3,vet4,vet5,vet6)
siti_seg<-apply(pri,1,sum)
statotot<-rbind(stato1,stato2,stato3,stato4,stato5,stato6)

fixed<-c()
for (i in 1:5){
	for (j in (i+1):6){
		fix<-0
			for (m in 1:length(statotot[1,])) {if (sum(statotot[i,m],statotot[j,m])==1){fix<-fix+1} }
fixed<-c(fixed,fix) #fixed sites
}

}


priv<-c()
for (m in 1:length(vet1)){priv[m]<-sum(pri[,m])}

posi<-c()
for (i in 1: length(priv)){if (priv[i]==1) {posi<-c(posi,i)}}

siti_priv<-apply(sub,1,sum)
siti<-matrix(nrow=1,ncol=length(c(siti_seg,siti_priv,fixed)))
siti[1,]<-c(siti_seg,siti_priv,fixed)
write.table(siti,"./siti_tot.tab",append=TRUE,col.names=F,row.names=F,quote=F)

