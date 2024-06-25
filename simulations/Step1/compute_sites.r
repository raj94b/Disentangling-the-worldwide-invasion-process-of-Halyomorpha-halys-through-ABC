arp<-readLines("./m1-temp/m1-temp_1_1.arp")

pop1L <- grep("1_", arp)
pop1L<- sort(c(pop1L,pop1L+1))
pop2L <- grep("2_", arp)
pop2L<- sort(c(pop2L,pop2L+1))
pop3L <- grep("3_", arp)
pop3L<- sort(c(pop3L,pop3L+1))

pop1<-arp[pop1L]
pop2<-arp[pop2L]
pop3<-arp[pop3L]

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



pri<-rbind(vet1,vet2,vet3)
siti_seg<-apply(pri,1,sum)
statotot<-rbind(stato1,stato2,stato3)

fixed<-c()
for (i in 1:2){
  for (j in (i+1):3){
    fix<-0
    for (m in 1:length(statotot[1,])) {if (sum(statotot[i,m],statotot[j,m])==1){fix<-fix+1} }
    fixed<-c(fixed,fix) #fixed sites
  }
  
}


priv<-c()
for (m in 1:length(vet1)){priv[m]<-sum(pri[,m])}

posi<-c()
for (i in 1: length(priv)){if (priv[i]==1) {posi<-c(posi,i)}}
sub<-as.matrix(pri[,c(posi)])
siti_priv<-apply(sub,1,sum)
siti<-matrix(nrow=1,ncol=length(c(siti_seg,siti_priv,fixed)))
siti[1,]<-c(siti_seg,siti_priv,fixed)
write.table(siti,"./siti_tot.tab",append=TRUE,col.names=F,row.names=F,quote=F)

