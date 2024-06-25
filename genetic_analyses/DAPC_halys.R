library(adegenet)
hal<-read.genetix("halys_snps.gtx")
grp <- find.clusters(hal, max.n.clust = 50) #find genetic groups
write.table(grp$grp, "halyomorpha/halys_6grp_kmeans.txt", sep="\t", quote = FALSE) #salvare info dei gruppi
dc <- dapc(hal, grp$grp) #DAPC
col<-c("blue", "green", "orange", "red", "magenta1", "cyan")
pdf("halys_DAPC.pdf")
scatter(dc, scree.da=FALSE, col=col, bg="white", pch=20, cstar=0, cex=1, clab=.5)
dev.off()