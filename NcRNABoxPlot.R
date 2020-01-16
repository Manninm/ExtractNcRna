NcRNABoxPlot<-function(Exp,GTF,FileName) {
	library('rtracklayer')
	FileName<-FileName
	GTF<-import(GTF)
	Counts<-read.delim(Exp)
	GTF<-as.data.frame(GTF)
	ExpGenes<-as.character(row.names(Counts))
	Genes<-GTF[GTF$type=='gene',]
	ncRNA<-as.character(Genes$gene_biotype)
	ncRNA<-unique(grep('*RNA*',ncRNA,value=TRUE))
	UniMt<-ncRNA[ !(ncRNA %in% c("Mt_rRNA","Mt_tRNA","miRNA"))]
	uncRNA<-Genes[ Genes$gene_biotype %in% UniMt ,]
	Genes<-Genes[Genes$gene_id %in% ExpGenes,]
	#GeneIds<-as.character(Genes$gene_id)
	counts<-NULL
	for (i in 1:length(UniMt)){
		genes<-Genes[Genes$gene_biotype==UniMt[i],]
		print(dim(genes))
		means<-Counts[row.names(Counts) %in% genes$gene_id,]
		print(dim(means))
		means<-rowMeans(means)
		print(length(means))
		type<-rep(UniMt[i],length(means))
		data<-cbind(means,type)
		print(dim(data))
		counts<-rbind(counts,data)
		}
    dfcounts<-as.data.frame(counts)
	dfcounts$means<-as.numeric(as.character(dfcounts$means))
	pdf(paste("NcRnaTypes",FileName,"BoxPlot.pdf",sep=""), width=15, height=15)
	par(cex.axis=0.5)
	boxplot(dfcounts[,1]~dfcounts[,2])
	dev.off()

  
  }