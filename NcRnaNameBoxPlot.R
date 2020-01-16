NcRNABoxPlot<-function(Exp,GTF,FileName) {
	library('rtracklayer')
  library('ggplot2')
	FileName<-FileName
	GTF<-import(GTF)
	Counts<-read.delim(Exp)
	GTF<-as.data.frame(GTF)
	ExpGenes<-as.character(Counts$Symbol)
	Genes<-GTF[GTF$type=='gene',]
	ncRNA<-as.character(Genes$gene_biotype)
	ncRNA<-unique(grep('*RNA*',ncRNA,value=TRUE))
	UniMt<-ncRNA[ !(ncRNA %in% c("Mt_rRNA","Mt_tRNA","miRNA"))]
	uncRNA<-Genes[ Genes$gene_biotype %in% UniMt ,]
	Genes<-Genes[Genes$gene_name %in% ExpGenes,]
	#GeneIds<-as.character(Genes$gene_id)
	counts<-NULL
	for (i in 1:length(UniMt)){
		genes<-Genes[Genes$gene_biotype==UniMt[i],]
		print(dim(genes))
		means<-Counts[as.character(Counts$Symbol) %in% genes$gene_name,]
		print(dim(means))
		means<-rowMeans(means[,3:ncol(means)])
		print(length(means))
		type<-rep(UniMt[i],length(means))
		data<-cbind(means,type)
		print(dim(data))
		counts<-rbind(counts,data)
		print(dim(counts))
		}
    dfcounts<-as.data.frame(counts)
	dfcounts$means<-as.numeric(as.character(dfcounts$means))
	pdf(paste("NcRnaTypes",FileName,"BoxPlot.pdf",sep=""), width=15, height=15)
	ggplot(dfcounts,aes(x=type,y=means,color=type))+geom_boxplot()+theme(axis.text.x=element_text(angle=90,hjust=1))
	dev.off()

  
  }