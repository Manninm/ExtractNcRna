ExtracNcRna<-function(Exp,GTF,Details) {
	library('rtracklayer')
	Details<-Details
	GTF<-import(GTF)
	Counts<-read.delim(Exp)
	GTF<-as.data.frame(GTF)
	Gene<-GTF[GTF$type=='gene',]
	ncRNA<-as.character(Gene$gene_biotype)
	ncRNA<-unique(grep('*RNA*',ncRNA,value=TRUE))
	UniMt<-ncRNA[ !(ncRNA %in% c("Mt_rRNA","Mt_tRNA","miRNA"))]
	uncRNA<-Gene[ Gene$gene_biotype %in% UniMt ,]
	Genes<-as.character(uncRNA$gene_id)
	Counts<-Counts[ row.names(Counts) %in% Genes,]
	write.table(Counts,paste(Details,'NcRnaExp',sep=''),quote=FALSE,sep='\t')
	Freq<-uncRNA[ uncRNA$gene_id %in% as.character(row.names(Counts)), c('gene_id','gene_biotype')]
	Freq<-table(Freq$gene_biotype)
	write.table(Freq,paste(Details,'ncRnaFreq',sep=''),quote=FALSE,sep='\t',row.names=FALSE)
  }