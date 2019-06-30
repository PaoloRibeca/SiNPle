#!/usr/bin/env Rscript

#######################################
# PCR error simulator for SiNPle simulations
#
# Parameters:
# - number of cycles
# - doubling efficiency
# - single nucleotide error rate
# - input FASTA file with original sequences
# - output prefix for FASTA file with amplified sequences and for error location
# - maximum sample size
#######################################

args = commandArgs(trailingOnly=TRUE)
library(ape)

generate_errors<-function(seq){
    newseq<-seq
    for(ch in c("a","c","g","t")){
        chred<-c("a","c","g","t")[c("a","c","g","t")!=ch]
        pos<-which(newseq==ch)
        newseq[pos]<-chred[sample(1:3,length(pos),replace=T)]
    }
    return(newseq)
}

ncycles<-as.numeric(args[1])
efficiency<-as.numeric(args[2])
rateerrors<-as.numeric(args[3])
infile<-args[4]
outfile<-paste(args[5],"fasta",sep=".")
outfile_pos<-paste(args[5],"errors",sep=".")
ssize<-as.numeric(args[6])

seq<-read.dna(infile,format="fasta")
seqc<-tolower(as.character(seq))
n0<-dim(seqc)[1]
l<-dim(seqc)[2]
errors<-c()

for(cc in 1:ncycles){
    n<-dim(seqc)[1]
    en<-round(efficiency*n)
    newseqc<-seqc[sample(1:n,en),]
    error_bool<-matrix(1:(en*l) %in% sample(1:(en*l),rpois(1,en*l*rateerrors)),nrow=en,ncol=l)
    errors<-c(errors,which(apply(error_bool,2,function(x){sum(x)})>0))
    newseqc[error_bool]<-generate_errors(newseqc[error_bool])
    seqc<-rbind(seqc,newseqc)
    if(dim(seqc)[1]>ssize){seqc<-seqc[sample(1:dim(seqc)[1],ssize),]}
}
errors<-intersect(errors,seg.sites(as.DNAbin(seqc[-c(1:n0),])))
errors<-unique(errors)
errors<-errors[order(errors)]
seq<-as.DNAbin(seqc)


write.dna(seq,file=outfile,format="fasta")
write.table(errors,file=outfile_pos,row.names=F,col.names=F,quote=F)
