#' Convert STRUCTURE format data to TASSEL format
#' 
#' This function converts STRUCTURE format genotype data format to the 
#' format required by TASSEL software. It handles recoding alleles,  
#' imputing missing data, pairing alleles, and attaching marker metadata. The 
#' 
#'
#' @param data Data frame with genotype calls and marker names
#' @param na.strings Character strings specifying missing genotype calls
#' @param met The meta data for the tassel data format
#' 
#' @return Data frame in TASSEL format with metadata and genotype calls
#' 
#' @examples  
#' data(Structure_example)
#' tassel_data <- str2tassel(data=Structure_example,met=metadata,na.strings="-9")
#' write.table(tassel_data,"Tassel_hapmap.txt", row.names = F,quote = F, sep = "\t")
#' write.csv(res_f,"Tassel_hapmap.csv", row.names = F,quote = F)
#' 
#'   
#' @export
str2tassel <- function(data, met, na.strings="-9") {
  
  # Function body
  
}


str2tassel=function(data,met,na.strings="-9"){
geno=unique(data$X)
mark=colnames(data)[-1]


code=c("A","C", "G","T", "M","M")
res=data.frame(GENO=geno)

j=1
i=2
for (i in 2:174) {
  x=data[,i]
  u=table(x)
  mark_name=colnames(data)[i]
  for (j in 1:length(u)) {
    x[which(x==as.numeric(names(which.max(u))))] <-code[j]
    u=u[-which.max(u)]
  }
  x1=x[seq(1,length(x),2)]
  x2=x[seq(2,length(x),2)]
  x=paste(x1,x2,sep = "")
  x[grep("NA",x, fixed = T)]<-"NN"
  
  x[grep("M",x, fixed = T)]<-"NN"
  
  
  res[mark_name]<-x
}

rownames(res)<-geno
res=res[,-1]

rs=mark
alleles=NA

meta_data=met
res=t(res)

temp=intersect(rownames(res),meta_data$rs)

meta_data_f=meta_data[na.omit(match(temp,meta_data$rs)),]
res_f=res[na.omit(match(temp,rownames(res))),]


rownames(res_f)==meta_data_f$rs

res_f=cbind(meta_data_f,res_f)

}

# write.table(res_f,"Final_Tassel_data_shan.txt", row.names = F,quote = F, sep = "\t")
# write.csv(res_f,"Final_Tassel_dataToshan.csv", row.names = F,quote = F)
