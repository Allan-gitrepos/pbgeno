#' Calculate PIC values for molecular marker data
#'
#' This function calculates polymorphism information content (PIC) 
#' values for each marker in the given data set. It handles both 
#' co-dominant and dominant markers appropriately.
#'
#' @param data Data frame with marker genotype data. First column should 
#'   be the marker names. Subsequent columns are the genotypes for each 
#'   sample.
#'   
#' @return A list with the following components:
#' \itemize{  
#'   \item pic: Data frame with marker name, heterozygosity, and PIC value
#'   \item data: Original input data frame 
#' }
#'
#' @examples
#' data(PIC_example)
#' results <- pic_calc(PIC_example)
#' 
#' @export
pic_calc <- function(data){
  
  # Function body
  
}
pic_calc=function(data){
  pic_multi=function(temp,n){
    s1=(rowSums(temp[,c(-1,-2)])/sum(rowSums(temp[,c(-1,-2)])))^2
    comb=t(combn(n,2))
    s2=(2*s1[comb[,1]]*s1[comb[,2]])
    H=1-sum(s1)
    PIC=H-sum(s2)
    return(c(H=H,PIC=PIC))
  }
  require(tidyr)
  pic=data.frame(Markers=unique(data[,1]),H=NA,PIC=NA)
  index_mark=unique(data[,1])
  for (i in 1:length(unique(data[,1]))) {
    
    temp=data[grep(paste("^",index_mark[i],"$", sep=""),data[,1]),]
    if(nrow(temp)==1){
      temp1=temp[,-c(1,2)]
      fi=rowSums(temp1)/ncol(temp1)
      
      pic[i,2]="DOMINANT MARKER"
      pic[i,3]=2*fi*(1-fi)
    }else{
      x=pic_multi(temp,nrow(temp))
      pic[i,2]=round(x[1],3)
      pic[i,3]=round(x[2],3)
    }
    
  }
return(pic)
}






