
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
return(list(pic=pic,data=data))
}






