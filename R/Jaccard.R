#' Generate dendrogram with Jaccard distance and clusters
#'
#' This function calculates the Jaccard distance matrix, clusters the data 
#' using hierarchical clustering, cuts the dendrogram into a specified  
#' number of clusters, and generates a ggtree fan dendrogram colored by  
#' cluster.
#'
#' @param data Data frame containing the SSR marker data  
#' @param `nclust_jaccard` Number of clusters to divide the dendrogram into
#' @param `offset_label` Offset factor for label positioning in ggtree plot
#'
#' @return A list with the following elements:  
#' \itemize{
#'  \item Jaccard distance matrix
#'  \item Jaccard distance matrix as gt table  
#'  \item Cluster assignments from cutree
#'  \item Cluster assignments as gt table  
#'  \item ggtree plot colored by cluster
#' }
#' 
#' @examples
#'
#' library(pbgeno)
#'   
#' data(Jaccard_example)  
#'   
#' ggtree_out <- ggtree_jaccard(Jaccard_example, nclust_jaccard = 2)
#'  
#' @export

ggtree_jaccard <- function(data, nclust_jaccard = 2, offset_label = 0.2){
  
  # Function body
  
}

ggtree_jaccard=function(data, nclust_jaccard=2,offset_label=0.2){
  
  require(ggtreeExtra)
  require(ggtree)
  require(cluster)
  require(ggplot2)
  require(RColorBrewer)
  require(factoextra)
  require(ape)
  require(gt)
  
  res=list()
  #Distance Matrix
  x=jaccard_dist_matrix(data)
  x1=as.data.frame(x)
  x1=x1%>%gt(rownames_to_stub = T)%>%
    tab_header(
      title = md("**Distance Matrix**"),
      subtitle = "Package used : PB-Perfect")%>%
    tab_source_note(source_note = "Source: Jaccard Distances- Data analysis from PB-Perfect")%>%
    tab_options(
      heading.subtitle.font.size = 12,
      heading.align = "left",
      table.border.top.color = "red",
      column_labels.border.bottom.color = "red",
      column_labels.border.bottom.width= px(3)
    )%>%opt_stylize(style = 6, color = "cyan")%>%
    tab_options(table.width = pct(80))
  
  res[[1]]=x #Jaccard dist
  res[[2]]=x1  #Jaccard dist as gt
  
  
  #Clusters
  x=as.dist(x)
  hc=hclust(x, method = "ward.D2")
  
  x=cutree(hc,nclust_jaccard)
  x=as.data.frame(x)
  colnames(x)="Clusters"
  
  x1=x%>%gt(rownames_to_stub = T)%>%
    tab_header(
      title = md("**Clusters**"),
      subtitle = "Package used : agricolae; PB-Perfect")%>%
    tab_source_note(source_note = "Source: Jaccard Distances- Data analysis from PB-Perfect")%>%
    tab_options(
      heading.subtitle.font.size = 12,
      heading.align = "left",
      table.border.top.color = "red",
      column_labels.border.bottom.color = "red",
      column_labels.border.bottom.width= px(3)
    )%>%opt_stylize(style = 6, color = "cyan")%>%
    tab_options(table.width = pct(80))
  
  res[[3]]=x
  res[[4]]=x1
  
  #Plotting the dendrogram
  hc=as.phylo(hc)
  p <- ggtree(hc,layout = "fan",open.angle=20)
  groups=matrix(NA, nrow = nrow(p$data), ncol = 2)
  colnames(groups)=c("Genotypes","Clusters")
  groups=as.data.frame(groups)
  groups$Genotypes=p$data$label
  groups[1:nrow(x),2]=x[,1]
  p$data$Clusters=as.factor(groups$Clusters)
  #Colouring the cluster
  p=p+geom_tippoint(aes(color=Clusters,fill=Clusters))+
    geom_tiplab(aes(color=Clusters,fill=Clusters),offset  = offset_label,show.legend=F)
  
  res[[5]]=p
  
  return(res)
}