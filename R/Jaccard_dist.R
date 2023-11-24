jaccard_dist_matrix <- function(data) {
  # Extract the genotype names and the marker data
  genotypes <- colnames(data)[-1]
  markers <- data[, -1]

  # Initialize the distance matrix
  n <- ncol(markers)
  dist_matrix <- matrix(0, nrow = n, ncol = n)

  # Calculate the Jaccard distance matrix between genotypes
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      x <- markers[, i]
      y <- markers[, j]
      dist_matrix[i, j] <- 1 - sum(x & y) / sum(x | y)
    }
  }
  dist_matrix[lower.tri(dist_matrix)] <- t(dist_matrix)[lower.tri(dist_matrix)]

  # Set the row and column names of the distance matrix to the genotype names
  rownames(dist_matrix) <- genotypes
  colnames(dist_matrix) <- genotypes

  return(dist_matrix)
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



