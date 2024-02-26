

################################################################################
#         Network and Cluster Analysis on WNV Data
################################################################################


## Loading Functions ------------------------------------------------
lapply("./Functions/Cluster_Network_Clean.R",
       source)

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(coop)
library(sp)

WNV_State_all = read_excel("../paper/WNV_data.xlsx")
# read data as WNV_State_all
WNV_pivot<-WNV_State_all%>%dplyr::select(WNVyear,`State/Province`,logWNVtotal)%>%
  pivot_wider(names_from = `State/Province`,
              values_from = logWNVtotal)%>%
  arrange(WNVyear)%>%
  dplyr::select(-WNVyear)

# calculate correlation analysis
WNV_pivot_all <-cor(WNV_pivot,use = 'pairwise.complete.obs')

#data2 includes abbreviation column
data2 <-as.data.frame(WNV_pivot_all)
data2$abbreviation <- row.names(data2) 

# prepair for the cluster analysis
water.active<-as.data.frame(WNV_pivot_all)

result <- Network_analysisFunc(water.active,0.5,data2)



# plot network analysis
coul2 =c("#FEC0EA","#B5FDB4","#B8F3FF","#FEE3BC")
colrs=coul2[as.numeric(as.factor(V(result[[1]])$community))]

# abbreviations
library(stringr)
abb<-result[[4]]$abbreviation
abb<-str_replace_all(abb,c("Quebec"='QC',"Ontario"="ON","Manitoba"="MB",
                           "Saskatchewan"="SK","Alberta"="AB","British Columbia"="BC"))  

# plot network analysis figure
library(igraph)
plot(result[[1]], 
     mark.groups=result[[2]], 
     mark.col = NA,
     mark.border = NA,
     layout=qgraph.layout.fruchtermanreingold(get.edgelist(result[[1]],names=FALSE),
                                              vcount=vcount(result[[1]]),
                                              area=8*(vcount(result[[1]])^2),
                                              repulse.rad=(vcount(result[[1]])^3.1)),
     vertex.label = abb,
     vertex.label.cex =1,
     vertex.frame.width=3,
     vertex.label.color = "black",
     vertex.color = colrs)

legend(x=0.9, y=0.5, title = 'Cluster \nGroup ',legend=c("C1","C2","C3","C4"),
       col= 'black', pt.bg=coul2, bty = "n", pch=21, pt.cex = 3, cex =2, 
       text.col="black" , horiz = F,y.intersp=0.6)


################################################################################
##  silhoette analysis and modulairy
################################################################################
library(cluster)

#cosine distance
dd = 1-result[[5]]  #1-cosine similarity
dd[is.na(dd)] <- 0
cosine_dist <- as.dist(dd)

silhouette(result[[2]]$membership, dd)
plot(silhouette(result[[2]]$membership, dd),
     col = c("#FEC0EA","#B5FDB4","#B8F3FF","#FEE3BC"))

# calculate modularity
modularity(cluster_louvain(result[[1]]))

