
#https://www.youtube.com/watch?v=0xsM0MbRPGE

# library(igraph)

## Social network analysis

library(igraph)
library(data.table)

dirNetworkCollaboration<-
  "C:/Users/Angelica/OneDrive/Documentos/FINAC"

dirData<-paste0(dirNetworkCollaboration,"/social Networks")


g<-graph(c(1,2))
plot(g,vertex.color="green",vertex.size=40,edge.color="red")

g<-graph(c(1,2,2,3,3,4,4,1),directed=F,n=7)
plot(g,vertex.color="green",vertex.size=20,edge.color="red")

g[]

g1<-graph(c("Amy","Ram","Ram","Li","Li","Amy","Amy","Li","Kate","Li"))
plot(g1,vertex.color="green",vertex.size=20,edge.color="red")
g1
#example to understand arrow directions: Kate follows Lee

g1u<-graph(c("Amy","Ram","Ram","Li","Li","Amy","Amy","Li","Kate","Li"),directed=F)
plot(g1u,vertex.color="green",vertex.size=20,edge.color="red")
g1u



# Network measures

## degree: number of connections
degree(g1)
degree(g1,mode="all")
degree(g1,mode="in")
degree(g1,mode="out")

## diameter: length of longest geodesic
diameter(g1,directed=F,weights = NA)
diameter(g1,directed=T,weights = NA)

##edge_density: number of edges / possible edges
edge_density(g1, loops=F)
ecount(g1)/(vcount(g1)*(vcount(g1)-1))
ecount(g1)
vcount(g1)

edge_density(g1, loops=T)


##reciprocity: reciprocity of a directed graph. Proportion of mutual connections in a directed graph
reciprocity(g1)

##closeness: steps required to access every other vertex
closeness(g1,mode="all",weights=NA,normalized = T)


##betweenness(centrality): number of geodesics (shortest paths) going through a vertex or an edge
betweenness(g1,directed=T,weights = NA)
edge.betweenness(g1,directed=T,weights = NA)
?betweenness

# read data file
data<-setDT(read.csv(paste0(dirData,"/networkdata.csv"),header=T))
colnames(data)
head(data)
y<-data[,.(first,second)]

net<-graph.data.frame(y,directed = T)

V(net)
E(net)
V(net)$label<-V(net)$name
V(net)$degree<-degree(net)

# histogram of degree
hist(V(net)$degree,col='green',main='Histogram of Node Degree',ylab='Frequency',xlab='Degree of Vertices')

# network diagram
set.seed(222)
plot(net)
plot(net,vertex.color='green',vertex.size=2,vertex.label.dist=1.5,edge.arrow.size=0.1,vertex.label.cex=0.8)
plot(net,vertex.color="green",vertext.size=2,edge.arrow.size=0.1,vertex.label.cex=0.8)

# highlight, degrees and layout
plot(net,vertex.color=rainbow(52),vertex.size=V(net)$degree*0.4,edge.arrow.size=0.1,layout=layout.fruchterman.reingold)
plot(net,vertex.color=rainbow(52),vertex.size=V(net)$degree*0.4,edge.arrow.size=0.1,layout=layout.graphopt)
plot(net,vertex.color=rainbow(52),vertex.size=V(net)$degree*0.4,edge.arrow.size=0.1,layout=layout.kamada.kawai)

# hub and authorities
hs<-hub_score(net)$vector
as <-authority.score(net)$vector

par(mfrow=c(1,2))
set.seed(123)
plot(net,vertex.size=hs*30,main="Hubs",vertex.color=rainbow(52),edge.arrow.size=0.1,layout=layout.kamada.kawai)

plot(net,vertex.size=as*30,main="Authorities",vertex.color=rainbow(52),edge.arrow.size=0.1,layout=layout.kamada.kawai)

par(mfrow=c(1,1))

# community detection
net<-graph.data.frame(y,directed=F)
cnet<-cluster_edge_betweenness(net)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.fruchterman.reingold)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.graphopt)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.kamada.kawai)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.circle)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.davidson.harel)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.drl)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.fruchterman.reingold.grid)
plot(cnet,net,vertex.size=10,veretx.label.cex=0.8,layout=layout.grid)




