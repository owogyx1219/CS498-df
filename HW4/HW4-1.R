library(ape)

rawData <- read.table("employment_data2.txt", TRUE, "\t")

countries <- rawData$Country
row.names(rawData) <- countries

hclustresult <- hclust(dist(rawData), method = "average")
plot(as.phylo(hclustresult), type='fan', show.node.label = TRUE, font = 2, cex= 0.45)
#axis(1, at=1:26, labels=countries)
#plot(hc)
