library(ape)

rawData <- read.table("employment_data2.txt", TRUE, "\t")

countries <- rawData$Country
row.names(rawData) <- countries

hclustresult1 <- hclust(dist(rawData), method = "single")
plot(as.phylo(hclustresult1), type='fan', show.node.label = TRUE, font = 2, cex= 0.45)

hclustresult2 <- hclust(dist(rawData), method = "average")
plot(as.phylo(hclustresult2), type='fan', show.node.label = TRUE, font = 2, cex= 0.45)

hclustresult3 <- hclust(dist(rawData), method = "average")
plot(as.phylo(hclustresult3), type='fan', show.node.label = TRUE, font = 2, cex= 0.45)



