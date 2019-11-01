getwd()

fna.data <- "../Downloads/data/WisconsinCancer.csv"
wisc.df <- read.csv("../Downloads/data/WisconsinCancer.csv", row.names = 1)
dim(wisc.df)
length(grep(pattern = "M", wisc.df$diagnosis))
length(grep(pattern = "_mean", colnames(wisc.df)))
wisc.df
head(wisc.df)
wisc.data<- as.matrix(wisc.df[0:nrow(wisc.df), 2:31])
row.names(wisc.data) <- wisc.df$id
head(wisc.data)
diagnosis <- wisc.df$diagnosis
head(diagnosis)
diagnosis
colMeans(wisc.data)
apply(wisc.data, 2, sd)
wisc.pr<- prcomp(wisc.data, scale. = TRUE)
summary(wisc.pr)
plot(wisc.pr$x[,"PC1"], wisc.pr$x[,"PC3"], col = diagnosis, xlab = "PC1", ylab = "PC3")
head(wisc.pr$x)

pr.var <- wisc.pr$sdev^2
head(pr.var)
pve <- pr.var/sum(pr.var)
pve
plot(pve, ylab = "Proportion of variance explained", xlab = "PC", ylim = c(0,1), type = "o")
install.packages("factoextra")
library(factoextra)
fviz_eig(wisc.pr, addlabels = TRUE)
wisc.pr$rotation[,1]
data.scaled <- scale(wisc.data)
data.dist <- dist(data.scaled, method = "euclidean")
wisc.hclust <- hclust(data.dist, method = "complete")
plot(wisc.hclust)
abline(h = 19, col = "red", lty = 2)
distance <- get_dist(data.scaled)
fviz_dist(distance, gradient)
wisc.k2 <- kmeans(wisc.data, centers = 2, nstart = 20)
fviz_cluster(wisc.k2, data = wisc.data)
wisc.k3 <- kmeans(wisc.data, centers = 3, nstart = 30)
fviz_cluster(wisc.k3, data = wisc.data)

wisc.k3 <- kmeans(wisc.data, )