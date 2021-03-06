source("data-loading.R")

# Load packages
.packages <- c("reshape2", "FactoMineR", "factoextra", "caret")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Number of clusters
fviz_nbclust(data.glaucoma[, varRims], kmeans, method = "wss") + 
  xlab("Number of clusters") + 
  ylab("Intra-groups sum of squares") + 
  ggtitle("Reduction of intra-groups variability according to the number of clusters")
#ggsave("img/reduction-variability-number-clusters.pdf")

# K-means clusters
seed = 123
# Variables considered
vars = varRims
# Número de clusters
n = 4
labels <- c("I", "II", "III", "IV")
# Color palette
colfunc <- colorRampPalette(c("#FD8D3C", "#800026"))
palette <- colfunc(n)
palette.healthy <- c("#00BFC4", palette)
# Shapes
shapes <- c(1, 3, 15, 17, 16)
# k-means
clusters <- kmeans(data.glaucoma[, vars], centers = n, nstart = 25)
centers <- clusters$centers[order(clusters$centers[,1], decreasing = T),]
clusters <- kmeans(data.glaucoma[, vars], centers = centers)
# Convert the cluster into a factor
clusters$cluster <- as.factor(clusters$cluster)
# Assign labels to factors
levels(clusters$cluster) <- labels
# Add cluster to data frame
data$Stage <- factor("Healthy", levels=c("Healthy", labels))
data[data$Glaucoma=="Y", "Stage"] <- clusters$cluster
# Save data frame with clusters
write.csv(data, file = "data/data-preprocessed-clusters.csv", row.names=FALSE, sep=",")

# Principal components
result.pca <- PCA(data[, sapply(data,is.numeric)], scale.unit = F, graph=F) 
# Add principal components coordinates to a data frame
data.pca <- as.data.frame(result.pca$ind$coord)
# Add cluster to principal components data frame
data.pca$Stage <- data$Stage

# Map of clusters (without healthy eyes)
fviz_pca_ind(
  result.pca,
  geom = c("point"),
  axes = c(1, 2),
  select.ind = list(name = rownames(data[data$Glaucoma == "Y", ])),
  col.ind = data$Stage,
  palette = palette,
  addEllipses = T,
  ellipse.type = "t",
  title = "Map of glaucoma stages on the first two principal components",
  legend.title = "Stage"
) +
  xlab("First principal component") +
  ylab("Second principal component") +
  scale_shape_manual(values = shapes[-1]) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("img/map-glaucoma-stages-whithout-healthy.pdf")

# Map of clusters (with healthy eyes)
fviz_pca_ind(
  result.pca,
  geom = c("point"),
  axes = c(1, 2),
  col.ind = data$Stage,
  palette = palette.healthy,
  addEllipses = T,
  ellipse.type = "t",
  title = "Map of glaucoma stages including healthy on the first two principal components",
  legend.title = "Stage"
) +
  xlab("First principal component") +
  ylab("Second principal component") +
  scale_shape_manual(values = shapes) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("img/map-glaucoma-stages-whith-healthy.pdf")

# Clusters distribution
ggplot(data.pca, aes(x = Dim.1)) +
  geom_density(aes(fill = Stage), colour = I("white"), position = "identity", alpha = .5) + 
  ggtitle("Distribution of the first principal component according to glacuoma stages") +
  xlab("First principal component") +
  scale_fill_manual(values = palette.healthy) +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("img/distribution-glaucoma-stages.pdf")

# Means of variables by clusters
melted.data <- melt(data[,c("Stage",vars)])
colnames(melted.data)[2] <- "Sector" 
means <- melted.data %>% group_by(Stage, Sector) %>% 
  summarise(n = n(), mean = mean(value, na.rm = T), sd = sd(value, na.rm = T))  %>%
  mutate(se = sd/sqrt(n), lower.ci = mean-qt(1-(0.05/2), n-1)*se, upper.ci = mean+qt(1-(0.05/2), n-1)*se)

# Plot of means by clusters
ggplot(means, aes(x = Sector, y = mean, colour = Stage, shape = Stage)) +
  geom_pointrange(aes(ymin = lower.ci, ymax = upper.ci), position = position_dodge(width = 0.5)) +
  ggtitle("Confidence intervals for the means of BMO and RNF rims sectors by stages") +
  scale_colour_manual(values = palette.healthy) +
  scale_shape_manual(values = shapes) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5), plot.title = element_text(hjust = 0.5))
