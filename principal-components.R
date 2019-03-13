source("data-loading.R")

# Load packages
.packages <- c("FactoMineR", "factoextra")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)


# Principal components healthy and glaucoma eyes
result.pca <- PCA(data[, c(varRims)], scale.unit = F, graph=F)
eigenvalues <- get_eigenvalue(result.pca)
eigenvalues
fviz_eig(result.pca, main = "Explained variability by principal components", ylab = "% of total variability", xlab = "Principal components (dimensions)")
fviz_cos2(result.pca, choice = "var", axes=1, title="Correlation of variables with the first principal component") + ylab("Coef. Determination r²")
fviz_contrib(result.pca, choice="var", axes=1, top = 10)
ggsave("img/explained-variability-principal-components.pdf")

# Add coordinates of principal components to data frame
data.pca <- cbind(result.pca$ind$coord, data[, "Glaucoma", drop=F])
# Plot eyes on the two first principal components
fviz_pca_ind(result.pca, 
             geom = c("point"), 
             axes = c(1,2), 
             habillage = data.pca$Glaucoma, 
             ddEllipses = T, 
             ellipse.type="t", 
             title = "Map of healthy and glaucoma eyes\non the two first principal components") +
  scale_shape_discrete(labels = c("Glaucoma", "Healthy")) + 
  scale_color_discrete(labels= c("Glaucoma", "Healthy")) +
  theme(legend.position="top") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("img/principal-compoments-healthy-glaucoma.pdf")
                                                                                   