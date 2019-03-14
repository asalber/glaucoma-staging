# Load packages
.packages <- c("tidyverse", "reshape2", "FactoMineR", "MASS", "caret", "magrittr")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Data loading
data <- read.csv(file="data/data-preprocessed-clusters.csv", header=T, sep=",")

# Set of variables
varBMO <- startsWith(colnames(data),"BMO")
varBMO[11] <- F
varBMO <- colnames(data)[varBMO]
var3.5 <- startsWith(colnames(data),"Rim3.5")
var3.5 <- colnames(data)[var3.5]
var4.1 <- startsWith(colnames(data),"Rim4.1")
var4.1 <- colnames(data)[var4.1]
var4.7 <- startsWith(colnames(data),"Rim4.7")
var4.7 <- colnames(data)[var4.7]
varG <- endsWith(colnames(data), ".G")
varG <- colnames(data[varG])
varTI <- endsWith(colnames(data), ".TI")
varTI <- colnames(data[varTI])
varT <- endsWith(colnames(data), ".T")
varT <- colnames(data[varT])
varTS <- endsWith(colnames(data), ".TS")
varTS <- colnames(data[varTS])
varNS <- endsWith(colnames(data), ".NS")
varNS <- colnames(data[varNS])
varN <- endsWith(colnames(data), ".N")
varN <- colnames(data[varN])
varNI <- endsWith(colnames(data), ".NI")
varNI <- colnames(data[varNI])
varRims <- c(varBMO, var3.5, var4.1, var4.7)
varSectors <- c(varG, varTI, varT, varTS, varNS, varN, varNI)

classify <- function(data, vars){
  # Principal components
  result.pca <- PCA(data[, vars], scale.unit = F, graph=F) 
  # Add principal components coordinates to a data frame
  data.pca <- as.data.frame(result.pca$ind$coord)
  # Add cluster to principal components data frame
  data.pca$Stage <- data$Stage
  # Linear discriminant analysis (without healthy eyes)
  result.lda.glaucoma <- lda(Stage~., data=data.pca[data.pca$Stage!="Healthy", ], CV=T)
  # Model goodness
  performance.glaucoma <- confusionMatrix(result.lda.glaucoma$class, data.pca[data.pca$Stage!="Healthy","Stage"])
  colnames(performance.glaucoma$byClass) <- c("Sensibilidad", "Especificidad", "VPP", "VPN", "Precisión", "Exhaustividad", "Medida.F", "Prevalencia", "Detection.Rate", "Detection.Prevalence", "Precisión.Global")
  performance.glaucoma$byClass <- performance.glaucoma$byClass[-1, ]
  rownames(performance.glaucoma$byClass) <- gsub("Class:", "Stage", rownames(performance.glaucoma$byClass))
  # Linear discriminant analysis (with healthy eyes)
  result.lda <- lda(Stage~., data=data.pca, CV=T)
  # Model goodness
  performance <-  confusionMatrix(result.lda$class, data.pca$Stage)
  rownames(performance$byClass) <- gsub("Class:", "Stage", rownames(performance$byClass))
  return(list(
    lda.glaucoma = result.lda.glaucoma,
    lda = result.lda,
    performance.glaucoma = performance.glaucoma,
    performance = performance))
}

# Comparison of classification models with different variables
accuracy.table <- data.frame(vars=character(), accuracy1=double(), accuracy2=double())
# All the variables
vars <- "All the variables"
classification <- classify(data, vars=varRims)
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# Clusters basados en todos los sectores globales y temporales inferiores
vars <- "Sectors G and TI of all the rims"
classification <- classify(data, vars=c(varG, varTI))
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en todos los sectores globales
vars <- "Sectors G of all the rims"
classification <- classify(data, vars=varG)
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en todos los sectores temporales inferiores
vars <- "Sectors TI of all the rims"
classification <- classify(data, vars=varTI)
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en los sectores global y temporal inferior de losa Rims BMO y 3.5
vars <- "Sectors G and TI of rims BMO and 3.5"
classification <- classify(data, vars=c("BMO.G", "BMO.TI", "Rim3.5.G", "Rim3.5.TI"))
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en los sectores global y temporal inferior del Rim BMO
vars <- "Sectors G and TI of BMO rim"
classification <- classify(data, vars=c("BMO.G", "BMO.TI"))
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en los sectores global y temporal inferior del Rim 3.5
vars <- "Sectors G and TI of 3.5 rim"
classification <- classify(data, vars=c("Rim3.5.G", "Rim3.5.TI"))
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en los sectores global de los Rims BMO y 3.5
vars <- "Sector G of rims BMO and 3.5"
classification <- classify(data, vars=c("BMO.G", "Rim3.5.G"))
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados en los sectores temporales inferiores de los Rims BMO y 3.5
vars <- "Sector TI of rims BMO and 3.5"
classification <- classify(data, vars=c("BMO.TI", "Rim3.5.TI"))
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados el sector global del Rim BMO
vars <- "Sector G of rim BMO"
classification <- classify(data, vars="BMO.G")
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados el sector global del Rim 3.5
vars <- "Sector G of rim 3.5"
classification <- classify(data, vars="Rim3.5.G")
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados el sector temporal inferior del Rim BMO
vars <- "Sector TI of rim BMO"
classification <- classify(data, vars="BMO.TI")
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])
# classification basados el sector temporal inferior del Rim 3.5
vars <- "Sector TI of rim 3.5"
classification <- classify(data, vars="Rim3.5.TI")
accuracy.table %<>% add_row(vars=vars, accuracy1=classification$performance.glaucoma$overall[1], accuracy2=classification$performance$overall[1])

colnames(accuracy.table) <- c("Variable used in the model", "Overall accuracy without healthy eyes", "Overall accuracy with healthty eyes")

# Means of variables by stages
melted.data <- melt(data[, c("Stage", vars=c(varG, varTI))])
colnames(melted.data)[2] <- "Sector" 
means <- melted.data %>% group_by(Stage, Sector) %>% 
  summarise(n = n(), mean = mean(value, na.rm = T), sd = sd(value, na.rm = T))  %>%
  mutate(se = sd/sqrt(n), lower.ci = mean-qt(1-(0.05/2), n-1)*se, upper.ci = mean+qt(1-(0.05/2), n-1)*se)

# Color palette
colfunc <- colorRampPalette(c("#FD8D3C", "#800026"))
palette <- colfunc(4)
palette.healthy <- c("#00BFC4", palette)

# Plot of means by stages
ggplot(means, aes(x = Sector, y = mean, colour = Stage, shape = Stage)) +
  geom_pointrange(aes(ymin = lower.ci, ymax = upper.ci), position = position_dodge(width = 0.5)) +
  ggtitle("Confidence intervals for the means of G and TI sectors\n of BMO and RNFL rims by glaucoma stages") +
  scale_colour_manual(values = palette.healthy) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5), plot.title = element_text(hjust = 0.5))
ggsave("img/confidence-intervals-G-TI-means-clusters.pdf")
