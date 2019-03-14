source("data-loading.R")

# Load packages
.packages <- c("reshape2")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)


# Correlation matrix
cormat <- round(cor(data[, varRims], use="complete.obs"),2)
melted.cormat <- melt(cormat)
melted.cormat$Var1 <- factor(melted.cormat$Var1, levels=varSectors)
melted.cormat$Var2 <- factor(melted.cormat$Var2, levels=varSectors)
ggplot(data = melted.cormat, aes(x = Var1, y = Var2, fill = value, label = value)) +
  geom_tile() +
  geom_text(size = 2) +
  xlab('') +
  ylab('') +
  ggtitle("Correlation matrix among BMO and RFNL rims by sectors") +
  labs(fill = "Pearson\ncorrelation r") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), plot.title = element_text(hjust = 0.5))
ggsave("img/correlation-matrix.pdf")
