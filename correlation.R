source("data-loading.R")

# Load packages
.packages <- c("GGally")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)


# Correlation matrix
ggcorr(data[, varSectors], hjust = 0.9, size = 3, layout.exp = 2, label = TRUE, label_size = 2, label_round = 2, low = "darkred", high = "#0072B2") +
  ggtitle("Correlation matrix among BMO and RFNL rims by sectors") +
  guides(fill = guide_colourbar(title="Pearson\ncorrelation r")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), plot.title = element_text(hjust = 0.5))
# Save plot  
ggsave("img/correlation-matrix.pdf")
