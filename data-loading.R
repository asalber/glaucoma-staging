# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Glaucoma staging system 
# Data preprocessing

# Load packages
.packages <- c("readxl", "tidyverse", "ggpubr")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, library, character.only=T)

# Data loading (see file data-preprocessing.R)
data <- read.csv(file="data/data-preprocessed.csv", header=T, sep=",")
# Convert some columns to factors
data$Gender <- as.factor(data$Gender)
data$Eye <- as.factor(data$Eye)
data$Glaucoma <- as.factor(data$Glaucoma)

# Set of variables
varBMO <- startsWith(colnames(data),"BMO")
varBMO[11] <- F
varBMO <- colnames(data)[varBMO]
var3.5 <- startsWith(colnames(data),"RNFL3.5")
var3.5 <- colnames(data)[var3.5]
var4.1 <- startsWith(colnames(data),"RNFL4.1")
var4.1 <- colnames(data)[var4.1]
var4.7 <- startsWith(colnames(data),"RNFL4.7")
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

# Variables selection
data <- dplyr::select(data, c("Id", "Age", "BMO.Area", "Eye", "Glaucoma", all_of(varRims)))
# Select complete cases
data <- data[complete.cases(data), ]

# Filter left eyes
data <- filter(data, Eye=="L")
# Data frame of glaucoma eyes
data.glaucoma <- data[data$Glaucoma == "Y", ]

