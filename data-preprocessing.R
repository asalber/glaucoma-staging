# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Project: Glaucoma staging system
# Data preprocessing

# Load packages
.packages <- c("readxl", "tidyverse", "magrittr")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, require, character.only=T)

# Load data
data.rims <- read.csv(file="data/data.csv", header=T, sep=",")
# Transform dates
data.rims$ExamDate <- as.Date(data.rims$ExamDate, format="%m/%d/%Y")
# Create key
data.rims %<>% dplyr::mutate(Id=paste(Patient.Id, ExamDate, Eye, sep="-"))
# Arrange columns
data.rims %<>% dplyr::select(Id, everything())
# Remove wrong data
data.rims %<>% filter(!Id %in% c("2587-2018-02-14-L",  "16091-2016-11-11-R", "1008-2017-07-26-L",  "1796-2016-10-04-L",  "1796-2016-10-04-R"))
# Remove big outliers
data.outliers <- read.csv(file="data/data-confrontation.csv", header=T, sep=",")
id.outliers <- data.outliers[is.na(data.outliers$SUMA.V) | data.outliers$SUMA.V>10, "Lastname"]
data.rims %<>% filter(!Patient.Id %in% id.outliers)
# Data standarization
# The width of the retinal nerve fiber layer depens on age and BMO area. W apply standarization regression model given by the OCT company.
std.coef <- read.csv(file="data/standardization-table.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
age.mean <- 52.17
bmo.area.mean <- 1.781
for (i in colnames(data.rims)[12:ncol(data.rims)]){
  data.rims[[i]] <- (data.rims[[i]]-std.coef[i,"Average"]-std.coef[i,"Slope.age"]*(data.rims$Age-age.mean)-std.coef[i,"Slope.bmo.area"]*(data.rims$BMO.Area-bmo.area.mean))/std.coef[i,"Stdev"]
  # Uncomment next line to work with percentiles
  # datos[[i]] <- pnorm(datos[[i]])*100
}

# Export data
write.csv(file="data/data-preprocessed.csv", data.rims, row.names=F)
