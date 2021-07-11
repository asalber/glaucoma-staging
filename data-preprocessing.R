# Autor: Alfredo Sánchez Alberca (asalber@ceu.es)
# Project: Glaucoma staging system
# Data preprocessing

# Load packages
.packages <- c("tidyverse", "readxl")
.installed <- .packages %in% installed.packages()
if (length(.packages[!.installed])>0) install.packages(.packages[!.installed])
lapply(.packages, library, character.only=T)

# Load data
df.rims <- read_csv("data/data.csv", col_types = paste0("cfffc",paste0(rep("d",33), collapse = ""))) %>%
  # Convert dates
  mutate(ExamDate = as.Date(ExamDate, format="%m/%d/%Y")) %>%
  # Create key
  mutate(Id = paste(Patient.Id, ExamDate, Eye, sep="-")) %>%
  # Reorder columns
  select(Id, everything())

# Load outliers data frame
df.outliers <- read.csv(file="data/data-confrontation.csv", header=T, sep=",")
id.outliers <- df.outliers[is.na(df.outliers$SUMA.V) | df.outliers$SUMA.V>10, "Lastname"]
# Remove outliers
df.rims <- df.rims %>% 
  filter(!Patient.Id %in% id.outliers) %>%
  # Remove wrong data
  filter(!Id %in% c("2587-2018-02-14-L",  "16091-2016-11-11-R", "1008-2017-07-26-L",  "1796-2016-10-04-L",  "1796-2016-10-04-R"))

# Data standarization
# The width of the retinal nerve fiber layer depens on age and BMO area. W apply standarization regression model given by the OCT company.
std.coef <- read.csv(file="data/standardization-table.csv", encoding = "UTF-8", header=T, row.names=1, sep=",")
age.mean <- 52.17
bmo.area.mean <- 1.781
for (i in colnames(df.rims)[12:ncol(df.rims)]){
  df.rims[[i]] <- (df.rims[[i]]-std.coef[i,"Average"]-std.coef[i,"Slope.age"]*(df.rims$Age-age.mean)-std.coef[i,"Slope.bmo.area"]*(df.rims$BMO.Area-bmo.area.mean))/std.coef[i,"Stdev"]
  # Uncomment next line to work with percentiles
  # datos[[i]] <- pnorm(datos[[i]])*100
}

# Renombrar variables
names(df.rims) <- gsub("Rim", "RNFL", names(df.rims))

# Export data
write_csv(df.rims, file = "data/data-rims-preprocessed.csv")
save(df.rims, file = "data/data-rims-preprocessed.RData")

