# This script creates the sysdata.rda file, that includes the GLI lookup tables
# used to calculate the L, M, S splines.


# GLI-2012
lookup <- read.table(file="RLookupTable.csv", header = TRUE, sep = ",")
lookup$f <- reorder(lookup$f, match(lookup$f, unique(lookup$f)))
attr(lookup$f, "scores") <- NULL
names(lookup)[names(lookup)=="sex"] <- "gender"


# GLI global (2022) equations
library(readxl)
GLIgl_lookup <- lapply(1:6, function(i) as.data.frame(read_excel("gli_global_lookuptables_dec6.xlsx", i)[,1:4]))
for (i in 1:length(GLIgl_lookup)) names(GLIgl_lookup[[i]]) <- c("agebound", "Mspline", "Sspline", "Lspline")
l <- unique(sapply(GLIgl_lookup, nrow))
GLIgl_lookup <- do.call(rbind, GLIgl_lookup)
GLIgl_lookup$gender <- rep(1:2, each=l*3)  # 1 = Male, 2 = Female
GLIgl_lookup$f <- rep(c("FEV1", "FVC", "FEV1FVC"), each=l)


# Load the NHANES data:
NHtb45 <- read.table(file="NHtb45.csv", header = TRUE, sep = ",")
NHtb6 <- read.table(file="NHtb6.csv", header = TRUE, sep = ",")


cat("Creating sysdata.rda...\n")
usethis::use_data(NHtb45, NHtb6, lookup, GLIgl_lookup, internal=TRUE)

