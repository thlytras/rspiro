# This script creates the sysdata.rda file, that contains the GLI-2012 lookup tables
# used to calculate the L, M, S splines.

lookup <- read.table(file="RLookupTable.csv", header = TRUE, sep = ",")
lookup$f <- reorder(lookup$f, match(lookup$f, unique(lookup$f)))
attr(lookup$f, "scores") <- NULL
names(lookup)[names(lookup)=="sex"] <- "gender"

# Load the NHANES data:
NHtb45 <- read.table(file="NHtb45.csv", header = TRUE, sep = ",")
NHtb6 <- read.table(file="NHtb6.csv", header = TRUE, sep = ",")

cat("Creating sysdata.rda...\n")
usethis::use_data(NHtb45, NHtb6, lookup, internal=TRUE)

