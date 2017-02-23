# This script creates the sysdata.rda file, that contains the GLI-2012 lookup tables
# used to calculate the L, M, S splines.

lookup <- read.table(file="RLookupTable.csv", header = TRUE, sep = ",")
lookup$f <- reorder(lookup$f, match(lookup$f, unique(lookup$f)))
attr(lookup$f, "scores") <- NULL
names(lookup)[names(lookup)=="sex"] <- "gender"


cat("Creating sysdata.rda...\n")
devtools::use_data(lookup, internal=TRUE)

