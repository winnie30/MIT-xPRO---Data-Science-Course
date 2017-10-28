
setwd(choose.dir())
getwd()

#download and install package to deal with genetic codes
install.packages("seqinr")
library(seqinr)
data <- read.fasta(choose.files(),seqtype = "DNA")
Genome <- data
rm(data)
pc <- prcomp(Genome)
help("seqinr")
help("require")
require("seqinr")
