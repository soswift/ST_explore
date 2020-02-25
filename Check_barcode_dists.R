# check EMP barcode distances
library(ape)
bcodes <- read.dna("Desktop/emp_out", format = "fasta")
bcodes <-bcodes[1:768]
dists <-dist.dna(bcodes, model = "raw")
min(as.matrix(dists))
