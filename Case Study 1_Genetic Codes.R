#original code by neelotpal1112

#### Set Up #####
getwd()
setwd(choose.dir())

#download and install package to deal with genetic codes
install.packages("seqinr")
library(seqinr)

#read fasta file ccrescentus.fa
genome <- read.fasta(choose.files(),seqtype = "DNA", set.attributes = FALSE)

#### Convert the dataset into numerical features ####
string_length <- 300

# m<- matrix(genome[[1]], nrow = length(genome[[1]]) / string_length )
# genome_string <- apply(m,1,paste, collapse = '')
# Intuitively you would think the nrow should be constructed as above, however this gives you a completely different pca space. In this space its very hard to perform the elbow test. 

m<- matrix(genome[[1]], nrow = string_length)
genome_string <- apply(m,2,paste, collapse = '')

genome_string <- as.list(genome_string)

# build out tables
WordCounter <- function(genome_string, word_combo, word_length) { 
  x <- grep(word_combo, substring(genome_string, seq(1, nchar(genome_string)- word_length + 1, word_length ), 
                                  seq(word_length, nchar(genome_string), word_length)))
  y <- length(lengths(x))
  return(y)
  }

TableCreator <- function(word_length, genome_string) {
  #create all permutations
  X <- expand.grid(rep(list(c('a', 't', 'g', 'c')), word_length))
  word_combos <- do.call(paste0, X)
  #create a dataframe and populate it
  freq_table <- matrix(, nrow = length(genome_string), ncol = length(word_combos))
  for(i in 1:length(word_combos)) { 
    freq_table[,i] <- sapply(genome_string,WordCounter,word_combos[i],word_length)
  }
  freq_table <- data.frame(freq_table)
  return(freq_table)
}

freq_table <- TableCreator(3,genome_string)

#### PCA #####
library(stats)

#obtain principal components
pca_genome <- prcomp(freq_table, scale. = TRUE)
#plot PCA 
plot(pca_genome, type = 'line')
#plot data along two main principal components
install.packages("devtools")
library(devtools)
install_github("ggbiplot","vqv")
library(ggbiplot)

biplot_pca <- ggbiplot(pca_genome, choices = 1:2, obs.scale = 1, var.scale = 1, ellipse = TRUE, var.axes = FALSE)
print(biplot_pca)

#### clustering ####
#project data on principle component space 

pred_pca_genome <- predict(pca_genome,freq_table)

# Perform k-means clustering
cluster_num = 7
word_cluster <- kmeans(pred_pca_genome[,1:2], cluster_num, nstart = 25) 
word_groups <- as.factor(word_cluster[[1]])

# Plot clusters in principal component space
biplot_pca <- ggbiplot(pca_genome, choices = 1:2, obs.scale = 1, var.scale = 1, groups = word_groups, var.axes = FALSE)
print(biplot_pca)









