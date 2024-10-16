
# Principal Components Analysis ---------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(purrr)
library(lattice)
library(corrplot)
library(psych)
library(GPArotation)
library(gridExtra)

# Load data
scope_sem_df <- readRDS("scope_sem.RDS")

# Select subset
scope_sem_sub <- scope_sem_df[,1:11]

# Overview
glimpse(scope_sem_sub)


## Descriptive overview ----------------------------------------------------

# Check correlation between number of senses and concreteness
cor(scope_sem_sub[,-1]$Nsenses_WordNet, scope_sem_sub[,-1]$Conc_Brys) # low

# Check correlation between haptic experience and concreteness
cor(scope_sem_sub[,-1]$Haptic_Lanc, scope_sem_sub[,-1]$Conc_Brys) # high


# Generate correlation matrix
cor_mat1 <- cor(scope_sem_sub[,-1])

head(cor_mat1)

# Plot correlation matrix
corrplot(cor_mat1, col = topo.colors(200), tl.col = "darkgrey", number.cex = 0.5, tl.cex = 0.5)

# Levelplot
seq1 <- seq(-1, 1, by = 0.01)

levelplot(cor_mat1, aspect = "fill", col.regions = topo.colors(length(seq1)),
          at = seq1, scales = list(x = list(rot = 45)),
          xlab = "", ylab = "")



## Application in R --------------------------------------------------------

# Fit initial PCA
pca1 <- principal(scope_sem_sub[,-1],
                  nfactors = ncol(scope_sem_sub[,-1]),
                  rotate = "none")

# Print loadings
loadings(pca1)

# Scree plot
barplot(pca1$values, main = "Scree plot", ylab = "Variances", xlab = "PC", # first three PCs
        names.arg = 1:length(pca1$values))
abline(h = 1, col = "blue", lty = "dotted")

# Parallel analysis

pca.pa <- fa.parallel(scope_sem_sub[,-1], # raw data
                      fa = "pc", # Use PCA instead of factor analysis
                      cor = "cor",  # Use Pearson correlations (default for PCA)
                      n.iter = 200, # Number of iterations (increase for more stable results)
                      quant = 0.95, # Use 95th percentile (common choice)
                      fm = "minres") # Factor method


### Accessing and visualising the loadings ----------------------------------

pca2 <- principal(scope_sem_sub[,-1],
                  nfactors = 3,
                  rotate = "none")

loadings(pca2)

# Path diagram
diagram(pca2, main = NA)

# Scatterplot
plot(pca2, labels = colnames(scope_sem_sub[,-1]), main = NA)

# Scores for further use
head(pca2$scores, n = 15)

## Biplots

# PC1 and PC2
biplot(pca2, choose = c(1, 2), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))


# PC2 and PC3
biplot(pca2, choose = c(2, 3), 
       main = NA, pch = 20, col = c("darkgrey", "blue"))
