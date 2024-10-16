
# Exploratory factor analysis ---------------------------------------------

# Load libraries
library(tidyverse)
library(purrr)
library(psych)
library(GPArotation)
library(gridExtra)

# Load data
scope_sem_df <- readRDS("scope_sem.RDS")

# Select subset
scope_sem_sub <- scope_sem_df[,1:11]

# Overview
glimpse(scope_sem_sub)


## Application in R --------------------------------------------------------

# Perform EFA
efa1 <- fa(scope_sem_sub[,-1], nfactors = 3, rotate = "none", fm = "pa")

# Print loadings
loadings(efa1)

# Plot loadings
plot(efa1, labels = colnames(scope_sem_sub[,-1]), main = NA)

# Biplots (scores + loadings)
biplot(efa1, choose = c(1, 2), main = NA,
       pch = 20, col = c("darkgrey", "blue"))


biplot(efa1, choose = c(2, 3), main = NA,
       pch = 20, col = c("darkgrey", "blue"))


## Rotation ----------------------------------------------------------------

# Perform EFA with Varimax rotation
efa2 <- fa(scope_sem_sub[,-1], nfactors = 3, rotate = "Varimax", fm = "pa")

# Print new loadings
loadings(efa2)

# Path diagram
diagram(efa2, main = NA)

## Biplots

biplot(efa2, choose = c(1, 2), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

biplot(efa2, choose = c(2, 3), main = NA,
       pch = 20, col = c("darkgrey", "blue"))

