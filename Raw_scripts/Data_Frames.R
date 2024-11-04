
# 8. Data frames ----------------------------------------------------------

# Word frequencies II

# Recreate table from previous unit
lemma <- c("start", "enjoy", "begin", "help")

frequency <- c(418, 139, 337, 281)

# Combine vectors to data frame
data <- data.frame(lemma, frequency)

print(data)

# Essential R concepts

data[1,1] # first row, first column

data[1,] # first row

data[,1] # first column

data$lemma # lemma column

# Filtering

## Rows where frequency is greater than 300
data[data$frequency > 300, ]

## Rows where lemma is "start"
data[data$lemma == "start", ]

## Rows where lemma is "start" or "help"
data[data$lemma == "start" | data$lemma == "help", ]

# Some alternatives

## subset()
subset(data, frequency > 300)

subset(data, lemma == "start" | lemma == "help")

## tidyverse

library(tidyverse)

### Generate tidyverse-style data frame
data2 <- tibble(
  lemma = c("start", "enjoy", "begin", "help"),
  frequency = c(418, 139, 337, 281)
)

### Print contents
print(data2)

### Select columns
select(data2, lemma)

### Filter rows 
filter(data2, frequency > 300)

filter(data2, lemma == "start" | lemma == "help")


