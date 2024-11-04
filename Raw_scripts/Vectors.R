
# 7. Vectors -----------------------------------------------------------------

# Word frequencies I

# Combine words into a character vector "lemma"
lemma <- c("start", "enjoy", "begin", "help")

# Print elements of "lemma"
print(lemma)

# Combine frequencies into a numeric vector "frequency"
frequency <- c(418, 139, 337, 281)

# Print elements of "frequency"
print(frequency)

# Generate a simple barplot
barplot(frequency, names.arg = lemma)

# Generate a more sophisticated barplot
barplot(frequency, names.arg = lemma, 
        main = "Frequency of Lemmas", # title
        xlab = "Lemmas",  # label for x-axis
        ylab = "Frequency", # label for y-axis
        col = "steelblue") # color


# Essential R concepts

lemma[1] # get first element

frequency[3] # get third element

frequency[2:4] # get elements 2 through 4