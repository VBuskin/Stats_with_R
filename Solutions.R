

# 7. Vectors  ----------------------------------------------------------------

# 1. Create a vector that lists the third person personal pronouns of
# English (subject and object forms). Store them in a variable `pp3`.

pp3 <- c("he", "she", "it", "him", "her", "they", "them")


# 2.  Now print ...

# ... the fourth element in `pp3`.

print(pp3[4]) # or simply

pp3[4]

# ... elements 3 through 5.

pp3[3:5]

# ... all elements.

pp3

# ... elements 1, 3 **and** 5.

pp3[c(1, 3, 5)]


# 3.  When working with large datasets, we often don't know whether an
  #  element is in the vector to begin with, let alone its position. For
  # instance, if we wanted to check whether *they* is in `pp3` or not,
  #  we could use the handy notation below, returning a `TRUE` or `FALSE`
  # value:

"they" %in% pp3

# Ascertain whether the following items are in `pp3`:

## "him"

"him" %in% pp3 # TRUE

# "you"

"you" %in% pp3 # FALSE

# "it" and "them"

c("it", "them") %in% pp3 # TRUE TRUE

# "we", "us", and "me"

c("we", "us", "them") %in% pp3 # FALSE FALSE TRUE


# 4. Once we are sure that an element is in the vector of interest,
  # another common problem that arises is finding its location. In this case,   # we can use `which()` to return the index of an element

which(pp3 == "they") # Note the two equal signs == !

# You can read this notation as "Which element in `pp3` is *they*?". The
# output suggests that is in position `6`. Note that the number obtained
# depends on the order of elements you've chosen when creating `pp3`.

# Find the locations of *it* and *them* in `pp3`!

# "him"
which(pp3 == "it")

# "you"
which(pp3 == "them")

# 5. Consider the vector "numbers".

numbers <- c(500:1000)

# What does the following code do? How does the output change when you subset `numbers` according to this expression?

which(numbers > 600) # returns indices (= positions) of all elements that are greater than 600; subsetting returns the actual elements

numbers[numbers != 500] # all numbers that are not equal to 500

numbers[numbers > 500 & numbers < 550] # all numbers that are greater than 500 AND (&) less than 550

numbers[numbers < 510 | numbers > 990] # all numbers that are less than 510 OR (|) greater than 990


# 8. Data frames -------------------------------------------------------------



# 10. Import/export data ------------------------------------------------------



# 12. Regular expressions -----------------------------------------------------



# 15. Categorical data --------------------------------------------------------



# 16. Continuous data ---------------------------------------------------------



