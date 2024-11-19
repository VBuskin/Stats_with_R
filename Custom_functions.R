
# Custom functions --------------------------------------------------------

## Sampling ----------------------------------------------------------------

### Function for drawing a random stratified sample from an ICE query

# (1) Requires the following libraries
library(tidyverse)
library(quanteda)
library(sampling)
library(data.table)

# (2) "data": Requires the data frame produced by a kwic() query
# (3) "size": Requires the size the of the stratified sample.

# Definition
stratified_sample_ICE <- function(data, size) {
  # Set seed for reproducibility
  set.seed(1234)
  
  # Required sample size
  sample_size_required <- size
  
  # Seprate the file numbers from the text categories
  verb_data <- separate_wider_delim(data,
                                    cols = docname, 
                                    delim = "-",
                                    names = c("Text_category",
                                              "File_number"))
  
  # Compute proportions in the population
  verb_data_prop <- table(verb_data$Text_category) / length(verb_data$Text_category)
  
  # Compute sizes of the stratified sample
  strat_sample_sizes <- round(sample_size_required * (verb_data_prop))
  
  # Draw the stratified sample
  verb_strat_sample <- tibble(strata(verb_data,
                                     "Text_category",
                                     strat_sample_sizes,
                                     method = "srswor"))
  
  
  # Generate output df
  output_sample <- tibble(getdata(verb_data, verb_strat_sample)) 
  
  # Some post-hoc clean-up
  output_sample %>% 
    dplyr::select(-ID_unit, -Prob, -Stratum) %>% # remove unnecessary columns
    relocate(Text_category) # move some columns around
  
  # Return the desired sample
  return(output_sample)
  
}

