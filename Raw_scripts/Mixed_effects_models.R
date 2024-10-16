
# Mixed-effects regression ------------------------------------------------

## Preparation -------------------------------------------------------------

# Load libraries
library(tidyverse)
library(broom)
library(broom.mixed)
library(tidyr)
library(lme4) # for linear mixed-effects models
library(sjPlot)
library(ggeffects)

# Load data
varmorph <- read.csv("varmorph_data.csv", header = TRUE)

# Reduce data
varmorph %>%
  select(rt, target, prime_type, subj_id) %>%
  filter(prime_type != "filler") %>% 
  drop_na() -> varmorph2

# Overview
glimpse(varmorph2)

## Application in R --------------------------------------------------------

### Varying-intercept model -------------------------------------------------

# Varying intercept model

# Define reference level for "prime_type"
varmorph2$prime_type <- factor(varmorph2$prime_type, levels = c("unrelated", "derived", "inflected"))

# Fit mixed-effects models
varmorph.me <- lm(rt ~ prime_type, #+ # fixed effect
                      #(1 | subj_id) + # let intercept vary by subject
                      #(1 | target), # # let intercept vary by target word
                    data = varmorph2)

# Summarise results
summary(varmorph.me)

# Print HTML table
tab_model(varmorph.me, show.se = TRUE, show.aic = TRUE, show.dev = TRUE)

# Plot

# Extract random effects and their standard errors
ranef_obj <- ranef(varmorph.me)  # Extract random effects with conditional variance
se_ranef <- arm::se.ranef(varmorph.me)           # Extract standard errors for random effects

# Prepare a data frame for 'subj_id' random intercepts with confidence intervals
subj_ranef <- ranef_obj$subj_id  # Random effects for subjects
subj_se <- se_ranef$subj_id      # Standard errors for subjects

# Combine random effects and standard errors into a data frame
subj_df <- data.frame(
  subj_id = rownames(subj_ranef),
  intercept = subj_ranef[, "(Intercept)"],
  se = subj_se[, "(Intercept)"],
  conf.low = subj_ranef[, "(Intercept)"] - 1.96 * subj_se[, "(Intercept)"],
  conf.high = subj_ranef[, "(Intercept)"] + 1.96 * subj_se[, "(Intercept)"]
)

# Create the waterfall plot
ggplot(subj_df, aes(x = reorder(subj_id, intercept), y = intercept)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey10") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey10") +
  labs(title = "Random Intercepts by Subject",
       x = "Subject ID",
       y = "Random Intercept") +
  theme_minimal()


### Varying-slope model -----------------------------------------------------

# Varying-slope model; replace 0 with 1 if you want the intercept to vary too
varmorph.me2 <- lmer(rt ~ prime_type +
                       (0 + prime_type | subj_id),
                     data = varmorph2)

summary(varmorph.me2)

# Print HTML table
tab_model(varmorph.me2, show.se = TRUE, show.aic = TRUE, show.dev = TRUE)

# Plot

# Extract random slopes
ranef_data1 <- ranef(varmorph.me2)$subj_id

# Extract standard errors
ranef_data1_se <- arm::se.ranef(varmorph.me2)$subj_id

# Convert data into long format
random_effects_df <- ranef_data1 %>%
  as.data.frame() %>% 
  rownames_to_column(var = "subj_id") %>%
  pivot_longer(cols = -subj_id, 
               names_to = "prime_type", 
               values_to = "random_effect")


# Create a data frame for standard errors
se_df <- as.data.frame(ranef_data1_se) %>%
  rownames_to_column(var = "subj_id") %>%
  pivot_longer(cols = -subj_id, 
               names_to = "prime_type", 
               values_to = "se")


# Combine random effects with standard errors
combined_df <- random_effects_df %>%
  left_join(se_df, by = c("subj_id", "prime_type"))

# Calculate confidence intervals
combined_df <- combined_df %>%
  mutate(lower_ci = random_effect - 1.96 * se,
         upper_ci = random_effect + 1.96 * se)


# Dotplots with confidence intervals
ggplot(combined_df, aes(x = subj_id, y = random_effect, col = prime_type)) +
  coord_flip() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey10") +
  facet_wrap(~ prime_type) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(title = "Random Effect of Prime Type by Subject ID",
       x = "Random Slope",
       y = "Subject ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

