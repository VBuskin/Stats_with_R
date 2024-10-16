
# Logistic regression  ------------------------------------------------------

## The pronoun data -----------------------------------------------------------

# Load libraries
library(tidyverse)
library(rms)
library(broom)
library(sjPlot)
library(ggeffects)

# Load data
data_pro <- read.csv("INPUT_pronouns.csv", sep = ",", header = TRUE)

# Inspect data
str(data_pro)
head(data_pro)


## Application in R -----------------------------------------------

# Step 2: Convert to factors and specify reference levels -----------------

# Store "Reference" as factor
data_pro$Reference <- as.factor(data_pro$Reference)

## Specify reference level (the 'unmarked' case)
data_pro$Reference <- relevel(data_pro$Reference, "overt")

## Print levels
levels(data_pro$Reference)

# Store "Register" as factor
data_pro$Register <- as.factor(data_pro$Register)

## Specify reference level
data_pro$Register <- relevel(data_pro$Register, "S1A")

# Store "Variety" as factor
data_pro$Variety <- as.factor(data_pro$Variety)

## Specify reference level
data_pro$Variety <- relevel(data_pro$Variety, "GB")

# Store "Person" as factor
data_pro$Person <- as.factor(data_pro$Person)

## Specify reference level
data_pro$Person <- relevel(data_pro$Person, "3")

# Store "Referentiality" as factor
data_pro$Referentiality <- as.factor(data_pro$Referentiality)

## Specify reference level
data_pro$Referentiality <- relevel(data_pro$Referentiality, "referential")


### Step 3: Fit the model  ---------------------------------------------------------

# With lrm(); requires library("rms")

# Fit interaction model
Reference.lrm <- lrm(Reference ~ Register + Variety + Register:Variety + Person, data = data_pro)

# View model statistics
Reference.lrm

# With (glm); available in base R
# Note the additional "family" argument!
Reference.glm <- glm(Reference ~ Register + Variety + Register:Variety + Person, data = data_pro, family = "binomial")

# View model statistics
summary(Reference.glm)

# Print HTML table
tab_model(Reference.glm, show.se = TRUE, show.aic = TRUE, show.dev = TRUE, show.fstat = TRUE, transform = NULL)

# Stepwise variable selection
drop1(Reference.glm, test = "Chisq") # both Person and the interaction term Register:Variety significantly improve the model (i.e., reduce the deviance and AIC)

### Step 4: Confidence intervals and odds ratios ------------------------------------

# Tidy the model output
tidy_model <- tidy(Reference.glm, conf.int = TRUE)

# Remove intercept, compute odds ratios and their CIs
tidy_model <- tidy_model %>% 
  filter(term != "(Intercept)") %>% 
  mutate(
    odds_ratio = exp(estimate),
    odds.conf.low = exp(conf.low),
    odds.conf.high = exp(conf.high)
  )

### Visualising the model ---------------------------------------------------

# Create the coefficient plot
ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "steelblue") +
  theme_minimal() +
  labs(
    x = "Coefficient Estimate (log-odds)",
    y = "Predictor",
    title = "Coefficient Estimates with Confidence Intervals",
    caption = "*Note that the CIs of singificant predictors do not include 0."
  )

# Plot odds ratios
ggplot(tidy_model, aes(x = exp(estimate), y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = odds.conf.low, xmax = odds.conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "steelblue") +
  theme_minimal() +
  labs(
    x = "Coefficient Estimate (odds ratios)",
    y = "Predictor",
    title = "Odds ratios with Confidence Intervals",
    caption = "*Note that the CIs of singificant predictors do not include 1."
  )

### Plot predictions --------------------------------------------------------

# Use ggeffect() from the ggeffects package
plot(ggeffect(Reference.glm, terms = c("Register"))) + geom_line(col = "steelblue")

plot(ggeffect(Reference.glm, terms = c("Variety"))) + geom_line(col = "steelblue")

plot(ggeffect(Reference.glm, terms = c("Person"))) + geom_line(col = "steelblue")


# Step 7: Further model diagnostics ----------------------------------------

# Set seed for reproducibility
set.seed(123)

# Refit the model with additional settings
Reference.val <- lrm(Reference ~ Register + Variety + Register:Variety + Person, data = data_pro, x = T, y = T)

# Perform 200-fold cross-validation
model.validated <- validate(Reference.val, B = 200)

# Slope optimism should be as low possible!
model.validated

# Variable inflation factors further reveal severe multicollinearity
vif(Reference.lrm)

