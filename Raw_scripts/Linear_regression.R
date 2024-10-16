
# Linear regression  ------------------------------------------------------

## The ELP data -----------------------------------------------------------

# Load libraries
library(readxl)
library(tidyverse)
library(broom)
library(effects) # a little old-fashioned
library(ggeffects) # the more recent ggplot2 equivalent
library(car)

# Load file
ELP <- read_xlsx("ELP.xlsx")

# Inspect file structure
str(ELP)

# Log-transformed
ggplot(ELP, aes(x = log(RT))) +
  geom_histogram() +
  geom_vline(xintercept = mean(log(ELP$RT)), color = "steelblue") +
  geom_vline(xintercept = mean(log(ELP$RT)), color = "red") +
  theme_minimal() +
  labs(
    x = "Reaction time (log)"
  ) +
  annotate("text", x = log(mean(ELP$RT)), y = 0, 
           label = "mean", 
           vjust = 1.5, hjust = -0.2, color = "steelblue", parse = TRUE) +
  annotate("text", x = log(mean(ELP$RT)) + -0.1, y = .7, 
           label = "median", 
           vjust = 1.5, hjust = -0.2, color = "red", parse = TRUE)


# Skewed
ggplot(ELP, aes(x = RT)) +
  geom_histogram() +
  geom_vline(xintercept = mean(ELP$RT), color = "steelblue") +
  geom_vline(xintercept = median(ELP$RT), color = "red") +
  theme_minimal() +
  labs(
    x = "Reaction time"
  ) +
  annotate("text", x = mean(ELP$RT) + 10, y = 0, 
           label = "mean", 
           vjust = 1.5, hjust = -0.2, color = "steelblue", parse = TRUE) +
  annotate("text", x = median(ELP$RT) -175, y = .7, 
           label = "median", 
           vjust = 1.5, hjust = -0.2, color = "red", parse = TRUE)


# Reaction time depending word frequency
ggplot(ELP, aes(x = log(Freq), y = log(RT))) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Log-transformed word frequency",
    y = "Log-transformed reaction time"
  )

## Simple linear model with a single predictor -----------------------------------------------

# Fit linear model
rt.lm1 = lm(log(RT) ~ log(Freq), data = ELP)

# View model data
summary(rt.lm1)

# Print HTML table
tab_model(rt.lm1, show.se = TRUE, show.aic = TRUE, show.dev = TRUE, show.fstat = TRUE, digits = 3)


# Show fitted values (= predictions) for the first six observations
head(rt.lm1$fitted.values)

# Show deviation of the fitted values from the observed values
head(rt.lm1$residuals)

# Convert coefficients to a tibble (= tidyverse-style data frame)
tidy_model <- tidy(rt.lm1)

tidy_model

# Compute confidence intervals for intercept and log(Freq)
tidy_model_ci <- tidy(rt.lm1, conf.int = TRUE)

tidy_model_ci


## Multiple linear regression ----------------------------------------------

# Fit multiple regression model
rt.lm2 <- lm(log(RT) ~ log(Freq) + POS + Length, data = ELP)

# View model statistics
summary(rt.lm2)

# Print HTML table
tab_model(rt.lm2, show.se = TRUE, show.aic = TRUE, show.dev = TRUE, show.fstat = TRUE, digits = 3)

# Tidy the model output
tidy_model <- tidy(rt.lm2, conf.int = TRUE)

# Remove intercept
tidy_model <- tidy_model %>% filter(term != "(Intercept)")

# Create the coefficient plot
ggplot(tidy_model, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "steelblue") +
  theme_minimal() +
  labs(
    x = "Coefficient Estimate",
    y = "Predictor",
    title = "Coefficient Estimates with Confidence Intervals"
  )

# Plot marginal effects
plot(Effect("Freq", mod = rt.lm2))
plot(Effect("POS", mod = rt.lm2))
plot(Effect("Length", mod = rt.lm2))


## Plotting predictions ----------------------------------------------------

plot(ggeffect(rt.lm2, "Freq"), residuals = TRUE) + geom_line(col = "steelblue") + labs(subtitle = "Untransformed frequencies", y = "log(RT)")

plot(ggeffect(rt.lm2, "Freq [log]"), residuals = TRUE) + geom_line(col = "steelblue") + labs(subtitle = "Log-transformed frequencies", y = "log(RT)", x = "log(RT)")

plot(ggeffect(rt.lm2, "POS"), residuals = TRUE) + geom_line(col = "steelblue") + labs(y = "log(RT)")

plot(ggeffect(rt.lm2, "Length"), residuals = TRUE) + geom_line(col = "steelblue") + labs(y = "log(RT)")


## Model assumptions and diagnostics ---------------------------------------

### Inspect residuals-------------------------------------------------------------------------

# pink line = main tendency vs. blue line = slope coefficients;
# some minor non-linearity can be observed

crPlot(rt.lm2, var = "log(Freq)") 
crPlot(rt.lm2, var = "POS")
crPlot(rt.lm2, var = "Length")


### Heteroscedasticity -------------------------------------------------------------------------

plot(rt.lm2, which = 1)

ncvTest(rt.lm2) # significant, meaning that errors do not vary constantly


### Multicollinearity -------------------------------------------------------

vif(rt.lm2) # vif < 5 indicates that predictors are not correlated


### Normal distribution of residuals ----------------------------------------

qqnorm(rt.lm2$residuals) # or

plot(rt.lm2, which = 2)

shapiro.test(residuals(rt.lm2)) # residuals are not normally distributed because p < 0.05


### Outliers ----------------------------------------------------------------

influencePlot(rt.lm2, id.method = "identify")

# Studentized residuals show differences between observed and predicted values; check observations with values greater than 2 and lower than -2.

# Hat-Values indicate the potential impact of a data point on the coefficient estimates.

# The bubbles represent Cook's D(istances). They represent the effect of an observation on the regression estimates if it is removed from the data. The influence is stronger for higher Cook's distances.

# e.g.,

ELP[452,]

ELP[498,]

ELP[660,]


### Interactions ------------------------------------------------------------

# Fit model with an interaction term "Advertising:Price"

rt.lm.int <- lm(log(RT) ~ log(Freq) + POS + Length + log(Freq):Length, data = ELP)

summary(rt.lm.int)

# ANOVA (analysis of variance)

## Compare interaction model with main effects model

anova(rt.lm.int, rt.lm2) # interaction term improves the model


### Overfitting -------------------------------------------------------------

library("rms")

# Refit the model with ols()

ols.rt <- ols(log(RT) ~ log(Freq) + POS + Length, data = ELP, x = TRUE, y = TRUE)

ols.val <- validate(ols.rt, bw = TRUE, B = 200) # Perform 200 random resampling iterations (= bootstrapping); compare model performance on training vs. test (= new) data. The slope optimism should be below 0.05 to rule out overfitting.

ols.val[,1:5] # The model does not overfit.
