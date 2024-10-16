

# Ordinal regression ------------------------------------------------------


## Introduction ------------------------------------------------------------

# Load libraries
library(DescTools)
library(tidyverse)
library(ordinal)
library(MASS)
library(sjPlot)
library(effects)
library(ggeffects)
library(ggpubr)


# For additional tests
library(generalhoslem)
library(brant)

# Load data
survey <- read.csv("Glass_2021_survey_processed.csv")

# Inspect dataset
str(survey)
head(survey)

unique(survey$rating)
unique(survey$verb)
unique(survey$freq)


## Application in R --------------------------------------------------------

# Using polr() from the "MASS" library --------------------------------------

# Convert to factor and determine ordering
survey$rating <- factor(survey$rating, ordered = TRUE, levels = c("1", "2", "3", "4", "5"))

# Fit polr model
survey.polr <- polr(rating ~ 
                      freq +
                      routine,
                    data = survey)

# Model summary
summary(survey.polr)

# R-squared and AIC
PseudoR2(survey.polr, c("Nagelkerke", "AIC"))

# Print HTML table
tab_model(survey.polr, show.se = TRUE, show.aic = TRUE, show.dev = TRUE, transform = NULL)


## Testing assumptions and goodness of fit ------------------------------------------------

# Brant test
brant(survey.polr) # p < 0.05 is a violation of the assumption

# Hosmer-Lemeshow test

logitgof(survey$rating, # observed
         fitted(survey.polr), # expected
         ord = TRUE) # respect ordering


# Lipsitz test
lipsitz.test(survey.polr)

# Pulkstenis-Robinson test
pulkrob.chisq(survey.polr, catvars = c("freq", "routine"))


## Visualisation -----------------------------------------------------------


## With the "effects" package
# Routine effect plot
plot(Effect(focal.predictors = c("routine"), mod = survey.polr), rug = FALSE, style="stacked")

## With ggeffects and ggplot2

### Plot freq effect

# Get the ggeffects data
eff <- ggeffects::ggeffect(survey.polr, "freq")

# Convert to a data frame
plot_data <- as.data.frame(eff)

# Ensure the response.level has the desired levels
plot_data$response.level <- factor(plot_data$response.level, 
                                   levels = c("X1", "X2", "X3", "X4", "X5"),
                                   labels = c("1", "2", "3", "4", "5"))

# Create the plot with confidence intervals
p1 <- ggplot(plot_data, aes(x = x, y = predicted, color = response.level, group = response.level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 0.5) +
  scale_color_viridis_d() +
  labs(
    x = "Frequency",
    y = "Predicted Probability",
    color = "Rating"
  ) +
  facet_grid(~ response.level) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

p1

### Plot routine effect


# Get the ggeffects data for "routine"
eff_routine <- ggeffects::ggeffect(survey.polr, "routine")

# Convert to a data frame
plot_data_routine <- as.data.frame(eff_routine)

# Ensure the response.level has the desired levels for "routine"
plot_data_routine$response.level <- factor(plot_data_routine$response.level, 
                                           levels = c("X1", "X2", "X3", "X4", "X5"),
                                           labels = c("1", "2", "3", "4", "5"))

# Create the second plot for "routine"
p2 <- ggplot(plot_data_routine, aes(x = x, y = predicted, color = response.level, group = response.level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 0.5) +
  scale_color_viridis_d() +
  labs(
    x = "Routine",
    y = "Predicted Probability",
    color = "Rating"
  ) +
  facet_grid(~ response.level) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

p2


### Plot interaction effect


# Generate the interaction effects for "freq" and "routine"
eff_interaction <- ggeffect(survey.polr, terms = c("freq", "routine"))

# Convert to a data frame
plot_data_interaction <- as.data.frame(eff_interaction)

# Ensure the response.level has the desired levels
plot_data_interaction$response.level <- factor(plot_data_interaction$response.level, 
                                               levels = c("X1", "X2", "X3", "X4", "X5"),
                                               labels = c("1", "2", "3", "4", "5"))


# Create the interaction plot with facet by 'x' and color by 'response.level'
p_interaction <- ggplot(plot_data_interaction, aes(x = group, y = predicted, color = response.level, group = response.level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, linewidth = 0.5) +
  scale_color_viridis_d() +
  labs(
    title = "Predicted Probabilities for Interaction of Frequency and Routine",
    x = "Routine",
    y = "Predicted Probability",
    color = "Rating"
  ) +
  facet_wrap(~ x, labeller = labeller(x = c("hi" = "High Frequency", "lo" = "Low Frequency"))) +  # Facet by "freq"
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# Display the interaction plot
p_interaction


# Using clm() from the "ordinal" library ---------------------------------------------------------------

# Convert to factor and determine ordering
survey$rating <- factor(survey$rating, ordered = TRUE, levels = c("1", "2", "3", "4", "5"))

# Fit cumulative link model
clm.1 <- ordinal::clm(rating ~ 
                        freq +
                        routine,
                      data = survey, 
                      Hess = TRUE)

# Model summary
summary(clm.1)

# Print HTML table
tab_model(clm.1, show.se = TRUE, show.aic = TRUE, show.dev = TRUE, transform = NULL)


# Mixed-effects ordinal regression ----------------------------------------

# Convert to factor and determine ordering
survey$rating <- factor(survey$rating, ordered = TRUE, levels = c("1", "2", "3", "4", "5"))

# Fit mixed model with random intercepts for "verb" and "ParticipantID"
clm.2 <- ordinal::clmm(rating ~ 
                         freq * routine +
                         (1 | verb) +
                         (1 | ParticipantID),
                       data = survey,
                       Hess = TRUE)

# Model summary
summary(clm.2)

# Print HTML table
tab_model(clm.2, show.se = TRUE, show.aic = TRUE, show.dev = TRUE, transform = NULL)

# Plot random intercepts for "ParticipantID" and "verb"

# Extract random effects
re_verb <- ranef(clm.2)$verb
re_participant <- ranef(clm.2)$ParticipantID

# Create dataframes for random effects
df_verb <- data.frame(verb = rownames(re_verb), re = re_verb[,1])
df_participant <- data.frame(ParticipantID = rownames(re_participant), re = re_participant[,1])

# Get predictions for an average case
pred_avg <- ggpredict(clm.2, terms = c("freq", "routine"))

# Add random effects to predictions
pred_verb <- crossing(pred_avg, df_verb) %>%
  mutate(predicted = predicted + re)

pred_participant <- crossing(pred_avg, df_participant) %>%
  mutate(predicted = predicted + re)

# Create a horizontal dot plot for random effects of participants
p_caterpillar <- ggplot(df_participant, aes(x = re, y = reorder(ParticipantID, re))) +
  geom_point(size = 3, color = "steelblue3") +  # Dots representing the random effects
  labs(title = "Random Effects for Participants", 
       x = "Random Effect Estimate (log odds)", 
       y = "Participant ID") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())  # Removes the gridlines for y-axis

p_caterpillar2 <- ggplot(df_verb, aes(x = re, y = reorder(verb, re))) +
  geom_point(size = 3, color = "steelblue3") +  # Dots representing the random effects
  labs(title = "Random Effects for Verbs", 
       x = "Random Effect Estimate (log odds)", 
       y = "Verb") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())  # Removes the gridlines for y-axis

ggarrange(p_caterpillar, p_caterpillar2, ncol = 2, common.legend = TRUE, legend = "right")


# Generalised Additive Mixed-effects Models (GAMMs) -----------------------

# Load libraries
library(mgcv)
library(itsadug)
library(gratia)

# Convert predictors to factors
survey$ParticipantID <- as.factor(survey$ParticipantID)
survey$verb <- as.factor(survey$verb)

# Fit GAMM
gam1 <- bam(as.numeric(rating) ~ # treated as numeric term
              freq + # linear term
              routine + # linear term
              s(ParticipantID, bs = "re") + # smooth term
              s(verb, bs = "re"), # smooth term
            data = survey, 
            family = ocat(R = 5) # number of ordinal categories
)

# Model summary
summary(gam1)

# Extract the intercepts for plotting
thresh <- gratia::theta(gam1) %>% 
  tibble::as_tibble() %>% 
  setNames(c("threshold"))

# Extract predictions for "routine"
routine_pred <- ggpredict(gam1, terms = "routine")

# Plot predictions
routine_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_point(col = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0), col = "steelblue") +
  geom_hline(data = thresh, aes(yintercept = threshold), linetype = "dashed") +
  theme_minimal()


# Extract random effects for "verb"
verb_pred <- ggpredict(gam1, terms = "verb")

# Plot random effect
verb_pred %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_point(col = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0), col = "steelblue") +
  geom_hline(data = thresh, aes(yintercept = threshold), linetype = "dashed") +
  theme_minimal()


# Extract random effects for "ParticipantID"
subj_pred <- ggpredict(gam1, terms = "ParticipantID")

# Plot random effect
subj_pred |>
  ggplot(aes(x = x, y = predicted)) +
  geom_point(col = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, width = 0), col = "steelblue") +
  #geom_line() +
  geom_hline(data = thresh, aes(yintercept = threshold), linetype = "dashed") +
  theme_minimal()

