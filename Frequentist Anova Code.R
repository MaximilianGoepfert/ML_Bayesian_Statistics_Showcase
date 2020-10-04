###Load the required packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(multcomp)
library(caret)
library(randomForest)
library(e1071)
library(class)

date<-Sys.Date()

### Set Working Directory
setwd("C:\\Users\\...\\MATH501")

###################################################################

## Frequentist One-way Analysis of Variance

### Scenario:
# A farming consortium conducts an experiment to test four types of fertilizer, using 20 fields that have similar soil types and weather conditions.
# Each fertilizer is assigned at random to five fields. 
# A crop is grown and the yield in tonnes per hectare is recorded.

## Create Data:
# Data
yield <- c( 3, 2, 4, 3, 5,
            5, 4, 2, 6, 6,
            7, 6, 4, 6, 4,
            7, 5, 5, 6, 9)
# Assign the corresponding fertilizer

fertilizer <- factor(c(rep(1, 5), 
                       rep(2, 5),
                       rep(3,5),
                       rep(4, 5)))

# Put the data into a data frame

fertilizer_df <- data.frame(yield, fertilizer)

# Assign y[k]
y <- fertilizer_df$yield

# Assign group[k]
group <- fertilizer_df$fertilizer

# Specify precision
options(digits = 5) 
#
fertilizer_df %>% 
  group_by(fertilizer) %>%
  summarize(yield_mean = mean(yield),
            yield_sd = sd(yield))

######

## Part a)

# Using ggplot2 we visualize the data

ggplot(fertilizer_df, aes(x = fertilizer, y = yield, 
                          colour = fertilizer)) +
  geom_boxplot(varwidth = TRUE) +
  geom_jitter(width = 0.05) +
  labs(x = "Fertilizer",
       y = "Yield") +
  stat_summary(fun.y = mean, 
               colour="black", 
               geom = "point", 
               shape = 18, 
               size = 3,
               show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               colour = "black", 
               geom = "text", 
               show.legend = FALSE, 
               vjust = -0.7, 
               aes(label = round(..y.., digits = 3))) +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser Yield")

######

## Part b)

# We perform a frequentist hypothesis test of size 0.05 of whether the underlying average crop yield is different 
# when a different fertilizer is applied.

freq_fertiliser <- lm(yield ~ fertilizer, data = fertilizer_df)

anova(freq_fertiliser)

# Extract the p-value

fertilizer_p <- anova(freq_fertiliser)$`Pr(>F)`[1]

######

## Part c)

# We perform a Follow-up Analysis using Tukey Honest Significant Differences

aov_fertilizer <- aov(yield ~ fertilizer, data = fertilizer_df)

coef(aov_fertilizer)
summary(aov_fertilizer)
model.matrix(aov_fertilizer)

# Perform Tukey HSD
TukeyHSD(aov_fertilizer)

######

## Part d)

# Is the underlying crop yield level $\mu4$ obtained using the fourth fertilizer more than 0.5 units greater 
# than the average of the underlying crop yield levels obtained using the other three fertilizers, $\mu1$, $\mu2$ and $\mu3$ respectively?

# To estimate the parameters $\mu1$, $\mu2$ and $\mu3$, instead of $\mu1$, $\alpha2$ and $\alpha3$:
para_fertilizer <- lm(yield ~ fertilizer - 1 , data = fertilizer_df)

ght_fertilizer <- glht(para_fertilizer, # Parameterized using mu_1, mu_2, mu_3 and mu_4
                       # Tesxted Hypothesis
                       linfct = c("fertilizer2 - fertilizer1 = 0",
                                  # Null Hypotheses
                                  "fertilizer3 - fertilizer1 = 0",
                                  "fertilizer3 - fertilizer2 = 0",
                                  "fertilizer4 - fertilizer1 = 0",
                                  "fertilizer4 - fertilizer2 = 0",
                                  "fertilizer4 - fertilizer3 = 0"))

summary(ght_fertilizer)

# Is $\mu4$ at least 0.5 units greater than the average of the underlying crop yield levels?
ght_mu4 <- glht(para_fertilizer, 
                # State the hypothesis to be tested (null hypothesis):
                linfct = c("fertilizer4  - (fertilizer1 + fertilizer2 + fertilizer3) / 3 <= 0.5"))

summary(ght_mu4)