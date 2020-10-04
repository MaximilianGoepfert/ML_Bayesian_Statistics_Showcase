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

# We Write jags/BUGS code to perform inference about the following related Bayesian one-way Analysis of Variance model:

# \begin{eqnarray*}
# y_i,_j & \sim & N(\mu_i, precision = \tau), \     i = 1,...,4, j = 1,...,5 \\
# \mu_i & = & \mu + \alpha_i, \ i = 1,...4, \\
# \mu & \sim & N(0, precision = 0.0001) \\
# \alpha_1 & = & 0, \\
# \alpha_i & \sim & N(0, precision = 0.0001), i = 2,...4 \\
# \tau & \sim & Gamma(shape = 0.001, rate = 0.001) \mbox,
# \end{eqnarray*}

# standard deviation $\sigma = \frac{1} {\sqrt{\tau}}.$

Bayesian_ANOVA <- function(){
  #
  # Data model part
  #
  for (k in 1:n){
    #
    # Response Variable
    #
    y[k] ~ dnorm(mu[k], tau) # Parameterized by precision
    #
    # Underlying mean
    #
    mu[k] <- m + alpha[group[k]]
    # Note that we use m as mu cannot be used twice for different quantities
  }
  #
  # Specify the prior distributions (or constraint)
  #
  m ~ dnorm(0.0, 1.0E-4)
  #
  alpha[1] <- 0 # Corner constraint
  #
  for (i in 2:I){ # I is the number of groups
    alpha[i] ~ dnorm(0.0, 1.0E-4) # Prior on non-constrained alphas
  }
  tau ~ dgamma(1.0E-3, 1.0E-3) # Prior on tau
  #
  # Also perform inference about sigma
  #
  sigma <- 1.0 / sqrt(tau) # Definition of sigma
  #
  ##
  # Monitor the group means
  #
  mean_Group[1] <- m + alpha[1] # alpha[1] = 0, so not needed
  mean_Group[2] <- m + alpha[2]
  mean_Group[3] <- m + alpha[3]
  mean_Group[4] <- m + alpha[4]
}

# Preparing the data
n <- length(y)

# Number of groups
I <- 4

# All the required data
data_anova <- list("y", "group", "n", "I")

# Performing inference:
require(R2jags)

Bayesian_anova_inference <- jags(data = data_anova,
                                 parameters.to.save = c("alpha",
                                                        "sigma",
                                                        "tau",
                                                        "mean_Group"), # mean_Group[1] = m
                                 n.iter = 100000,
                                 n.chains = 3,
                                 model.file = Bayesian_ANOVA)

# Examining output:
print(Bayesian_anova_inference, intervals = c(0.025, 0.5, 0.975))

######

## Part b)

# We include a graphical representation of the posterior densities of $\alpha_0$, $\alpha_1$, $\alpha_2$, $\alpha_3$ and $\alpha_4$.

require(ggmcmc)

Bayesian_anova_inference.mcmc <- as.mcmc(Bayesian_anova_inference)
Bayesian_anova_inference.ggs <- ggs(Bayesian_anova_inference.mcmc)

ggs_density(Bayesian_anova_inference.ggs, family = "^alpha") + 
  xlim(-10, 10) + 
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser Posterior Densities")

######

## Part c)

# Also include a graphical representation and the numerical values of the 95% credible intervals for the parameters 
# $\alpha_i$ and $\mu_i$, with i = 1,...4.

#alpha
ggs_caterpillar(Bayesian_anova_inference.ggs, family = "^alpha") +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser 95% Credible Intervals - alpha_i ")

#mu
ggs_caterpillar(Bayesian_anova_inference.ggs, family = "^mean_Group") +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser 95% Credible Intervals - mu_i ")

# numerical values:
Bayesian_anova_inference$BUGSoutput$summary[c("mean_Group[1]",
                                              "alpha[2]",
                                              "alpha[3]",
                                              "alpha[4]",
                                              "mean_Group[2]",
                                              "mean_Group[3]",
                                              "mean_Group[4]"),
                                            c("mean",
                                              "50%",
                                              "2.5%",
                                              "97.5%")]

######

## Part d)

# Modify our code to perform posterior inference about the differences between:
# - $\alpha_3$ and $\alpha_1$;
# - $\alpha_3$ and $\alpha_2$;
# - $\alpha_4$ and $\alpha_3$;
# - $\alpha_4$ and $\alpha_1$.

# What is the posterior probability that the underlying crop yield level $\mu_4$ obtained using
# the fourth fertilizer is more than 0.5 units greater than the average of the underlying crop
# yield levels obtained using the other three fertilizers, $\mu_1$, $\mu_2$ and $\mu_3$, respectively? Provide an
# interpretation of your output.

# For this part, we base your inference on one run of a very long chain (a million iterations).
# Modifying model, adding $\delta_i$ as a parameter for measuring the differences we're looking for:
Bayesian_ANOVA_2 <- function(){
  #
  # Data model part
  #
  for (k in 1:n){
    #
    # Response Variable
    #
    y[k] ~ dnorm(mu[k], tau) # Parameterized by precision
    #
    # Underlying mean
    #
    mu[k] <- m + alpha[group[k]]
    # Note that we use m as mu cannot be used twice for different quantities
  }
  #
  # Specify the prior distributions (or constraint)
  #
  m ~ dnorm(0.0, 1.0E-4)
  #
  alpha[1] <- 0 # Corner constraint
  #
  for (i in 2:I){ # I is the number of groups
    alpha[i] ~ dnorm(0.0, 1.0E-4) # Prior on non-constrained alphas
  }
  tau ~ dgamma(1.0E-3, 1.0E-3) # Prior on tau
  #
  # Also perform inference about sigma
  #
  sigma <- 1.0 / sqrt(tau) # Definition of sigma
  #
  ##
  # Monitor the group means
  #
  mean_Group[1] <- m + alpha[1] # alpha[1] = 0, so not needed
  mean_Group[2] <- m + alpha[2]
  mean_Group[3] <- m + alpha[3]
  mean_Group[4] <- m + alpha[4]
  
  
  ### New lines of code ###
  #
  delta[1] <- alpha[3] - alpha[1]
  delta[2] <- alpha[3] - alpha[2]
  delta[3] <- alpha[4] - alpha[3]
  delta[4] <- alpha[4] - alpha[1]
  
  #
  #
  ###
  # Required quantity
  required_quantity <- mean_Group[4] - ((mean_Group[1] + mean_Group[2] + mean_Group[3]) / 3)
}

# Performing inference based on one run of a very long chain:
Bayesian_anova_inference_2 <- jags(data = data_anova,
                                   parameters.to.save = c("required_quantity",
                                                          "delta"),
                                   n.iter = 1000000,
                                   n.chains = 1,
                                   model.file = Bayesian_ANOVA_2)

# Examine output
print(Bayesian_anova_inference_2, intervals = c(0.025, 0.5, 0.975))

Bayesian_anova_inference_2.mcmc <- as.mcmc(Bayesian_anova_inference_2)
Bayesian_anova_inference_2.ggs <- ggs(Bayesian_anova_inference_2.mcmc)

# Set threshold
threshold <- 0.5

sampled_values_required_quantity <- Bayesian_anova_inference_2.mcmc[[1]][,"required_quantity"]
head(sampled_values_required_quantity)

###
# Which values are greater than the threshold
#
head(sampled_values_required_quantity > threshold)

## Graphical examinations:

ggs_traceplot(Bayesian_anova_inference_2.ggs, family = "^required_quantity") +
  geom_hline(yintercept = threshold) +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser 4 Yield Traceplot")

ggs_density(Bayesian_anova_inference_2.ggs, family = "^required_quantity") +
  geom_vline(xintercept = threshold) +
  xlim(-5, 5) +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser 4 Yield Line Plot")

ggs_caterpillar(Bayesian_anova_inference_2.ggs, family = "^required_quantity") +
  geom_vline(xintercept = threshold) +
  xlim(-5, 5) +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser 4 Yield Caterpillar Plot")

## Finding posterior probabilities:

mean(sampled_values_required_quantity > threshold)
post_prob <- mean(sampled_values_required_quantity > threshold) * 100
post_prob2 <- (1 - mean(sampled_values_required_quantity > threshold)) * 100

######

## Part e)

# we now consider the following simpler Bayesian model for the crop yield data and perform inference about its parameters 
# writing appropriate jags/BUGS code.

# \begin{eqnarray*}
# y_i & \sim & N(\mu, precision = \tau), \ i = 1,...n \\
# \mu & \sim & N(0, precision = 0.0001) \\
# \tau & \sim & Gamma(shape = 0.001, rate = 0.001) \mbox,
# \end{eqnarray*}

# standard deviation $\sigma = \frac{1} {\sqrt{\tau}}.$

Simpler_Bayesian_Anova <- function(){
  for (i in 1:n){
    #
    # Data model or likelihood part
    #
    y[i] ~ dnorm(m, tau)
  }
  #
  # Priors
  #
  m ~ dnorm(0.0, 1.0E-4)
  tau ~ dgamma(1.0E-3, 1.0E-3)
  #
  # Definition of sigma: it's completely determied by tau
  #
  sigma <- 1.0 / sqrt(tau)
  #
}

# Prepare data:
n <- length(y)

data_anova_s <- list("y", "n")

# Perform inference:
Simpler_Bayesian_Anova_inference <- jags(data = data_anova_s,
                                         parameters.to.save = c("m",
                                                                "tau",
                                                                "sigma"),
                                         n.iter = 100000, # Number of iterations
                                         n.chains = 3, # Number of times to repeat the sampling
                                         model.file = Simpler_Bayesian_Anova)

# Examining the output:
print(Simpler_Bayesian_Anova_inference, intervals = c(0.025, 0.5, 0.975))

######

## Part f)

# We include graphical representations and the numerical values of the 95% credible intervals

Simpler_Bayesian_Anova_inference.mcmc <- as.mcmc(Simpler_Bayesian_Anova_inference)
Simpler_Bayesian_Anova_inference.ggs <- ggs(Simpler_Bayesian_Anova_inference.mcmc)

ggs_caterpillar(Simpler_Bayesian_Anova_inference.ggs, family = "^m") +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser Credible Intervals - mu")

ggs_caterpillar(Simpler_Bayesian_Anova_inference.ggs, family = "^sigma") +
  theme_wsj() + scale_color_wsj("colors6") +
  theme(legend.position = "bottom", plot.title = element_text(size = 10), legend.title = element_text(size = 8), axis.text = element_text(size = 8)) +
  ggtitle("Fertiliser Credible Intervals - sigma")

## Numerical values:
Simpler_Bayesian_Anova_inference$BUGSoutput$summary[c("m",
                                                      "sigma"),
                                                    c("mean",
                                                      "50%",
                                                      "2.5%",
                                                      "97.5%")]

######

## Part g)

# Comparing performance of full with simpler model

# Comparing DIC (Deviance Information Criterion) as a (penalized badness-of-fit) criterion for model selection:
Simpler_Bayesian_Anova_inference$BUGSoutput$DIC
Bayesian_anova_inference$BUGSoutput$DIC