library(readxl)
library(tidyr)
library(readr)
library(brms)
library(ggplot2)
library(tidybayes)
library(bayesplot)
library(here)

# load data
bayes_data <- read_excel("/Users/neu/Documents/Bücher:Pdf/Uni/Psy/Master/Semester 4/Masterarbeit/MA Data/andrei/nfb analysis skripten angepasst/a0_prepare_data/nfb_3.3_wide_std_base.xlsx")

feedback_std_base_df <- bayes_data[, 0:400] # get feedback data only
feedback_std_base_df[is.na(feedback_std_base_df)] <- 0 # replace NAs with 0
feedback_std_base_mean_df <- apply(feedback_std_base_df, 1, mean)

feedback_std_base_df_mean <- read_csv('/Users/neu/Documents/R - Dateien Saves/feedback_std_mean_df.csv')

x_bayes <- feedback_std_base_df_mean[, 1:1]
y_bayes <- (feedback_std_base_df_mean[, 2:2])

dat1_bayes <- data.frame(x_bayes, y_bayes)

names(dat1_bayes)[1] <- "TTime_in_min"
names(dat1_bayes)[2] <- "NF"

dat1_bayes_plot <- ggplot(data = dat1_bayes, aes(x = TTime_in_min, y = NF)) + 
  geom_point()

predictive_checks <- function(m) {
  # Plot functions for pp checks and title
  p <- plot(conditional_effects(m, conditions = cond_nf,
                                method = "predict"), points = TRUE, ylim = c(-1, 1))
  
  return(p)
}

f0 <- as.formula(NF ~ TTime_in_min) #linear model; doesn't need specific priors in brms
f1 <- as.formula(NF ~ -((k*a)+(1-(k*a))*b*(1+TTime_in_min)^(-c))) # saturation function 1 (power function from forgetting curve with parameter k)
f2 <- as.formula(NF ~ -((a+(1-a))*b*(1+TTime_in_min)^(-c))) # power function without k
f3 <- as.formula(NF ~ -(k*a+(1-(k*a))*b*exp(-d*(TTime_in_min)))) # exponential function with parameter k
f4 <- as.formula(NF ~ -((k*0.116)+(1-(k*0.116))*b*((1+(g*TTime_in_min))^(-c)))) #  function with "a" as constant and parameter k


# priors

prior1 <- prior(beta(2, 2), nlpar = "a", lb = 0, ub = 1) + # c = beta parameter
  prior(beta(2, 2), nlpar = "b", lb = 0, ub = 1) +
  prior(beta(5, 1), nlpar = "c", lb = 0, ub = 1) +
  prior(normal(-3, 0.1), nlpar = "k")

prior2 <- prior(beta(2, 2), nlpar = "a", lb = 0, ub = 1) + 
  prior(beta(2, 2), nlpar = "b", lb = 0, ub = 1) +
  prior(beta(5, 1), nlpar = "c", lb = 0, ub = 1)

prior3 <- prior(beta(2, 2), nlpar = "a", lb = 0, ub = 1) + 
  prior(beta(2, 2), nlpar = "b", lb = 0, ub = 1) +
  prior(beta(2, 2), nlpar = "d", lb = 0, ub = 1) +
  prior(normal(-3, 0.1), nlpar = "k")

prior4 <- prior(beta(2, 2), nlpar = "g", lb = 0, ub = 1) + # not sure if gamma is really a probability (g = gamma)
  prior(beta(2, 2), nlpar = "b", lb = 0, ub = 1) +
  prior(beta(5, 1), nlpar = "c", lb = 0, ub = 1) +
  prior(normal(-3, 0.1), nlpar = "k")

# models

model0 <- brm(f0,
              data = dat1_bayes,
              iter = 5000,
              save_pars = save_pars(all = TRUE),
              seed = 42)


model0 <- add_criterion(model0, c("bayes_R2", "waic", "loo")) # add additional fit criteria

# model0_priors <- brm(NF ~ TTime_in_min,
#              data = dat1_bayes,
#              iter = 5000,
#              prior = prior0,
#              save_pars = save_pars(all = TRUE),
#              sample_prior = "only",
#              seed = 42)

# plot(conditional_effects(model0))
# plot(model0)
# summary(model0, priors = TRUE)
# pp_check(model0, ndraws = 100) # posterior predictive check
# plot(model0_priors)

print(model0$criteria$loo) # print additional criteria
print(model0$criteria$waic)
bayes_R2(model0)

model1 <- brm(bf(f1, a+b+c+k ~ 1, nl = TRUE), # model with constant k for faster growth of parameter a
              data = dat1_bayes,
              prior = prior1,
              iter = 5000,
              save_pars = save_pars(all = TRUE),
              control = list(adapt_delta = 0.9),
              seed = 42)

model1 <- add_criterion(model1, c("bayes_R2", "waic", "loo"))

# model with prior predictive check (ie sample prior)

model1_priors <- brm(bf(f1, a+b+c+k ~ 1, nl = TRUE), # model with constant k for faster growth of parameter a
                     data = dat1_bayes,
                     prior = prior1,
                     iter = 5000,
                     save_pars = save_pars(all = TRUE),
                     control = list(adapt_delta = 0.9),
                     sample_prior = "only",
                     seed = 42)

cond_nf <- make_conditions(dat1_bayes, vars = c("TTime_in_min", "NF"))

# plot(conditional_effects(model3_priors, conditions = cond_nf,
#                         method = "predict"), points = TRUE, ylim = c(-1, 1))

plot(conditional_effects(model1))
plot(dat1_bayes)


# plot(conditional_effects(model1))
plot(model1)
# summary(model1, priors = TRUE)
# pp_check(model1, ndraws = 100)
# plot(model1_priors)

print(model1$criteria$loo)
print(model1$criteria$waic)
bayes_R2(model1)

model2 <- brm(bf(f2, a+b+c ~ 1, nl = TRUE), # model without constant k; ie forgetting curve power function
              data = dat1_bayes,
              prior = prior2,
              iter = 5000,
              save_pars = save_pars(all = TRUE),
              seed = 42)
# model with prior predictive check (ie sample prior)

model2_priors <- brm(bf(f2, a+b+c ~ 1, nl = TRUE), # model without constant k; ie forgetting curve power function
                     data = dat1_bayes,
                     prior = prior2,
                     iter = 5000,
                     save_pars = save_pars(all = TRUE),
                     sample_prior = "only",
                     seed = 42)

model2 <- add_criterion(model2, c("bayes_R2", "waic", "loo"))

# plot(conditional_effects(model2))
plot(model2)
# summary(model2, priors = TRUE)
# pp_check(model2, ndraws = 100)
# plot(model2_priors)

print(model2$criteria$loo)
print(model2$criteria$waic)
bayes_R2(model2)

model3 <- brm(bf(f3, a+b+d+k ~ 1, nl = TRUE), # forgetting curve exponential function
              data = dat1_bayes,
              prior = prior3,
              iter = 5000,
              save_pars = save_pars(all = TRUE),
              seed = 42)
# model with prior predictive check (ie sample prior)

model3_priors <- brm(bf(f3, a+b+d+k ~ 1, nl = TRUE), # forgetting curve exponential function
                     data = dat1_bayes,
                     prior = prior3,
                     iter = 5000,
                     save_pars = save_pars(all = TRUE),
                     sample_prior = "only",
                     seed = 42)

model3 <- add_criterion(model3, c("bayes_R2", "waic", "loo"))

# plot(conditional_effects(model3))
plot(model3)
# summary(model3, priors = TRUE)
# pp_check(model3, ndraws = 100)
# plot(model3_priors)

print(model3$criteria$loo)
print(model3$criteria$waic)
bayes_R2(model3)

model4 <- brm(bf(f4, b+c+g+k ~ 1, nl = TRUE), # forgetting curve pareto function
              data = dat1_bayes,
              prior = prior4,
              iter = 5000,
              save_pars = save_pars(all = TRUE),
              seed = 42)

# model with prior predictive check (ie sample prior)

model4_priors <- brm(bf(f4, b+c+g+k ~ 1, nl = TRUE), # forgetting curve pareto function
                     data = dat1_bayes,
                     prior = prior4,
                     iter = 5000,
                     save_pars = save_pars(all = TRUE),
                     sample_prior = "only",
                     seed = 42)

model4 <- add_criterion(model4, c("bayes_R2", "waic", "loo"))


# plot(conditional_effects(model4))
# plot(model4)
# summary(model4, priors = TRUE)
# pp_check(model4, ndraws = 100)
# plot(model4_priors)

print(model4$criteria$loo)
print(model4$criteria$waic)
bayes_R2(model4)

bayes_factor(model0, model1)
bayes_factor(model0, model2)
bayes_factor(model1, model2)
bayes_factor(model1, model3)
bayes_factor(model2, model3)
bayes_factor(model0, model3)
bayes_factor(model0, model4)
bayes_factor(model1, model4)
bayes_factor(model2, model4)
bayes_factor(model3, model4)

####

## visualizing nf/time dataset and prior predictive distributions

dat1_bayes %>% 
  ggplot(aes(TTime_in_min, NF)) + 
  geom_point() + geom_smooth(method = "loess") + 
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Set1") + 
  theme_bw(base_size = 14) +
  ggtitle("Training Time v Neurofeedback-Score")

#
# dat1_bayes %>%

# conditions_m1_priors <- data.frame(TTime_in_min = unique(dat1_bayes$TTime_in_min))
# rownames(conditions_m1_priors) <- unique(dat1_bayes$TTime_in_min)

# ce_nfdata_prior1 <- conditional_effects(model1_priors, conditions = conditions_m1_priors, re_formula = NULL, method = "predict")

# p1 <- plot(ce_nfdata_prior1, ncol = 5, points = TRUE, plot = FALSE)
# p1$dev + ggtitle("Prior predictive distribution model1")

# plot(p1)

# pp_check(model1_priors, type = "hist", ndraws = 20, binwidth = 1, resp = "NF")
# pp_check(model2_priors, type = "hist", ndraws = 20, binwidth = 1, resp = "NF")
# pp_check(model3_priors, type = "hist", ndraws = 20, binwidth = 1, resp = "NF")
# pp_check(model4_priors, type = "hist", ndraws = 20, binwidth = 1, resp = "NF")

# pp_check(model1_priors, height, type = "error_scatter_avg_vs_x", x = dat1_bayes_df$TTime_in_min) +
#   geom_hline(yintercept = 0, color = "red", size = 1.5) +
#   scale_x_continuous(breaks = -5:5)

cond_nf <- make_conditions(dat1_bayes, vars = c("TTime_in_min", "NF"))


####


model1_priors_ppd <- predictive_checks(model1_priors)
model2_priors_ppd <- predictive_checks(model2_priors)
model3_priors_ppd <- predictive_checks(model3_priors)
model4_priors_ppd <- predictive_checks(model4_priors)

model1_posteriors_ppd <- predictive_checks(model1)
model2_posteriors_ppd <- predictive_checks(model2)
model3_posteriors_ppd <- predictive_checks(model3)
model4_posteriors_ppd <- predictive_checks(model4)
model0_posteriors_ppd <- predictive_checks(model0)
# model 1 distributions 

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model1), 
  pars = c("b_a_Intercept", "b_b_Intercept",
           "b_c_Intercept",
           "b_k_Intercept", "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model1",
  subtitle = "with medians and 80% intervals"
)

# model1_priors distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model1_priors), 
  pars = c("b_a_Intercept", "b_b_Intercept",
           "b_c_Intercept",
           "b_k_Intercept", "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model1_priors",
  subtitle = "with medians and 80% intervals"
)

# model2 distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model2), 
  pars = c("b_a_Intercept", "b_b_Intercept",
           "b_c_Intercept",
           "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model2",
  subtitle = "with medians and 80% intervals"
)

# model2_priors distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model2_priors), 
  pars = c("b_a_Intercept", "b_b_Intercept",
           "b_c_Intercept",
           "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model2_priors",
  subtitle = "with medians and 80% intervals"
)

# model3 distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model3), 
  pars = c("b_a_Intercept", "b_b_Intercept",
           "b_d_Intercept",
           "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model3",
  subtitle = "with medians and 80% intervals"
)

# model3_priors distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model3_priors), 
  pars = c("b_a_Intercept", "b_b_Intercept",
           "b_d_Intercept",
           "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model3_priors",
  subtitle = "with medians and 80% intervals"
)

# model4 distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model4), 
  pars = c("b_c_Intercept", "b_b_Intercept",
           "b_g_Intercept",
           "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model4",
  subtitle = "with medians and 80% intervals"
)

# model4_priors distributions

theme_update(text = element_text(family = "sans"))
mcmc_areas(
  as.array(model4_priors), 
  pars = c("b_c_Intercept", "b_b_Intercept",
           "b_g_Intercept",
           "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
) + ggplot2::labs(
  title = "Prior parameter distributions Model4_priors",
  subtitle = "with medians and 80% intervals"
)



