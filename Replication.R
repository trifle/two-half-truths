# Replication Code for "Two Half-Truths Make a Whole?"
# Author: Pascal Jürgens (p@atlasnovus.net)

# The following code enables replication of statistical analyses and graphics contained in the paper.
# It requires a data file that can be obtained from the authors, see the corresponding section in the text.

# IMPORTANT NOTE ON REPLICABILITY
# The bias simulation step is only replicable if the seed value was set properly.
# Special care must be taken to include that step when running parts of the script selectively.
# Software versions at publication time:
# R version 3.5.1 (2018-07-02) -- "Feather Spray", Platform: x86_64-apple-darwin15.6.0 (64-bit)
# brms 2.6.0
# cowplot 0.9.3
# dplyr 0.7.8
# ggplot2 3.1.0
# sjPlot 2.6.1
# rstan 2.18.2

###############
# Prerequisites:
# Libraries: ggplot2, brms, sjPlot, cowplot, dplyr
# Data file: replication.data.csv
###############

require(brms)
require(ggplot2)
require(cowplot)
require(sjPlot)
require(dplyr)



###############
# Setup
###############

# Set seed for the PRNG so that results are replicable.
# Given this seed, numerical results will match the published manuscript
# as long as there are no changes to the employed libraries.
# Note that brms/rstan models don't use R's seed and still need to have the value as an explicit argument.

set.seed(112358)

# Read data
source("replication.data.Rdmpd")

###############
# Data Overview
###############



###############
# Figures
###############

###############
# Figure 2: Self-Report Bias of Internet usage
# Note: This section uses the library "cowplot" to combine two plots
###############

# Output to file
png("f2_bias_daily_and_avg.png", width=5000, height=2500, res=300)

average_bias <- ggplot(
    # Data
    replication.data,
    aes(x=online_time_survey, y=online_time_tracking_average, group=online_time_survey)) +
    geom_boxplot() +
    # Fix scales across both plots
    scale_y_continuous(limits = c(0, 15)) +
    # Plot dashed line for zero bias
    geom_abline(intercept = 0, slope = 1, linetype="dashed") +
    theme_minimal() +
    # Hide label for this x axis, having one on the other plot is enough
    xlab("") +
    ylab("Daily Usage from Tracking (hours)") +
    ggtitle("Self-Report Bias (Internet, Average Day)") +
    theme(axis.line.x=element_blank(),axis.line.y=element_blank())

daily_bias <- ggplot(
    # Data
    replication.data,
    aes(x=online_time_survey, y=online_time_tracking_daily, group=online_time_survey)) +
    geom_boxplot() +
    # Fix scales across both plots
    scale_y_continuous(limits = c(0, 15)) +
    # Plot dashed line for zero bias
    geom_abline(intercept = 0, slope = 1, linetype="dashed") +
    theme_minimal() +
    # Hide label for x and y axis
    xlab("") +
    ylab("") +
    ggtitle("Self-Report Bias (Internet, All Days)") +
    theme(axis.line.x=element_blank(),axis.line.y=element_blank())

# Combine both plots side by side, adding a common x axis label and title
combined_plot <- cowplot::plot_grid(average_bias, daily_bias, labels = "AUTO")
title <- ggdraw() + draw_label("Figure 2: Self-Report Bias of Internet Usage", fontface = 'bold')
combined_plot <- add_sub(combined_plot, "Self-Assessed Average Daily Internet Usage (hours)")
cowplot::plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
dev.off()


###############
# Figure 3: Self-Report Bias by Availability of Device Data
###############

# Output to file
png("f3_bias_by_device.png", width=5000, height=2500, res=300)
ggplot(
    # Data to use. Omit group with no data.
    subset(replication.data, device_factor!='none'),
    aes(x=online_time_survey, y=bias_daily, group=online_time_survey)) +
    # Use jitter to make individual data points discernible+
    geom_jitter(color="grey", size=1) +
    # Show dotted line at zero bias
    geom_abline(linetype="dashed", slope=0) +
    facet_grid(~device_factor) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.line.x=element_blank(),axis.line.y=element_blank()) +
    scale_fill_grey(start = 0.7, end = 1) +
    ggtitle("Figure 3: Self-Report Bias by Availability of Device Data") +
    ylab("Self-Assessment Bias (Overestimation of Usage in h)") +
    xlab("Self-Assessed Internet Usage Time (h)")
dev.off()

###############
# Statistical Models
###############

###############
# Model 1 (Table 1): Predictors of Bias in Self-Reports (Bayesian Mixed Model)
###############

# Data includes up to two issues per day. Since this model does not include
# the DV on a per-issue basis, we need to pool the data into daily data points

# Create new variable for day-id pairs
replication.data$day_id <- apply(replication.data[c("id", "day")], 1, paste, collapse="_")

# Group by day_id
replication.data.grouped <- group_by(replication.data, day_id)

# Create new pooled data frame
replication.data.pooled <- summarize(
  replication.data.grouped,
  id=first(id),
  day=first(day),
  age=first(age),
  sex=first(sex),
  education=first(education),
  income=first(income),
  online_time_survey=first(online_time_survey),
  device_factor=first(device_factor),
  bias_daily=first(bias_daily),
  bias_censored=first(bias_censored)
)

bias_predictor_model <- brm(
    # DV is the daily bias, which can be censored
    # (has a capped maximum). We therefore use a binary variable to flag censored
    # values for the model.

    bias_daily|cens(bias_censored)

    # Predictors

    ~ device_factor +
    scale(age) +
    sex +
    scale(education) +
    scale(income) +
    scale(online_time_survey) +

    # Levels, fixed slope, random intercept
    (1|day) +
    (1|id)

    # parameters
    # Note that the model run will take some time due to
    # increased number of chains and iterations.

    ,data=replication.data.pooled
    ,chains=8
    ,cores=8
    ,iter=8000
    ,seed=112358
)

###############
# Example Models (Table 2): Bayesian Models for Bias Simulation
###############

###############
# Authentic Models
# Note that the first model (without device factor) is not included in the paper.
###############

two.mlm <- brm(
  p.polarization
    ~ scale(age) +
    sex +
    scale(education) +
    scale(income) +
    scale(own_position) +
    scale(online_time_survey) +
    scale(online_time_tracking_daily) +
    (1|day) +
    (1|issue)
    ,data=replication.data
    ,cores=8
    ,seed=112358
)

three.mlm <- brm(
  p.polarization
    ~ scale(age) +
    sex +
    scale(education) +
    scale(income) +
    scale(own_position) +
    scale(online_time_survey) +
    scale(online_time_tracking_daily) +
    device_factor +
    (1|day) +
    (1|issue)
    ,data=replication.data
    ,cores=8
    ,seed=112358
)

###############
# Simulated Models
###############

###############
# Generate Bias
###############

# Set seed again to enable replicability of this part even when run in isolation.
set.seed(112358)

# Make copy of the data
d.sim <- replication.data

# Calculate means to have a rough indicator of the impact of our simulated bias
pre.mean.mobile <- mean(d.sim$online_time_survey[d.sim$device_factor=='mobile'])
pre.mean.other <- mean(d.sim$online_time_survey[d.sim$device_factor!='mobile'])
pre.mean <- mean(d.sim$online_time_survey)

# Get number of cases for each device group
n.mobile <- length(d.sim$online_time_survey[d.sim$device_factor=='mobile'])
n.other <- length(d.sim$online_time_survey[d.sim$device_factor!='mobile'])

# Create normally distributed error
# Add higher bias to mobile, and balance this out by
# subtracting the appropriate counterpart from the other devices.
mobile.add <- rnorm(n.mobile, 15, 1)
other.add <- rnorm(n.other, 10, 1)
other.factor <- sum(other.add) / sum(mobile.add)
other.add <- other.add / other.factor

# Add bias to self-assessments
d.sim$online_time_survey[d.sim$device_factor=='mobile'] <- d.sim$online_time_survey[d.sim$device_factor=='mobile'] + mobile.add
d.sim$online_time_survey[d.sim$device_factor!='mobile'] <- d.sim$online_time_survey[d.sim$device_factor!='mobile'] - other.add

# Calculate impact by comparing to previous means
post.mean.mobile <- mean(d.sim$online_time_survey[d.sim$device_factor=='mobile'])
post.mean.other <- mean(d.sim$online_time_survey[d.sim$device_factor!='mobile'])
post.mean <- mean(d.sim$online_time_survey)

###############
# Models with Biased Data
###############

two.sim <- brm(p.polarization
    ~ scale(age) +
    sex +
    scale(education) +
    scale(income) +
    scale(own_position) +
    scale(online_time_survey) +
    scale(online_time_tracking_daily) +
    (1|day) +
    (1|issue)
    ,data=d.sim
    ,cores=8
    ,seed=112358
)

three.sim <- brm(p.polarization
    ~ scale(age) +
    sex +
    scale(education) +
    scale(income) +
    scale(own_position) +
    scale(online_time_survey) +
    scale(online_time_tracking_daily) +
    device_factor +
    (1|day) +
    (1|issue)
    ,data=d.sim
    ,cores=8
    ,seed=112358
)

# Create table with all models in one.
# The function tab_model is from sjPlot, tweak parameters to show desired data.
# Note: To get additional model diagnostics, such as R hat, use summary(model)

# All models
tab_model(two.sim, three.sim, two.mlm, three.mlm, emph.p = T, show.hdi50 = F)

# Table2 from paper
tab_model(three.mlm, two.sim, three.sim, emph.p = T, show.hdi50 = F)

# Model analysis. Provided for completeness, since the comparison is not meaningful
# across simulation and authentic data.
waic.three.mlm <- waic(three.mlm)
waic.two.sim <- waic(two.sim)
waic.three.sim <- waic(three.sim)

compare_ic(waic.three.mlm, waic.two.sim, waic.three.sim)
