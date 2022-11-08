
library(tidyverse)


otsego <- read.csv("data/physical.csv", stringsAsFactors = FALSE)


# Like this:
glimpse(otsego)


names(otsego)


# First, we replace temp with Temperature
names(otsego)[3] <- "Temperature"

# Print the names of the df
# to the console so we can
# see them.
names(otsego)

library(lubridate)

# Get date in a standard, unambiguous format
otsego$date <- as.Date(otsego$date, format = "%m/%d/%Y")

# Make a variable to hold year in the df
otsego$year <- year(otsego$date)

# Make a variable to hold month
otsego$month_num <- month(otsego$date)

# Convert this to a factor so it can be easily re-leveled and ordered
otsego$month <- month.abb[otsego$month_num]
otsego$month <- factor(otsego$month, levels = month.abb)

# Let's make an ordinal date variable to get day of year as well
otsego$doy <- yday(otsego$date)


ggplot(otsego, aes(x = Temperature)) +
  geom_histogram(bins = 30)

otsego$log_temperature <- log(otsego$Temperature)

ggplot(otsego, aes(x = log_temperature)) +
  geom_histogram(bins = 30)

otsego <- otsego %>% 
  filter(!is.na(Temperature) & Temperature > 0)

otsego$layer <- "epilimnion"
otsego$layer[otsego$depth > 15] <- "hypolimnion" 

# Wilcox test to assess the null hypothesis
# that there is no difference in temperature between
# epilimnion and hypolimnion.
wilcox.test(Temperature ~ layer, data = otsego)

# T-test to assess the null hypothesis
# that there is no difference in temperature
# between the epilimnion and hypolimnion in Otsego Lake.

# We use log_temperature to meet assumptions
# of normality.

# We can specify this one using a formula.
# To be conservative here, we will assume
# that we have unequal variances using
# one of the optional arguments. Note that
# the default in R is to assume that variances
# are not equal, and this defaults to
# a Welch's t-test that uses a calculated df
# to adjust the calculated test statistic.
t.test(log_temperature ~ layer, data = otsego, equal = FALSE)

mod <- t.test(log_temperature ~ layer, data = otsego, equal = FALSE)

ggplot(data = otsego,
       aes(x = layer, y = log_temperature, color = layer, fill = layer)) +
  geom_violin(alpha = 0.10, draw_quantiles = 0.50)

# Fit an ANOVA to test for differences in
# means between groups
mod <- lm(log(Temperature) ~ month, data = otsego)

# Get ANOVA summary for the model
anova(mod)

ggplot(otsego, aes(x = month, y = Temperature)) +
  geom_boxplot()

# Fit the model
lmod <- lm(log(Temperature) ~ year, data = otsego)

summary(lmod)

log_preds <- predict(lmod, interval = "prediction")

# Convert back to real scale
real_preds <- apply(X = log_preds, MARGIN = 2, FUN = exp)

# Take a look at the first few
# rows of the preds dataframe
head(real_preds)

otsego_preds <- data.frame(otsego, real_preds)

ggplot(otsego_preds, aes(x = year, y = Temperature)) +
  geom_point(alpha = 0.10) +
  geom_line(aes(y = fit), size = 1, color = "black") +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
    color = "black",
    fill = "gray87",
    alpha = .5,
    lty = 2,
    lwd = .5
  ) +
  theme(legend.position = "none")

surface <- otsego %>% 
  filter(depth <= 5)

surface_mod <- lm(log(Temperature) ~ year + month, data = surface) 

# Make predictions from the model on the log scale
lpreds <- predict(surface_mod, interval = "prediction")

# Back-transform fit, lwr, upr to the real scale by exponentiating
rpreds <- apply(lpreds, 2, exp)

# Smash it together with the original data.
# Yes R, we know these are not future responses, just do it
lake_preds <- data.frame(surface, rpreds)

# Plot the predictions against the observed data
ggplot(lake_preds, aes(x = year, y = Temperature)) +
  geom_point(alpha = 0.10) +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(xmax = year, ymin = lwr, ymax = upr), alpha = 0.1) +
  facet_wrap(~month) +
  theme_bw()

# Set this aside as a new object
# We'll drop any observations that lack data
lim <- otsego %>% 
  filter(!is.na(depth) & !is.na(doy) & !is.na(Temperature))


# Multiply depth column by -1 so depth will
# plot from top to bottom.
lim$depth <- -1 * lim$depth

library(akima)


# Create a data frame containing the
# x, y, and z variables of interest
plotter <- data.frame(x = lim$doy, y = lim$depth, z = lim$Temperature)

# Sort it so we have ascending values of x and y
plotter <- plotter[with(plotter, order(x, y)), ]

# Make a regularly spaced x, y, z grid using
# linear interpolation from the akima package
im <- with(
  plotter,
  akima::interp(x, y, z,
    duplicate = "mean",
    nx = 360,#length(unique(lim$doy)),
    ny = 50 #length(unique(lim$depth))
  )
)

# Plot the isopleth
# filled.contour is the function that actually
# makes the contour plot. This is the same function
# that is used in the wtr.heat.map function in the
# RLakeAnalyzer package, but it is executed from
# within a convenience wrapper there, so it is
# hard to customize.

# I tend to work with the filled.contour
# function from the graphics package (included
# in base R and loaded by default). This is
# just a preference driven by need for
# more flexibility.

# Set up plotting window margins
par(mar = c(4, 4, 2, 8))

# Make the graph
filled.contour(
  im$x, # Variable on x-axis (date)
  im$y, # Variable on y-axis (depth)
  im$z, # Response (water quality parameter)
  # Could also choose 'grey.colors' or 'terrain.colors'.
  # If you want the ramp to go the other way,
  # just delete the 'rev'. Note that you will
  # need to change the 26 in parentheses to match
  # the number of levels that you actually have or
  # want to display.
  col = topo.colors(30),
  # I don't like in-figure titles.
  # You can add one, though. You will, however,
  # need to change the 'mar' argument in the call
  # to par above.
  main = expression(paste("Temperature (", degree, "C)")),
  # Specify y-axis limits.
  ylim = c(min(im$y), max(im$y)),
  # Specify x-axis limits. In this case, we are "zooming in" on year 2017
  # Can use this one instead if date formatting throws error
  # xlim = c(as.numeric(as.Date("2017/05/01")), max(im$x)),
  xlim = c(0, 366),
  # X-axis label
  xlab = "Day of Year",
  # Y-axis label
  ylab = "Depth (m)",
  # This next block of code controls the lines that are overlaid. The interpolation
  # is the same, so the axes and the number of levels need to be the same, too.
  plot.axes = {
    contour(
      im$x, im$y, im$z, # X, Y, Z values (same)
      nlevels = 26, # Need to change this with the No. on line 37 to be same!
      levels = seq(0, 26, 1), # Will need to change for different parameters
      drawlabels = FALSE, # Don't want R to draw the labels, they are ugly
      col = rgb(1,1,1,0),
      add = TRUE); # Add the lines to the existing plot
    # X-axis to assign month abbreviation at first ordinal day of each month
    axis(1, at = c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336),
         labels = month.abb, cex.axis=1.10
    )
    # Y-axis
    ### Change to match range of data
    axis(side=2, at=seq(-50, 0, 5), seq(50, 0, -5), las=2, yaxt='l', cex.axis=1.1);
    # Add sampling points to x-axis
    points(unique(lim$doy), 
           rep(-50, length(unique(lim$doy))), 
           pch=21, bg=rgb(0,0,0,.05), xpd=T)
    
  })
