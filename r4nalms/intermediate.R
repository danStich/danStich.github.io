
## -------------------------------------------
library(tidyverse)



## ---------------------------------------------------------------------------
minnesota <- read.csv("data/minnesota.csv", stringsAsFactors = FALSE)


## ---- eval=TRUE-------------------------------------------------------------
# Like this:
str(minnesota)


## ---------------------------------------------------------------------------
names(minnesota)


## ---------------------------------------------------------------------------
# First, we replace Secchi..m. with Secchi
names(minnesota)[8] <- "Secchi"


## ---------------------------------------------------------------------------
# Print the names of the df
# to the console so we can
# see them.
names(minnesota)


## ---------------------------------------------------------------------------
# So funny
minnesummary <- minnesota %>%
  group_by(Water.Body, GNIS.ID, GNIS.Class, Elevation) %>%
  summarize(
    Secchi = mean(Secchi),
    Latitude = mean(Latitude),
    Longitude = mean(Longitude),
    .groups = "keep"
  )


## ----------------------------------------------
library(rgdal)
MN <- rgdal::readOGR("data/Boundaries_of_Minnesota.shp", verbose = FALSE)


## ---------------------------------------------------------------------------
# Remove those points with missing longitude and latitude
d <- minnesummary %>%
  filter(!is.na(Latitude) &
    !is.na(Longitude) &
    Longitude <= 0 &
    (Latitude >= 40 & Latitude <= 60))


## ---------------------------------------------------------------------------
# Load sp package
library(sp)

# Make an intermediate object to modify
ds <- d

# Assign longitude and latitude to a
# SpatialPoints-class obj. This converts d to
# a spatial dataframe
coordinates(ds) <- c("Longitude", "Latitude")


## ---------------------------------------------------------------------------
proj4string(ds) <- CRS("+proj=longlat +datum=WGS84")


## ---------------------------------------------------------------------------
# Get UTMs for the longitudes and latitudes using
# the coordinate system of our shape file
coord_utm <- sp::spTransform(ds, CRS("+init=epsg:26911"))


## ---------------------------------------------------------------------------
# Assign the coordinates to new columns
# in our dataframe
d$x <- coord_utm@coords[, 1]
d$y <- coord_utm@coords[, 2]
coordinates(d) <- ~ x + y
proj4string(d) <- CRS("+init=epsg:26911")


## ---------------------------------------------------------------------------
MN <- sp::spTransform(MN, CRS("+init=epsg:26911"))


## ----------------------------------------------------------
# We'll use the ggplot2 library
library(ggplot2)

# Make the plot
ggplot() +
  geom_polygon(
    data = fortify(MN),
    color = "black",
    fill = "gray40",
    aes(x = long, y = lat)
  ) +
  # coord_sf() +
  geom_point(
    data = data.frame(d),
    mapping = aes(x = x, y = y)
  ) +
  labs(x = "Easting", y = "Northing") +
  ggtitle("Minnesota lakes") +
  theme(
    plot.title = element_text(hjust = .5),
    text = element_text(size = 10)
  ) +

  # Adjusting output width: distorts CRS
  # but can actually see the plot
  coord_equal(ratio = .5)


## ---------------------------------------------------------------------------
# Perform a spatial intersect between
# the Minnesota shapefile (polygon) and
# the SpatialPoints object.
# Note that order is important here.
proj4string(d) <- proj4string(MN)
ins <- sp::over(d, MN)

# Then, we can drop the points that
# do not not intersect with the polygon,
# now saving over the original data set.
dd <- d[!is.na(ins[, 1]), ]

# Get the modified data back out of the
# SpatialPoints object
minnesota_sp <- dd@data


## --------------------------------------------------------------
# We'll use the ggplot2 library
library(ggplot2)

# Make the plot
ggplot() +
  geom_polygon(
    data = fortify(MN),
    color = "black",
    fill = "gray60",
    aes(x = long, y = lat)
  ) +
  coord_sf() +
  geom_point(
    data = data.frame(dd),
    mapping = aes(x = x, y = y)
  ) +
  labs(x = "Easting", y = "Northing") +
  ggtitle("Minnesota lakes") +
  theme(
    plot.title = element_text(hjust = .5),
    text = element_text(size = 10)
  )


## --------------------------------------------------------------
# Make the plot...again
ggplot() +
  geom_polygon(
    data = fortify(MN),
    color = "black",
    fill = "gray90",
    aes(x = long, y = lat)
  ) +
  coord_sf() +
  geom_jitter(
    data = data.frame(dd),
    mapping = aes(x = x, y = y, color = dd$Secchi),
    alpha = .5, size = 3
  ) +
  labs(x = "", y = "") +
  ggtitle("Secchi depth (m)") +
  theme(
    plot.title = element_text(hjust = .5, face = "bold"),
    text = element_text(size = 14),
    legend.position = "right",
    legend.title.align = 0,
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) +
  scale_colour_gradientn("", colours = c("gray90", "black"))


## ---------------------------------------------------------------------------
ggplot(minnesummary, aes(x = Secchi)) +
  geom_histogram(bins = 15)


## ---------------------------------------------------------------------------
minnesummary$logSecchi <- log(minnesummary$Secchi)


## ---------------------------------------------------------------------------
ggplot(minnesummary, aes(x = logSecchi)) +
  geom_histogram(bins = 15)


## ---------------------------------------------------------------------------
# Haha, like the abbreviation for Holdren et al. (2006)!
mlr <- minnesummary %>%
  filter(GNIS.Class == "Lake" | GNIS.Class == "Reservoir")


## ---------------------------------------------------------------------------
# Wilcox test to assess the null hypothesis
# that there is no difference in Secchi between
# lakes and reservoirs.
wilcox.test(Secchi ~ GNIS.Class, data = mlr)


## ---------------------------------------------------------------------------
# T-test to assess the null hypothesis
# that there is no difference in Secchi
# between lakes and reservoirs in Minnesota.

# We use logSecchi to meet assumptions
# of normality.

# We can specify this one using a formula.
# To be conservative here, we will assume
# that we have unequal variances using
# one of the optional arguments. Note that
# the default in R is to assume that variances
# are not equal, and this defaults to
# a Welch's t-test that uses a calculated df
# to adjust the calculated test statistic.
t.test(logSecchi ~ GNIS.Class, data = mlr, equal = FALSE)

## ---------------------------------------------------------------------------
ggplot(
  data = mlr,
  aes(
    x = GNIS.Class, y = logSecchi,
    color = GNIS.Class, fill = GNIS.Class
  )
) +
  geom_violin(alpha = 0.10, draw_quantiles = 0.50) +
  geom_jitter(width = 0.1, alpha = 0.20)


## ---------------------------------------------------------------------------
# Fit an ANOVA to test for differences in
# means between groups
mod <- lm(logSecchi ~ GNIS.Class, data = mlr)


## ---------------------------------------------------------------------------
# Get ANOVA summary for the model
anova(mod)


## ---------------------------------------------------------------------------
TukeyHSD(aov(mod))


## ---------------------------------------------------------------------------
# Drop rows with missing Latitudes
mn_data <- minnesummary %>%
  filter(!is.na(Latitude) &
    (Latitude >= 40 & Latitude <= 50))

# Now fit the model
lmod <- lm(logSecchi ~ Latitude, data = mn_data)


## ---------------------------------------------------------------------------
summary(lmod)


## ------------------------------------------
log_preds <- predict(lmod, interval = "prediction")


## ---------------------------------------------------------------------------
# Convert back to real scale
real_preds <- apply(X = log_preds, MARGIN = 2, FUN = exp)


## ---------------------------------------------------------------------------
# Take a look at the first few
# rows of the preds dataframe
head(real_preds)


## ---------------------------------------------------------------------------
mn_preds <- data.frame(mn_data, real_preds)


## ---------------------------------------------------------------------------
ggplot(mn_preds, aes(x = Latitude, y = Secchi)) +
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


## ---------------------------------------------------------------------------
otsego <- read.csv("data/physical.csv")


## ---------------------------------------------------------------------------
# Data formatting & extraction

# First, we convert the date column
# to a character string. We pass the
# result directly to the as.Date
# function, and along with that we
# specify a format so R knows where it
# is looking for specific elements of
# the date info we are trying to pass.
otsego$date <- as.Date(
  as.character(otsego$date),
  format = "%m/%d/%Y"
)


## ---------------------------------------------------------------------------
# Remove NA values to make life easier
lim <- na.omit(otsego)


## ---------------------------------------------------------------------------
# Multiply depth column by -1 so depth will
# plot from top to bottom.
lim$depth <- -1 * lim$depth


## ----------------------------------------------
library(akima)


## ---------------------------------------------------------------------------
# Create a data frame containing the
# x, y, and z variables of interest
plotter <- data.frame(x = lim$date, y = lim$depth, z = lim$temp)

# Sort it so we have ascending values of x and y
plotter <- plotter[with(plotter, order(x, y)), ]

# Make a regularly spaced x, y, z grid using
# linear interpolation from the akima package
im <- with(
  plotter,
  interp(x, y, z,
    duplicate = "mean",
    nx = length(unique(lim$date)),
    ny = length(unique(lim$depth))
  )
)


## ---------------------------------------------------------------------------
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
  im$z, # Response (wq parameter)
  # Could also choose 'grey.colors' or 'terrain.colors'.
  # If you want the ramp to go the other way,
  # just delete the 'rev'. Note that you will
  # need to change the 26 in parentheses to match
  # the number of levels that you actually have or
  # want to display.
  col = topo.colors(26),
  # I don't like in-figure titles.
  # You can add one, though. You will, however,
  # need to change the 'mar' argument in the call
  # to par above.
  main = expression(paste("Temperature (", degree, "C)")),
  # Specify y-axis limits.
  ylim = c(min(im$y), max(im$y)),
  # Specify x-axis limits. In
  # this case, we are "zooming in"
  # on year 2017
  xlim = c(as.Date("2017/05/01"), max(im$x)),
  # X-axis label
  xlab = "Date",
  # Y-axis label
  ylab = "Depth (m)",
  # Axis options
  plot.axes = {
    # This is how we include
    # countour lines
    contour(
      im$x,
      im$y,
      im$z,
      nlevels = 26,
      drawlabels = FALSE,
      col = topo.colors(26),
      lwd = 1,
      lty = 2,
      add = TRUE
    )
    # Y-axis
    axis(2,
      at = seq(0, -50, -10),
      labels = seq(0, 50, 10)
    )
    # X-axis
    axis(1,
      at = seq(as.Date("2017/05/01"),
        by = "2 months",
        length.out = 16
      ),
      labels = format(
        seq(as.Date("2017/05/01"),
          by = "2 months",
          length.out = 16
        ),
        "%b %Y"
      )
    )
  }
)

