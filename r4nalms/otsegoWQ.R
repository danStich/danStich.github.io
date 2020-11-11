# 1. Front-end needs ----
library(akima)
library(lubridate)
library(rLakeAnalyzer)


# 2. Isopleth analysis ----
# Read in the data
limnos <- read.csv("physical.csv")

# Data formatting & extraction
limnos$date <- as.POSIXct(limnos$date, format = "%m/%d/%Y")
limnos$year <- year(limnos$date)
limnos$day <- yday(limnos$date)

# Subset the data: just want to work with a single
# year for this example, but we want a little data
# on either end of that.
limnos <- limnos[limnos$date >= "2015-10-15 EST" &
  limnos$date <= "2017-04-15 EST", ]

# Remove NA values to make life easier
lim <- na.omit(limnos)

# Multiply depth column by -1 so depth will
# plot from top to bottom
lim$depth <- -1 * lim$depth

# Create a data frame containing the
# x, y, and z variables of interest
plotter <- data.frame(x = lim$day, y = lim$depth, z = lim$temp)

# Sort it so we have ascending values of x and y
plotter <- plotter[with(plotter, order(x, y)), ]

# Make a regularly spaced x, y, z grid using
# linear interpolation from the akima package
im <- with(
  plotter,
  interp(x, y, z,
    duplicate = "mean",
    nx = 10, ny = 100
  )
)

# Plot the isopleth
# filled.contour is the function that actually
# makes the contour plot. This is the same function
# that is used in the wtr.heat.map function in the
# rLakeAnalyzer package, but it is executed from
# within a convenience wrapper there, so it is
# hard to customize. I tend to work with the
# filled.contour function from the graphics
# package (included in base R and loaded by default).

# Set up plotting window margins
par(mar = c(4, 4, 1, 8))

# Make the graph
filled.contour(
  im$x,
  im$y,
  im$z,
  levels = seq(0, 25, 1),
  col = topo.colors(25),
  ylim = c(-48, 0),
  xlim = c(min(im$x), max(im$x)),
  xlab = "Ordinal date",
  ylab = "Depth (m)",
  cex.lab = 1.15,
  plot.axes = {
    axis(2,
      at = seq(0, -50, -10),
      labels = seq(0, 50, 10),
      cex.axis = 1.10
    )
    axis(1)
  }
)





# 3. Thermocline depth analysis ----
# Read in the data
limnos <- read.csv("physical.csv")

# Data formatting & extraction
limnos$date <- as.POSIXct(limnos$date, format = "%m/%d/%Y")
limnos$year <- year(limnos$date)
limnos$day <- yday(limnos$date)
limnos$month <- month(limnos$date)

# Remove rows with NA to make life easier for now
limnos <- na.omit(limnos)

# Create a list of dataframes split by date
# so each date corresponds to a df that is an
# element of the list
limlist <- split(limnos, list(limnos$date))

# Create a function to calculate thermocline
# depth that can be applied to a list as long
# as depth is in the 4th column and temperature
# is in the 5th column of each list element (matrix
# or dataframe). This function creates a new
# variable (column) called 'thermo' in each df,
# and returns the original object modified in
# place.
ltd <- function(x) {
  x$thermo <- rLakeAnalyzer::thermo.depth(x[, 5], x[, 4])
  return(x)
}

# Create a test object that contains the
test <- lapply(limlist, ltd)

# Stack all of the dfs in the list into
# a single, large df
out <- do.call(rbind, test)

# Make a plot of July thermocline depth
# across years, and include predictions from
# a GAM for data viz.
# First, subset the data
out1 <- out[out$month == 7, ]

# Now, plot it with ggplot2 functions
plot <- ggplot(out1, aes(year, thermo)) +
  geom_point(pch = 21, cex = 5, col = "gray40", bg = "gray40") +
  geom_smooth(
    method = "gam", level = .999,
    col = "red", bg = "tomato"
  ) +
  xlab("Year") +
  xlim(2011, 2017) +
  ylab("July thermocline depth (m)") +
  ylim(5, 10)
plot + theme(plot.margin = margin(5, 5, 10, 10))
