# 04/02/19 Coby stats

# Make R do stuff
1 + 1 

a <- 1 + 2

b <- a * 3

a
b

a <- c(1,2,3,4,5,6,7,8,9)

b <- a * 3

b

sum(a) * sum(b)

a * b

library(FSA)




bass <- read.csv("lmblw.csv")


hist(bass$Length)

hist(x = bass$Length,
     col = 'gray87',
     xlab = 'Total length (mm)' 
    )

mybass = bass[bass$Water == 'Saratoga Lake', ]


hist(x = mybass$Length,
     breaks = diff(range(mybass$Length))/10,
     yaxt = 'n',  # Don't print y-axis yet
     xaxt = 'n',  # Don't print x-axis yet
     col = 'gray87',
     main = '',
     xlab = 'Fork length (mm)'
     )
axis(side = 1, pos = 0)
axis(side = 2, pos = min(mybass$Length), las = 2)


# Make an l-f histogram that includes Gabelhouse lengths
psdPlot(~Length,    
        data=mybass, 
        species="Largemouth Bass", 
        w=10) 

  psdCalc(~Length,
          data=mybass,
          species="Largemouth Bass",
          digits=1)



  
  # Make up mean and 95% CI of PSD for each species
  prey <- c(45.4, 30.2, 56.8)
  pred <- c(24.5, 10.2, 36.7)
  
# Give names to the values in each of those objects  
  names(prey) <- names(pred) <- c("Estimate", "95% LCI", "95% UCI")

tictactoe(predobj = c(40, 60),
        preyobj = c(20, 40),
        predlab = "Predator PSD",
        preylab = "Prey PSD", obj.col = "black",
        obj.trans = 0.2,
        bnd.col = "black",
        bnd.lwd = 1,
        bnd.lty = 2
        )
  
# Load the `plotrix` package for the
# `plotCI` function first. You may need
# to install this if it didn't automatically
# install with `FSA`
  library(plotrix)

# Plot the PSD and the confidence intervals
  plotCI(prey[1],pred[1],li=prey[2],ui=prey[3],
         err="x",pch=16,add=TRUE)
  
  
  plotCI(prey[1],pred[1],li=pred[2],ui=pred[3],
         err="y",pch=16,add=TRUE)  
  
  
shad <- read.csv('sus_shad.csv')
  
  
# Define von Bertalanffy growth function
  vbmod <- FL ~ Linf * (1 - exp( -K * (Age - t0) ) )  
  
  starts = vbStarts(formula=FL~Age, data=shad)  
  
  starts
  
  mymod = nls(vbmod, data = shad, start = starts)

  summary(mymod)  
  
  
  
# New sequence of ages with tiny steps
# to make the line look smooth
  newage <- seq(1, 13, .5)

# Get parameters
  params = summary(mymod)$coefficients

# Predict mean length at age:
  pred = params[1,1]*(1-exp(-params[2,1]*(newage-params[3,1])))
  
# Plot the raw data
  plot(shad$Age,
       shad$FL,
       pch=21, 
       bg=rgb(0.8, 0.8, 0.8, 0.15),
       col=rgb(0.8, 0.8, 0.8, 0.15),
       xlab = 'Age (years)',
       ylab = 'Fork length (mm)',
       yaxt='n'
       )
  axis(2, las=2)
       
# Add the mean length at age predictions
  lines(x=newage, y=pred, lty=1, lwd=2, col='gray40')  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  




























