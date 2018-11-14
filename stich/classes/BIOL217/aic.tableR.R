# 'aic.tableR'
# Function to create AIC Model selection table for mixed models
  # We define a function that takes the arguments 'x' and 'y'
    aic.tableR <- function(x, y=NULL){
      # Create a column to hold number of parameters for our model
        k <- c()
      # Get number of parameters for each ith model in our candidate model set
        for (i in 1:length(x)){
        # Note that we are adding a one to the number of parameters in each
        # model. This is to account for the df needed to estimate the random
        # effect. I suppose we could use a different number or a series of
        # numbers if we were interested in evaluating the sensitivity of
        # model selection statistics to the
          k[i]<-nrow(data.frame(summary(x[[i]])[10]))+1
        }
      # Make blank columns to hold statistics
        AICc <- c(rep(0, length(k)))        # AICc score
        DeltaAIC <- c(rep(0, length(k)))    # Delta AICc
        AICWeight <- c(rep(0, length(k)))   # Wi
        Deviance <- c(rep(0, length(k)))    # Model deviance
        R.lik<-c(rep(0, length(k)))         # Rel. likelihood for wi
      # Make a dataframe to hold results of model selection
        if(is.null(y)){ y=rep("", length(x))}

        ranks<-data.frame(y, k, AICc, DeltaAIC, AICWeight)
        names(ranks)[1] <- "Model"
      # Get AICc for each model
        for(i in 1:nrow(ranks)){
          # Note that we are penalizing the likelihood of each model based on
          # the number of parameters in each model in addition to sample size
          ranks$AICc[i] <- AIC(x[[i]])+(((2*k[i])*(k[i]+1))/
            (nrow(est.mvmt)-k[i]-1))
        }
      # Sort the table by AICc to get them in order for calculations
        ranks <- ranks[with(ranks, order(AICc)), ]
      # Calculate delta AIC
        for(i in 1:nrow(ranks)){
          ranks$DeltaAIC[i] <- ranks$AICc[i]-ranks$AICc[1]
        }
      # Calculate the relative likelihood for each of the models
        for(i in 1:nrow(ranks)){
          R.lik[i] <- exp(-.5*ranks$DeltaAIC[i])
        }
      # Calculate AIC weight for each of the models based on the relative
      # likelihood
        for(i in 1:length(R.lik)){
          ranks$AICWeight[i] <- R.lik[i]/sum(R.lik[1:length(R.lik)])
        }
      # Round off the calculated columns to two decimal places because that is
      # really all we need for wi and we want the rest to be standardized.
        ranks[ , 3:5] <- round(ranks[ , 3:5], 2)
      # Print the model selection results
        return(ranks)
  }