n_groups <- 16
n_years <- 10
n <- n_groups * n_years
pop <- gl(n = n_groups, k = n_years)
# year <- rep(seq(1, 10, 1), n_groups)
rain <- runif(n, 0, 1)
temp <- runif(n, 0, 1)
# haplotype <- rbinom(n, 1, 0.5)
N <- round(runif(n, 10, 50))
Xmat <- model.matrix(~pop*rain - 1 - rain)

intercept_mean <- -0.2
intercept_sd <- 1
slope_mean <- 3
slope_sd <- 0.10


intercept_effects <- rnorm(n = n_groups, 
                           mean = intercept_mean, 
                           sd = intercept_sd)

slope_effects <- rnorm(n = n_groups, 
                           mean = slope_mean, 
                           sd = slope_sd)

all_effects <- c(intercept_effects, slope_effects)

lin_pred <- Xmat %*% all_effects
exp_p <- exp(lin_pred)/(1 + exp(lin_pred))

infected <- rbinom(n, size = N, prob = exp_p)

horse_data <- data.frame(infected, N, rain, temp, pop)

write.table(horse_data, "horse_data.csv", row.names = FALSE,
            sep = ",", quote = FALSE)





