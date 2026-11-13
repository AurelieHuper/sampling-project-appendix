pilot_data <- read.csv("pilot_data.csv", header = TRUE, sep = ";")

#Summing up, all of my belongings for each sampled unit, and also grouping them by strata
result_table <- aggregate(Belongings ~ Sampled_unit + Strata, data = pilot_data, FUN = sum)

#Calculating the proportion for each sampled unit
result_table$proportion <- result_table$Belongings / 624

print(result_table)

#Calculating the mean proportion within each strata
strata_summary <- aggregate(proportion ~ Strata, data = result_table, FUN = mean)

#Add N_j, n_j, W_j, f_j in each strata to the summary, as they will be needed for further calculations
#Also note, R organised the strata in alphabetical order, so I am using that order when inputting the required values
strata_summary$N_j[strata_summary$Strata == "morning"] <- 30
strata_summary$N_j[strata_summary$Strata == "lunchtime"] <- 20
strata_summary$N_j[strata_summary$Strata == "afternoon"] <- 30
strata_summary$W_j <- strata_summary$N_j / 80
strata_summary$n_j <- 2
strata_summary$f_j <- strata_summary$n_j / strata_summary$N_j

#To calculate the sample variance within each strata, I need to sum up (proportion_sampled_unit - proportion_strata)^2 for all sampled units
result_table$squared_diff <- (result_table$proportion - strata_summary$proportion[match(result_table$Strata, strata_summary$Strata)])^2
sum_squared_diff <- aggregate(squared_diff ~ Strata, data = result_table, FUN = sum)
strata_summary$s_j_squared <- sum_squared_diff$squared_diff[match(strata_summary$Strata, sum_squared_diff$Strata)] / (strata_summary$n_j - 1)

#To calculate pilot estimates (point estimate for the mean of our proportions and estimate for the variance):
#I need to calculate W_j*proportion_j in each strata for the estimated mean of the proportions
#I need (1-f_j)*W_j^2*s_j^2/n_j in each strata for the estimated variance 
strata_summary$mu_component <- strata_summary$W_j * strata_summary$proportion
strata_summary$var_component <- (1 - strata_summary$f_j) * (strata_summary$W_j^2) * strata_summary$s_j_squared / strata_summary$n_j

mean_proportion_estimate <- sum(strata_summary$mu_component)
variance_estimate <- sum(strata_summary$var_component)
print(paste("My pilot mean proportion point estimate is:", mean_proportion_estimate))
print(paste("My pilot variance estimate is:", variance_estimate))

#My goal SE for the main survey is half of the pilot SE
pilot_SE <- sqrt(variance_estimate)
SE_goal <- pilot_SE / 2
Var_goal <- (SE_goal)^2
print(paste("My variance goal for the main survey is:", Var_goal))

#Now, I need to calculate the required sample size to achieve that goal variance
#With my group, we considered doing both, proportional allocation and Neyman Optimal Allocation.
#We decided to use proportional allocation (see explanation in the report).
#We had initially calculated the required sample sizes for disproportional allocation on Excel, and I mentioned them briefly in my report. 
#However, I won't do the calculations here, since we decided against disproportional allocation.
#These are the calculations and results using proportional allocation

strata_summary$W_jxs_j_squared <- strata_summary$W_j * strata_summary$s_j_squared
required_n <- (80*sum(strata_summary$W_jxs_j_squared)) / (80*Var_goal + sum(strata_summary$W_jxs_j_squared))
print(paste("My required sample size is n = ", required_n))

#Now I calculate, the required n within each strata, using n_j_required = n*W_j

strata_summary$n_j_required <- required_n * strata_summary$W_j
print(strata_summary)

#So rounding up each n_j (since I want my variance to be equal or smaller than Var_goal)
print("I need n = 8 samples for the morning; n = 5 samples for lunchtime and n = 8 samples for the afternoon")

#Now i can calculate my main survey estimates

main_data <- read.csv("main_data.csv", header = TRUE, sep = ";")

#Using the same method, I'm adding my belongings and dividing them by the total amount of seats.
result_table_2 <- aggregate(Belongings ~ Sampled_unit + Strata, data = main_data, FUN = sum)
result_table_2$proportion <- result_table_2$Belongings / 624

print(result_table_2)

#calculating the mean proportion within each strata
strata_summary_2 <- aggregate(proportion ~ Strata, data = result_table_2, FUN = mean)

#adding required information for my calculations to my dataset
strata_summary_2$N_j[strata_summary_2$Strata == "morning"] <- 30
strata_summary_2$N_j[strata_summary_2$Strata == "lunchtime"] <- 20
strata_summary_2$N_j[strata_summary_2$Strata == "afternoon"] <- 30

strata_summary_2$W_j <- strata_summary_2$N_j / 80

strata_summary_2$n_j[strata_summary_2$Strata == "morning"] <- 8
strata_summary_2$n_j[strata_summary_2$Strata == "lunchtime"] <- 5
strata_summary_2$n_j[strata_summary_2$Strata == "afternoon"] <- 8

strata_summary_2$f_j <- strata_summary_2$n_j / strata_summary_2$N_j

#(proportion_sampled_unit - proportion_strata)^2 for all sampled units calculation
result_table_2$squared_diff <- (result_table_2$proportion - strata_summary_2$proportion[match(result_table_2$Strata, strata_summary_2$Strata)])^2
sum_squared_diff_2 <- aggregate(squared_diff ~ Strata, data = result_table_2, FUN = sum)
strata_summary_2$s_j_squared <- sum_squared_diff_2$squared_diff[match(strata_summary_2$Strata, sum_squared_diff_2$Strata)] / (strata_summary_2$n_j - 1)

print(strata_summary_2)

#calculating the components I will need for my point and variance estimates
strata_summary_2$mu_component <- strata_summary_2$W_j * strata_summary_2$proportion
strata_summary_2$var_component <- (1 - strata_summary_2$f_j) * (strata_summary_2$W_j^2) * strata_summary_2$s_j_squared / strata_summary_2$n_j

#calculating the estimates
mean_proportion_estimate_2 <- sum(strata_summary_2$mu_component)
variance_estimate_2 <- sum(strata_summary_2$var_component)
print(paste("My mean proportion point estimate is:", mean_proportion_estimate_2))
print(paste("My variance estimate is:", variance_estimate_2))

#now calculating a 95% confidence interval
SE_estimate_2 <- sqrt(variance_estimate_2)
#using a t distribution because our sample size is less than 50
t_val <- qt(0.975, df = 20)
lower_bound <- mean_proportion_estimate_2 - (t_val*SE_estimate_2)
upper_bound <- mean_proportion_estimate_2 + (t_val*SE_estimate_2)

print(paste("We are 95% confident that the interval (", lower_bound,", ",upper_bound,") contains the true population proportion of seats taken up by solely belongings"))

SEmain_over_SEpilot <- SE_estimate_2/pilot_SE
print(paste("SE_main/SE_pilot = ",SEmain_over_SEpilot))

Reduction_SE <- (pilot_SE - SE_estimate_2)*100 / pilot_SE
print(paste("Reduction in SE: ",Reduction_SE))
