main_data <- read.csv("main_data.csv", header = TRUE, sep = ";")
pilot_data <- read.csv("pilot_data.csv", header = TRUE, sep = ";")

#making a graph for the pilot findings to visualize the difference in proportions
#I will use some code that I did in the Project_code, to get the proportions in each stratum 

result_table <- aggregate(Belongings ~ Sampled_unit + Strata, data = pilot_data, FUN = sum)
result_table$proportion <- result_table$Belongings / 624
strata_summary <- aggregate(proportion ~ Strata, data = result_table, FUN = mean)
print(strata_summary)

#creating a bar chart to look at the proportions for the pilot
strata_names <- c("Morning", "Lunchtime", "Afternoon")
pilot_proportions <- c(strata_summary$proportion[strata_summary$Strata == "morning"], strata_summary$proportion[strata_summary$Strata == "lunchtime"], strata_summary$proportion[strata_summary$Strata == "afternoon"])

barplot(pilot_proportions, 
        names.arg = strata_names, 
        ylim = c(0, 0.20),
        col = "coral",
        main = "Proportion of Belongings by Strata in Pilot Survey",
        xlab = "Strata",
        ylab = "Estimated Proportion")

#Now doing the same for the main survey

result_table_2 <- aggregate(Belongings ~ Sampled_unit + Strata, data = main_data, FUN = sum)
result_table_2$proportion <- result_table_2$Belongings / 624
strata_summary_2 <- aggregate(proportion ~ Strata, data = result_table_2, FUN = mean)
print(strata_summary_2)

main_proportions <- c(strata_summary_2$proportion[strata_summary_2$Strata == "morning"], strata_summary_2$proportion[strata_summary_2$Strata == "lunchtime"], strata_summary_2$proportion[strata_summary_2$Strata == "afternoon"])


barplot(main_proportions, 
        names.arg = strata_names, 
        ylim = c(0, 0.20),
        col = "darkseagreen1",
        main = "Proportion of Belongings by Strata in Main Survey",
        xlab = "Strata",
        ylab = "Estimated Proportion")

#Now to compare computer seats with non-computer seats, and how the proportions behave for these 2 different types of seats

data_computer <- main_data[main_data$Computer == 1, ]

#calculating the proportion within each stratum, just as above, but only looking at computer seats
#On Levels 2 and 4, there are 99 computer seats in total
result_table_comp <- aggregate(Belongings ~ Sampled_unit + Strata, data = data_computer, FUN = sum)
result_table_comp$proportion <- result_table_comp$Belongings / 99
strata_summary_comp <- aggregate(proportion ~ Strata, data = result_table_comp, FUN = mean)
print(strata_summary_comp)

comp_proportions <- c(strata_summary_comp$proportion[strata_summary_comp$Strata == "morning"], strata_summary_comp$proportion[strata_summary_comp$Strata == "lunchtime"], strata_summary_comp$proportion[strata_summary_comp$Strata == "afternoon"])

barplot(comp_proportions, 
        names.arg = strata_names, 
        ylim = c(0, 0.20),
        col = "darkolivegreen3",
        main = "Proportion of Belongings by Strata for Computer Seats",
        xlab = "Strata",
        ylab = "Estimated Proportion")

#doing the same for non-computer seats (normal seats)
#there are 525 normal seats

data_normal <- main_data[main_data$Computer == 0, ]
result_table_norm <- aggregate(Belongings ~ Sampled_unit + Strata, data = data_normal, FUN = sum)
result_table_norm$proportion <- result_table_norm$Belongings / 525
strata_summary_norm <- aggregate(proportion ~ Strata, data = result_table_norm, FUN = mean)
print(strata_summary_norm)

norm_proportions <- c(strata_summary_norm$proportion[strata_summary_norm$Strata == "morning"], strata_summary_norm$proportion[strata_summary_norm$Strata == "lunchtime"], strata_summary_norm$proportion[strata_summary_norm$Strata == "afternoon"])

barplot(norm_proportions, 
        names.arg = strata_names, 
        ylim = c(0, 0.20),
        col = "darkolivegreen3",
        main = "Proportion of Belongings by Strata for Non-Computer Seats",
        xlab = "Strata",
        ylab = "Estimated Proportion")

