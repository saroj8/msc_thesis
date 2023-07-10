# here we are calculating filed biomass using improved equation by Chave et al 2005
library(readxl)
library(tidyverse)
library(dplyr)
data<-read_excel("F:\\2078 and 79\\MSc\\4th sem\\my_data\\analysed data.xlsx")
data1<-data.frame(data)
data1

class(data1$DBH)
class((data1$Height))
data1$Height<-as.numeric(data1$Height)

class(data1$Specific_gravity)
#calculate abg for all species

abg_results <- data1 %>%
  filter(Plot.No >= 1 & Plot.No <= 51) %>%
  group_by(Plot.No) %>%
  mutate(abg = 0.0673 *(DBH^2 * Specific_gravity * Height)^0.976) %>%
  ungroup()
abg_results

#abg mean
abg_mean <- filter(abg_results, Plot.No == 8)
abg_mean
mean(abg_mean$abg)
# try
sum_values <- c()

for (plot_num in 1:51) {
  abg_mean <- filter(abg_results, Plot.No == plot_num)
  sum_value <- sum(abg_mean$abg)
  sum_values <- c(sum_values, sum_value)
}

sum_values_df <- data.frame(Plot.No = 1:51, Sum_abg = sum_values)
# export this mean abg 
file_path<- "F:\\2078 and 79\\MSc\\4th sem\\my_data\\sum_abg.csv"
write.csv(sum_values_df, file = file_path, row.names = FALSE)

