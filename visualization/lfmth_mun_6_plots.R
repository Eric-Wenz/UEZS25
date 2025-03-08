library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)

# Original File modified in excel to include only 6 test cities, will include the control cities once informed
data <- read_excel("lfmth_mun_6_1.xlsx") |> 
  pivot_longer(cols = JAN:DEC, names_to = "Month", values_to = "Value") |> 
  mutate(Month = factor(Month, levels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                                          "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")))

# Example to show outputs for comparing two cities
Camden_Vineland_Example <- data |> 
  filter((CITY == "Camden city, NJ") | (CITY == "Vineland city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Camden city, NJ" = "red", "Vineland city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Camden vs Vineland",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Camden_Vineland_Example)

# The following is code that does not generate plots yet, but will compare each test city to their control
# once that information is figured out


# Camden
Camden_2024 <- data |> 
  filter((CITY == "Camden city, NJ") | (CITY == "Control city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Camden city, NJ" = "red", "Control city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Camden vs Control",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Camden_2024)

# Millville
Millville_2024 <- data |> 
  filter((CITY == "Millville city, NJ") | (CITY == "Control city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Millville city, NJ" = "red", "Control city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Millville vs Control",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Millville_2024)

# Vineland
Vineland_2024 <- data |> 
  filter((CITY == "Vineland city, NJ") | (CITY == "Control city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Vineland city, NJ" = "red", "Control city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Vineland vs Control",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Vineland_2024)

# Bayonne
Bayonne_2024 <- data |> 
  filter((CITY == "Bayonne city, NJ") | (CITY == "Control city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Bayonne city, NJ" = "red", "Control city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Bayonne vs Control",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Bayonne_2024)

# Jersey City
Vineland_2024 <- data |> 
  filter((CITY == "Jersey City city, NJ") | (CITY == "Control city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Jersey City city, NJ" = "red", "Control city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Jersey City vs Control",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Jersey_City_2024)

# Lakewood

Vineland_2024 <- data |> 
  filter((CITY == "Lakewood township city, NJ") | (CITY == "Control city, NJ")) |>
  ggplot(aes(x = Month, y = Value, color = CITY, group = CITY)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(aes(yintercept = ANN.AV), color = "black", linetype = "dashed", size = 0.5) +
  scale_color_manual(values = c("Lakewood township city, NJ" = "red", "Control city, NJ" = "cyan3")) +
  labs(title = "Labor Metrics Comparison: Lakewood vs Control",
       x = "Month",
       y = "Value",
       caption = "Comparison of labor force, unemployment, unemployment rate, and employment metrics") +
  facet_wrap(~`LABOR METRIC`, scales = "free_y") +
  theme_minimal()

print(Lakewood_2024)