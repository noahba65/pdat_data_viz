library(tidyverse)
library(readxl)
library(scales)  
library(glue)

mohealth_raw <- read_xls("module_1/2016_MO_CLS_PUF_w_DEMOGRAPHICS.xls")


# Clean county health data
mohealth <- mohealth_raw %>%
  
  
  mutate(urban_score = as.factor(RUCC2013), # Convert Urban Score to factor
         flu_vax_rate = (100 - FLUSHOT_NOP) / 100, # Switch pct no to pct yes for flu vax and convert to rate
         insured_rate = (100 - HLTHINS_NOP) / 100) %>% # Switch pct no to pct yes for insurance and convert to rate
  
  # Select only the columns that will be used for analysis
  select(urban_score, flu_vax_rate, insured_rate) %>%
  
  # Compute median vaccination and insurance rates by urban score
  group_by(urban_score) %>%
  group_by(urban_score) %>%
  mutate(
    median_rate_flu_vax = median(flu_vax_rate, na.rm = TRUE),
    median_rate_insured = median(insured_rate, na.rm = TRUE)
  ) %>%
  ungroup()
  

# Create boxplot comparing all 9 urban continuum types
ggplot(data = mohealth) +
  
  # Generate boxplot
  geom_boxplot(aes(x = urban_score, y = flu_vax_rate, fill = median_rate_insured), 
               color = "black", alpha = .6) +
  
  # Set gradient labels to Truman purple to white and convert to percents
  scale_fill_gradient(high = "#510C76", low = "white", labels = percent_format()) +  # Percent format for legend
  
  # Convert y labels to percent using the scales package
  scale_y_continuous(labels = percent_format()) +  # Use percent formatting
  
  # Set labels
  labs(x = "Urban Score", y = "Vaccination Rate", fill = "Insurance\nRate",
       title = "Vaccination Rate", subtitle = "Comparing Urban and Rural Counties") +
  
  # Edit theme
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.title.align = 0,  # Centers the title of the fill legend
    plot.title = element_text(size = 30, face = "bold"), # Adjust plot title font and height and make it bold
    plot.subtitle = element_text(size = 24, face = "italic"), # Adjust plot sub-title font and height and make it bold
    axis.title = element_text(size = 24, face = "bold"),  # Adjust axis title font size
    axis.text = element_text(size = 24),  # Adjust axis text font size
    legend.text = element_text(size = 20),  # Adjust legend text font size
    legend.title = element_text(size = 22, face = "bold"),  # Adjust legend title font size
    
    legend.key.size = unit(1, "cm")  # Adjust size of legend keys (optional)

    )

ggsave("vaccination_rate.png", width = 10, height = 7 ) 

# Assuming 'insurance_rate' is a column in your dataset that you want to use for fill
mohealth %>%
  filter(urban_score %in% c(1, 9)) %>%
  
  # Iteratively mutate median insured rate labels to be rounded and show as percents
  mutate(median_insured_rate_label = median_rate_insured * 100,
         median_insured_rate_label = round(median_insured_rate_label),
         median_insured_rate_label = glue("{median_insured_rate_label}%")) %>%
  
  ggplot(aes(x = urban_score, y = flu_vax_rate, fill = median_insured_rate_label)) +  # Use insurance_category for fill
  geom_boxplot(color = "black", alpha = 0.6) +
  scale_fill_manual(values = c( "86%" = "white", "90%" = "#510C76")) +  # Map colors to categories
  scale_y_continuous(labels = percent_format()) +  # Use percent formatting for y-axis
  scale_x_discrete(labels = c("1" = "Most Urban\nCounties", "9" = "Most Rural\nCounties")) +  # Custom x-axis labels
  
  labs(x = "", y = "Vaccination Rate", fill = "Insurance\nRate",
       title = "Vaccination Rate", subtitle = "Comparing Urban and Rural Counties") +

  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 24, face = "italic"),
    axis.text.x = element_text(size = 24, face = "bold", color = "black"),
    axis.title = element_text(size = 24, face = "bold"),  # Adjust axis title font size
    axis.text = element_text(size = 24),  # Adjust axis text font size
    legend.text = element_text(size = 20),  # Adjust legend text font size
    legend.title = element_text(size = 22, face = "bold")  # Adjust legend title font size
  )


ggsave("vaccination_rate_trimmed.png", width = 10, height = 7 ) 
