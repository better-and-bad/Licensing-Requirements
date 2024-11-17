library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)


theme_cx <- function(){
  theme_light() +
    theme(
      axis.text.y  = element_text(size=11),
      axis.title.x = element_text(size=14, margin = margin(t=12)),
      axis.text.x = element_text(size=10),  # Rotate x-axis labels by 45 degrees
      axis.title.y = element_text(size=14, margin = margin(r=12)),
      plot.title = element_text(size=22, face="bold", margin = margin(b=10),
                                hjust=0.5),
      plot.subtitle = element_text(size=18, face="bold", margin = margin(b=10),
                                   hjust=0.5),
      legend.text = element_text(size = 16),  # Increase the size of legend text
      
    )
}


### set the theme
theme_set(theme_cx())


### read in data
requirements <- read_excel("/Users/jackconnors/Downloads/licensing_requirements.xlsx",
                           sheet="comparison_dataset")
### how many jobs are observed ??
num_occupations_per_state <- requirements %>% 
  group_by(state) %>% 
  reframe(total_jobs = n_distinct(occupation_title))

### number of licensed jobs per state in 2022
licensed_jobs_22 <- requirements %>%
  filter(ltw3_regulated == 1) %>%
  group_by(state) %>%
  summarize(total_licensed_occupations_2022 = n_distinct(occupation_title))

mean(licensed_jobs_22$total_licensed_occupations_2022)

### number of licensed jobs per state in 2012
licensed_jobs_12 <- requirements %>%
  filter(ltw1_regulated == 1) %>%
  group_by(state) %>%
  summarize(total_licensed_occupations_2012 = n_distinct(occupation_title))

mean(licensed_jobs_12$total_licensed_occupations_2012)

### average number of licensed jobs per state
mean(licensed_jobs_per_state$total_regulated_jobs)

### STRINGENCY OF REQUIREMENTS ###
### DAYS LOST
### 2022
requirements %>%
  filter(ltw3_days_lost != ".") %>%
  summarize(average_days_lost = mean(as.numeric(ltw3_days_lost), 
                                     na.rm = TRUE))
### 2012
requirements %>%
  filter(ltw1_days_lost != ".") %>%
  summarize(average_days_lost = mean(as.numeric(ltw1_days_lost), 
                                     na.rm = TRUE))
### FEES DEMANDED ###
### 2022
requirements %>%
  filter(ltw3_fees != ".") %>%
  summarize(average_fees = mean(as.numeric(ltw3_fees), 
                                na.rm = TRUE))
### 2012
requirements %>%
  filter(ltw1_fees != ".") %>%
  summarize(average_fees = mean(as.numeric(ltw1_fees), 
                                na.rm = TRUE))

### EXAMS ###
### 2022
requirements %>%
  filter(ltw3_exams != "." & ltw3_exams > 0) %>%
  summarize(exam_job = n_distinct(occupation_title))
summarize(average_exams = mean(as.numeric(ltw1_exams), 
                               na.rm = TRUE))
