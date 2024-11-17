

### relative comparison and Z-Scores
burden_by_state <- requirements %>% 
  filter(ltw2_days_lost != "." & ltw2_fees != "." & ltw2_min_age != "." & ltw2_min_grade != "." & ltw2_exams != "." & ltw3_days_lost != "." & ltw3_fees != "." & ltw3_min_age != "." & ltw3_min_grade != "." & ltw3_exams != ".") %>% 
  group_by(state) %>% 
  summarize(
    # calculate z-scores for each burden component
    z_days_lost = (as.numeric(ltw2_days_lost) - mean(as.numeric(ltw2_days_lost))) / sd(as.numeric(ltw2_days_lost)),
    z_fees = (as.numeric(ltw2_fees) - mean(as.numeric(ltw2_fees))) / sd(as.numeric(ltw2_fees)),
    z_min_age = (as.numeric(ltw2_min_age) - mean(as.numeric(ltw2_min_age))) / sd(as.numeric(ltw2_min_age)),
    z_min_grade = (as.numeric(ltw2_min_grade) - mean(as.numeric(ltw2_min_grade))) / sd(as.numeric(ltw2_min_grade)),
    z_exams = (as.numeric(ltw2_exams) - mean(as.numeric(ltw2_exams))) / sd(as.numeric(ltw2_exams)),
    z_days_lost_two = (as.numeric(ltw3_days_lost) - mean(as.numeric(ltw3_days_lost))) / sd(as.numeric(ltw3_days_lost)),
    z_fees_two = (as.numeric(ltw3_fees) - mean(as.numeric(ltw3_fees))) / sd(as.numeric(ltw3_fees)),
    z_min_age_two = (as.numeric(ltw3_min_age) - mean(as.numeric(ltw3_min_age))) / sd(as.numeric(ltw3_min_age)),
    z_min_grade_two = (as.numeric(ltw3_min_grade) - mean(as.numeric(ltw3_min_grade))) / sd(as.numeric(ltw3_min_grade)),
    z_exams_two = (as.numeric(ltw3_exams) - mean(as.numeric(ltw3_exams))) / sd(as.numeric(ltw3_exams))
  ) %>% 
  mutate(
    # calculate weighted t-scores for each burden component
    t_days_lost = (z_days_lost + z_days_lost_two) / sqrt(2),
    t_fees = (z_fees + z_fees_two) / sqrt(2),
    t_min_age = (z_min_age + z_min_age_two) / sqrt(2),
    t_min_grade = (z_min_grade + z_min_grade_two) / sqrt(2),
    t_exams = (z_exams + z_exams_two) / sqrt(2),
    # calculate weighted average t-score for each state
    weighted_t_score = (2 * t_min_grade + t_min_age + 2 * t_fees + 2 * t_days_lost + t_exams) / 10
  ) %>% 
  select(state, weighted_t_score)

burden_by_state_summed <- burden_by_state %>% 
  group_by(state) %>% 
  summarize(w_t_score = sum(weighted_t_score)) 

### create nat'l average then compare
mean(burden_by_state_summed$w_t_score)

burden_by_state_summed <- burden_by_state_summed %>% 
  mutate(relative_t_score = w_t_score - 1.435739e-16) %>% 
  arrange(desc(relative_t_score))
### weighted t score + breathe df
index <- merge(burden_by_state_summed, perc_change_breadth, by="state")


index_mean <- index %>% 
  group_by(state) %>% 
  mutate(final =  perc_change + relative_t_score)

mean(index_mean$final, na.rm=T)

### graph breathe and burden index
# Create a new column "category" based on final value being above or below 0
index_mean %>%
  ggplot(aes(reorder(state, final), final, fill = factor(sign(final)))) +  # Use factor(sign(final)) to map positive and negative values to colors
  geom_col() +  # Add black borders to the bars
  coord_flip() + 
  geom_hline(yintercept = -0.547, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_text(aes(x = "South Dakota", y = -2, label = ""),
            angle = 90, size=5) +  
  labs(x = "", y = "Percent", subtitle="Change from 2012-2017", title = "Breathe and Burden Index",
       caption="Source: Institute for Justice, 3rd Edition") +
  theme(legend.position = "none")

scale_fill_manual(values = c("red", "blue"), 
                  labels = c("Above 0", "Below 0")) +  # Update labels
  scale_y_continuous(limits = c(-5, 25), breaks = seq(-5, 25, 5)) +
  theme_minimal() 
theme(legend.position = "none")

# Merge burden_by_state_summed and perc_change_breadth by state
index <- merge(burden_by_state_summed, perc_change_breadth, by = "state")

# Calculate the final index_mean
index_mean <- index %>%
  mutate(final = perc_change + relative_t_score)

# Calculate final value based on your formula

### ?? 2 relative t-score per state ? so we're looking at the percent change in relation t score?
## relative t score is the difference to the mean.
index <- index %>% 
  mutate(final = (((first + relative_t_score) - second + relative_t_score) / 
                    (first + relative_t_score)) * 100)

# Create the plot
index %>%
  ggplot(aes(reorder(state, final), final)) +
  geom_col() +
  coord_flip() + 
  geom_hline(yintercept = -0.547, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_text(aes(x = "South Dakota", y = -2, label = "average change = -0.5%"),
            angle = 90) +  
  labs(x = "State", y = "Percent", title = "Breathe and Burden Index",
       caption = "Source: Institute for Justice, 3rd Edition") +
  scale_fill_manual(values = c("red", "blue"), 
                    labels = c("Below 0", "Above 0"), 
                    guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(-5, 25), breaks = seq(-5, 25, 5)) +
  theme_minimal() +
  guides(fill = FALSE)
