

### burdens
### average burden by burden type
requirements %>% 
  filter(ltw1_days_lost != ".") %>% 
  summarize(average=mean(as.numeric(ltw1_days_lost), na.rm=T))

requirements %>% 
  filter(ltw3_regulated != ".") %>% 
  group_by(occupation_title) %>% 
  summarize(jobs = n_distinct(occupation_title)) %>% 
  count()

### average days lost and fees required
requirements %>%
  select(ltw1_days_lost, ltw1_fees) %>%
  mutate_all(as.numeric) %>% 
  colMeans(na.rm=T)

### licensed jobs in 2017
requirements %>% 
  filter(ltw3_regulated == 1) %>% 
  group_by(occupation_title) %>% 
  summarize(licenses=sum(ltw3_regulated)) %>% 
  arrange(licenses)

### burden for shampooers
requirements %>% 
  filter(ltw3_regulated == 1 & occupation_title=="Shampooer") %>% 
  summarize(days_lost = mean(as.numeric(ltw3_days_lost)),
            fees = mean(as.numeric(ltw3_fees)),
            grade = mean(as.numeric(ltw3_min_grade)),
            age=mean(as.numeric(ltw2_min_age)))


### change in burden requirements over time
fees_by_year <- requirements %>%
  filter(ltw2_fees != ".", ltw3_fees != ".") %>%
  group_by(state) %>%
  summarize(first_avg_fees = mean(as.numeric(ltw2_fees)),
            second_avg_fees= mean(as.numeric(ltw3_fees))) %>% 
  mutate(change_fees = ((second_avg_fees - first_avg_fees)/first_avg_fees) *100)

days_lost_by_year <- requirements %>%
  filter(ltw2_days_lost != ".", ltw3_days_lost != ".") %>%
  group_by(state) %>%
  summarize(first_avg_days_lost = mean(as.numeric(ltw2_days_lost)),
            second_avg_days_lost = mean( as.numeric(ltw3_days_lost))) %>% 
  mutate(change_avg_days_lost = ((second_avg_days_lost - first_avg_days_lost) /
                                   first_avg_days_lost)*100)

change_by_year <- fees_by_year %>%
  inner_join(days_lost_by_year, by = "state")
change_by_year %>% 
  select(change_fees) %>% 
  arrange(desc(change_fees))

ggplot(change_by_year) +
  geom_histogram(aes(change_fees, fill="Fees"), binwidth = 2, color="white", boundary=0) +
  geom_histogram(aes(change_avg_days_lost, fill="Days Lost"), binwidth=2, color="white",boundary=0) +
  scale_fill_manual(values=c("blue", "black"), labels=c("Fees", "Days Lost")) +
  labs(x="Percent Change", y="Number of States", title="Change in Burden 2017-2022") 

### breadth and burden measures
### breadth
occupation_change  <- requirements %>%
  filter(ltw3_regulated != "." & ltw2_regulated != ".") %>%
  group_by(occupation_title) %>%
  summarize(regulation_change = ltw3_regulated - ltw2_regulated) %>% 
  group_by(occupation_title) %>% 
  summarize(change = sum(regulation_change)) 

### change in licensing per occupation btwn 2017 - 2022
licensing_change <- occupation_change %>% 
  filter(change != 0) %>%
  ggplot(aes(x = reorder(occupation_title, change), y = change, fill=change)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color="red") +
  labs(x = "Occupation Title", y = "Change in Licensing Requirements",
       title="Change in Licensing Requirements from 2017-2022") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### percent change in state licensing
#group_by(occupation_title) %>% 
#  count(occupation_title) %>% 
# arrange(desc(n))
perc_change_breadth <- requirements %>%
  group_by(state) %>% 
  reframe(`2012` = sum(ltw2_regulated==1),
          `2017`=sum(ltw3_regulated==1)) %>% 
  mutate(perc_change = ((`2017`-`2012`)/`2012`)*100)
filter(ltw2_regulated == 1, ltw3_regulated == 0) %>%
  group_by(occupation_title) %>% 
  summarize(num_states_decreased = n_distinct(state)) %>% 
  mutate(perc_change = (num_states_decreased / nrow(requirements)) * 100)

# Assuming you also want to calculate the percentage change by state
perc_change_breadth_by_state <- requirements %>%
  filter(ltw3_regulated == 1) %>%
  group_by(state) %>%
  reframe(count = n())

# summarize(num_occupations_decreased = n_distinct(occupation_title)) %>% 
#mutate(perc_change = (num_occupations_decreased / nrow(requirements)) * 100)


save(perc_change_breadth, file="perc_change_breathe.Rdata")
write.csv(perc_change_breadth, "perc_change_breathe.csv")

### looking at occupation that are licensed in later editions
newly_licensed_jobs <- requirements %>%
  filter(ltw1_regulated != "." &  ltw3_regulated != "." & ltw3_regulated == 1) %>%
  group_by(occupation_title) %>%
  summarize(num_licenses_added = sum(ltw3_regulated - as.numeric(ltw1_regulated))) %>%
  arrange(desc(num_licenses_added)) %>% 
  head() %>% 
  ggplot(aes(reorder(occupation_title, num_licenses_added), num_licenses_added, fill=occupation_title)) +
  geom_col() +
  coord_flip() + 
  labs(y="Number of States", x="Occupation", title="Occupations Where Licenses were Mandated Between 2012-2017",
       caption="Source:Institute for Justice, 3rd Edition") +
  theme_minimal()
