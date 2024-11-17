

### what fraction of jobs are licensed in 2022??
### how many jobs were licensed across the US?
### total jobs across states in 2022
requirements %>% 
  filter(ltw3_regulated %in% c(0, 1)) %>% 
  group_by(state, occupation_title) %>% 
  summarize(total_states = sum(state)) %>% 
  summarize(total_job = sum(occupation_title))

### specific jobs that are newly licensed
requirements %>% 
  filter(ltw2_regulated == 0 & ltw3_regulated == 1) %>% 
  group_by(state, occupation_title) %>% 
  tally(name = "newly licensed")

### unnecessary burdens
requirements %>% 
  filter(ltw3_regulated == 1) %>% 
  arrange(desc(ltw3_days_lost)) %>% 
  group_by(occupation_title) %>% 
  select(state, ltw3_days_lost) %>% 
  View()


### data cleaning
### NAs
unlist(lapply(requirements, function(x)sum(is.na(x))))

### stats
stringent_states  <- requirements %>% 
  filter(ltw3_regulated == 1) %>% 
  group_by(state) %>% 
  count(ltw3_regulated) %>% 
  mutate(frac = n/104) %>% 
  arrange(desc(n)) %>% 
  head() %>% 
  ggplot(aes(reorder(state, -frac), frac, fill=state)) +
  geom_col() +
  labs(y="Fraction",
       x="State", title="Fraction of Jobs Licensed 2022", 
       caption="Source: Source:Institute for Justice, 3rd Edition") +
  theme_minimal()

### occupations licensed
requirements %>% 
  filter(ltw3_regulated == 1) %>% 
  group_by(state) %>% 
  count(ltw3_regulated) %>% 
  mutate(frac=n/104) %>% 
  arrange(frac) %>% 
  head() %>% 
  ggplot(aes(reorder(state, frac), frac, fill=state)) +
  geom_col() +
  labs(y="Number of Occupations Licensed",
       x="State") +
  theme_bw()

### how many licenses are there in 2022?
requirements %>% 
  filter(ltw3_regulated ==1) %>% 
  count(ltw3_regulated)

### average license per state
requirements %>% 
  filter(ltw3_regulated !=".") %>% 
  group_by(state) %>% 
  summarize(total=sum(ltw3_regulated)) %>% 
  summarize(avg=mean(total))

### which job is most regulated
requirements %>%
  filter(ltw3_regulated == 1) %>%
  group_by(occupation_title) %>%
  summarize(n_states = n_distinct(state)) %>%
  arrange(desc(n_states)) %>%
  head(n=13)

licensed_in_all_states <- requirements %>%
  filter(ltw3_regulated == 1) %>% 
  group_by(occupation_title) %>% 
  filter(n_distinct(state) == 51)

# Count the number of unique occupation codes
n_jobs_licensed_in_all_states <- n_distinct(licensed_in_all_states$occupation_title)
n_jobs_licensed_in_all_states

library(kableExtra)

# Your data manipulation code
table_data <- requirements %>% 
  filter(ltw3_regulated == 1) %>% 
  group_by(state) %>% 
  count(ltw3_regulated) %>% 
  select(state, n) %>% 
  rename(`Regulated Jobs` = "n") %>% 
  arrange(desc(`Regulated Jobs`)) %>% 
  head(n = 10)

# Create HTML table
table_html <- table_data %>%
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

### occupations were licensed in 2012 & 2017
library(knitr)
install.packages("kableExtra")
requirements %>% 
  filter(ltw3_regulated== 1 & ltw3_regulated != ".") %>% 
  group_by(occupation_title) %>% 
  summarize(states = sum(as.numeric(ltw3_regulated))) %>% 
  arrange(desc(states)) %>% 
  head(n=10) %>% 
  kable(format="html", align=c("l", "r")) %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::row_spec(0, bold = T)

### number of jobs that don't require a license per state
requirements %>% 
  filter(ltw3_regulated== 0 & ltw3_regulated != ".") %>% 
  group_by(state) %>% 
  summarize(jobs = n_distinct(occupation_title)) %>% 
  group_by(state) %>% 
  select(state, jobs) %>% 
  arrange(jobs)