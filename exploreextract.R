library(ipumsr)
library(tidyverse)
library(ggthemes)
library(gt)
library(janitor)
library(rstanarm)
library(broom.mixed)
library(gtsummary)
library(tidybayes)

# read in the ipums data 

ddi <- read_ipums_ddi("raw_data/nhis_00001.xml")
sleep_data <- read_ipums_micro(ddi)

ddi <- read_ipums_ddi("raw_data/meps_00002.xml")
costaccess_data <- read_ipums_micro(ddi)

# rename the cost variables

costs <- costaccess_data %>%
  rename(direct_pay = EXPTOT,
         self_pay = EXPSELFPAY,
         total = CHGTOT) 

# find the median for each year
# Choosing median over mean because I'm wary of outliers throwing off range

median_costs <- costs %>%
  group_by(YEAR) %>%
  summarize(median(total),
            median(direct_pay),
            median(self_pay),
            .groups = "drop") %>%
  rename(total = `median(total)`,
         self_pay = `median(self_pay)`,
         direct_pay = `median(direct_pay)`) %>%
  select(YEAR,
         direct_pay,
         self_pay,
         total) 

# Plot the medians. We need to stack a few to get them to appear on one plot.
# source definitions and paste them here in a comment for explanatory clarity

median_plot <- median_costs %>%
  ggplot(aes(x = YEAR,
             y = total)) +
  geom_point() +
  geom_line() +
  geom_point(aes(x = YEAR,
                 y = direct_pay)) +
  geom_line(aes(x = YEAR,
                y = direct_pay)) +
  geom_point(aes(x = YEAR,
                 y = self_pay)) +
  geom_line(aes(x = YEAR,
                y = self_pay)) + 
 annotate("text", x = 2016, y = 1200, label = "Total Cost") +
 annotate("text", x = 2014, y = 450, label = "Direct Payments") +
 annotate("text", x = 2011, y = 150, label = "Out of Pocket") +
  labs(title = "Annual Cost of Medical Care in the US (median)",
       subtitle = "2016 saw an uptick in all categories",
       x = "",
       y = "Amount Paid in USD",
       caption = "Source: IPUMS") +
  theme_classic()
  

# if I log my full data I'll need to remove the zeros first, like we did here: 

costsmall <- costs %>% 
  filter(total > 0) %>%
  slice_sample(n = 25000)

fit_1 <- stan_glm(costsmall,
         formula = log(total) ~ self_pay + YEAR + self_pay*YEAR,
         family = gaussian,
         refresh = 0,
         seed = 288)

# question: 
# do people who cause more medical expenditures pay more or less 
# out of pocket year over year?

# ~~~~~~~~~~~~~~~

# all of these are reasons for "no usual source of care"
# isolate the access tibble, and rename the variables
# codes: 0 = n/a, 1 = no, 2 = yes
# we need to re-code to standard binary so our models are more interpretable. 
# But first, rename so it's more intuitive

costs_clean <- costs %>%
  select(YEAR, 
         NOUSLYDKWHER, 
         NOUSLYDRMOV, 
         NOUSLYFAR, 
         NOUSLYLANG, 
         NOUSLYNOLIKE,
         NOUSLYNONEED, 
         NOUSLYOTH,
         NOUSLYJOB, 
         NOUSLYNOINS,
         total,
         self_pay,
         direct_pay) %>% 
  rename(where = NOUSLYDKWHER,
         doc_moved = NOUSLYDRMOV,
         far = NOUSLYFAR, 
         language = NOUSLYLANG, 
         dislike_doc = NOUSLYNOLIKE,
         noneed_doc = NOUSLYNONEED, 
         other = NOUSLYOTH,
         jobrelated = NOUSLYJOB, 
         noinsurance = NOUSLYNOINS)

# recode all of the binary variables to NA, 0, 1
# There are some coded as "unknown" and I've decided it's reasonable to recode these as NA 

access_clean <- costs_clean %>%
    mutate(where = case_when(where == 1 ~ "No",
                           where == 2 ~ "Yes",
                           TRUE ~ NA_character_),
           doc_moved = case_when(doc_moved == 1 ~ "No",
                               doc_moved == 2 ~ "Yes",
                               TRUE ~ NA_character_),
         far = case_when(far == 1 ~ "No",
                          far == 2 ~ "Yes",
                         TRUE ~ NA_character_),
         language = case_when(language == 1 ~ "No",
                              language == 2 ~ "Yes",
                              TRUE ~ NA_character_),
         dislike_doc = case_when(dislike_doc == 1 ~ "No",
                                 dislike_doc == 2 ~ "Yes",
                                 TRUE ~ NA_character_),
         noneed_doc = case_when(noneed_doc == 1 ~ "No",
                                noneed_doc == 2 ~ "Yes",
                                TRUE ~ NA_character_),
         other = case_when(other == 1 ~ "No",
                           other == 2 ~ "Yes",
                           TRUE ~ NA_character_),
         jobrelated = case_when(jobrelated == 1 ~ "No",
                                jobrelated == 2 ~ "Yes",
                                TRUE ~ NA_character_),
         noinsurance = case_when(noinsurance == 1 ~ "No",
                                 noinsurance == 2 ~ "Yes",
                                 TRUE ~ NA_character_))

# a few of the questions I'm exploring: 
# how would all of these access variables impact total costs and out of pocket costs? 

# note on model significance: 
# if SDx2 is < Median the model is significant (?)

# next steps
# run with larger sets of data
# run loocompare and select the models that most significantly predict total cost and self pay
# run these models on the full data 
# based on the access variables, what can we learn about potential health care costs and expenditures? 

# select 2 fits, save the output as RDS. Load 

# x <- access_clean %>%
#   filter(total > 0) %>%
#   slice_sample(n = 100000)

# I'm swapping in y for the models that seemed most interesting 

y <- access_clean %>%
  filter(total > 0) 

fit_2 <- stan_glm(x,
                  formula = log(total) ~ language + self_pay,
                   refresh = 0,
                   family = gaussian,
                   seed = 254)

# saveRDS(fit_2, file = "fit_2.rds")
# this is the line to save the output of my fit. Then I move it to clean_data folder 
# within my app, I call readRDS("(foldername)/fit_2.RDS") assign to an object
# don't put rstanarm in the app or the app won't publish 

fit_3 <- stan_glm(x,
                 formula = log(total) ~ language + noinsurance + self_pay + self_pay*jobrelated,
                 refresh = 0,
                 family = gaussian,
                 seed = 254)

fit_4 <- stan_glm(y,
                  formula = log(total) ~ self_pay + where + doc_moved + far + direct_pay*doc_moved,
                  refresh = 0,
                  family = gaussian,
                  seed = 254)

# fit_4 is the preferred model according to my loo_compare

fit_5 <- stan_glm(x,
                  formula = self_pay ~ total + noinsurance + jobrelated + noneed_doc + other +
                            total*jobrelated,
                  refresh = 0,
                  family = gaussian,
                  seed = 254)

fit_6 <- stan_glm(x,
                  formula = self_pay ~ total + direct_pay + doc_moved +
                  noinsurance*jobrelated,
                  refresh = 0,
                  family = gaussian,
                  seed = 254)

# fit_6 is not running, producing the Warning: Warning message:
# In center_x(x, sparse) :
#   Dropped empty interaction levels: noinsuranceYes:jobrelatedYes 

fit_7 <- stan_glm(y,
                  formula = log(total) ~ self_pay + YEAR + self_pay*noinsurance,
                  family = gaussian,
                  refresh = 0,
                  seed = 288)

# Access posteriors: what is the probability of being impacted by various barriers to care?

loo1 <- loo(fit_1, k_threshold = 0.7)
loo2 <- loo(fit_2, k_threshold = 0.7)
loo3 <- loo(fit_3, k_threshold = 0.7)
loo4 <- loo(fit_4, k_threshold = 0.7)
loo5 <- loo(fit_5, k_threshold = 0.7)
# loo6 <- loo(fit_6, k_threshold = 0.7)
loo7 <- loo(fit_7, k_threshold = 0.7)

loo_compare(loo2, loo3, loo4, loo5, loo7)
compare <- loo_compare(loo4, loo7)


# ~~~~~~~~~~~~~~~

# sleep data exploration 
# recoding data such that all those with more than 7 hours of sleep = Optimal,
# less than 5 hours of sleep = "Deprived", and 6 hours = suboptimal 
# (Double check with American Sleep Society this maps to recs)

ideal_sleep <- sleep_data %>% 
  mutate(HRSLEEP = case_when(HRSLEEP >= 7 ~ "Optimal",
                             HRSLEEP < 5  ~ "Deprived",
                             HRSLEEP == 6 ~ "Suboptimal",
                             TRUE ~ NA_character_)) %>%
  select(YEAR, HRSLEEP)

# sleep_costs <- inner_join(ideal_sleep, costs_clean)

# Error: vector memory exhausted (limit reached?)
# I need to make these tibbles smaller so I can build a model and not crash my machine
# If something is interesting I'll scale on FAS-ondemand 

ccs <- costs_clean %>%
  slice_sample(n = 30000)

iss <- ideal_sleep %>%
  slice_sample(n = 30000)

sleep_costs <- inner_join(ccs, iss) %>%
  select(YEAR, HRSLEEP, direct_pay, self_pay, total)

scs <- sleep_costs %>%
  slice_sample(n = 100000) %>%
  filter(total > 0)

fit_8 <- stan_glm(scs,
                  formula = log(total) ~ YEAR + HRSLEEP + self_pay*HRSLEEP,
                  family = gaussian, 
                  refresh = 0, 
                  seed = 325)

# let's look at a couple fits in table form 

tbl_fit_4 <- tbl_regression(fit_4, 
               intercept = TRUE, 
               estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = md("**Geographic Barriers to Healthcare**"),
             subtitle = "What's the relationship between cost and access?") %>%
  tab_source_note(md("Source: IPUMS")) %>% 
  cols_label(estimate = md("**Parameter**"))

tbl_fit_8 <- tbl_regression(fit_8, 
               intercept = TRUE, 
               estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = md("**Sleep and Health Care Costs**"),
             subtitle = "Those who get optimal sleep cost less?") %>%
  tab_source_note(md("Source: IPUMS")) %>% 
  cols_label(estimate = md("**Parameter**"))


# I built these tables as attempts at interpretation, but they may be off.
# maybe my classmates can help me during demo day? 

# I'm not sure if either of these models are accurate enough to build posteriors, but if they are:

# fit_4 formula = log(total) ~ self_pay + where + doc_moved + far + direct_pay*doc_moved,

#~~~ 

# tidybayes 

total <- unique(y$total)
self_pay <- unique(y$self_pay)
where <- unique(y$where)
doc_moved <- unique(y$doc_moved)
far <- unique(y$far)
direct_pay <- unique(y$direct_pay)            

newobs <- expand_grid(total, self_pay, where, doc_moved, far, direct_pay) %>%
  as_tibble()

