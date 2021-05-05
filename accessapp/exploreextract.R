library(ipumsr)
library(tidyverse)
library(ggthemes)
library(gt)
library(janitor)
library(rstanarm)
library(broom.mixed)
library(gtsummary)
library(tidybayes)
library(ggdist)

# this is the file I do most of my data manipulation, cleaning, and plotting to prep for my web app

# ABOUT THE DATA
# The Medical Expenditure Panel Survey (MEPS) 
# provides harmonized microdata from the longitudinal survey of 
# U.S. health care expenditures and utilization

# read in the IPUMS / MPES data 

# ddi <- read_ipums_ddi("raw_data/nhis_00001.xml")
# sleep_data <- read_ipums_micro(ddi)

ddi <- read_ipums_ddi("raw_data/meps_00002.xml")
costaccess_data <- read_ipums_micro(ddi)

# rename the cost variables

costs <- costaccess_data %>%
  rename(direct_pay = EXPTOT,
         self_pay = EXPSELFPAY,
         total = CHGTOT) 

# find the median for each year and create a new tibble with median_costs

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
         total) %>%
  pivot_longer(names_to = "type",
               values_to = "cost",
               cols = -YEAR)

# pivot_longer is the key to be able to make this "plottable", 
# so the values and names appear in separate columns

# save the rds with this median_costs tibble so I can pull just the clean data into my app. 
# Then move to the app level, into the folder "clean_data"

write_rds(median_costs, "median_costs.rds")

# I recreate this plot in the app, but here is the code for my median plot:

median_app <- median_costs %>%
  ggplot(aes(x = YEAR, 
             y = cost, 
             color = type)) +
  geom_point() +
  geom_line() +
  labs(title = "Cost of Medical Care in the United States",
       subtitle = "Healthcare costs continue to rise",
       x = NULL,
       y = "Median Cost",
       color = "Type",
       caption = "Source: IPUMS") +
  theme_classic()


# This is a different version of the same plot. 
# If I didn't use an interactive plot, I could display all three together with this code:

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
       subtitle = "What changed in 2016 that led to an uptick?",
       x = "",
       y = "Amount Paid in USD",
       caption = "Source: IPUMS") +
  theme_classic()

################################################################################
# a note on the data on access / barriers to healthcare. 

# all of the variables below are reasons for "no usual source of care"

# Variable explanations: 
# NOUSLYDKWHER	Why no usual source of care: Doesn't know where to go
# P	NOUSLYDRMOV	Why no usual source of care: Previous doctor moved or is unavailable
# P	NOUSLYFAR	Why no usual source of care: Care too far away or inconvenient
# P	NOUSLYLANG	Why no usual source of care: Speak a different language
# P	NOUSLYNOLIKE	Why no usual source of care: Doesn't like doctors
# P	NOUSLYNONEED	Why no usual source of care: Doesn't need doctor
# P	NOUSLYOTH	Why no usual source of care: Other reason
# P	NOUSLYJOB	Why no usual source of care: Reason related to job
# P	NOUSLYNOINS	Why no usual source of care: No health insurance

# To explore this data in more depth, 

# I'll isolate the access tibble, and rename each variable

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


# the ipums coders use the following logic: 
# codes: 0 = n/a, 1 = no, 2 = yes

# I decided to re-code to standard binary so my models are easier to interpret
# In the next step, I recode all of the binary variables to NA, 0, or 1
# There are some coded as "unknown" which for our purposes can be re-coded these as NA 

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


# question I'll consider: how many individuals faced each of these barriers to care, and what's the trend over time?
# For this I'll need to calculate how many "yes" per year in any given variable. There's likely a more efficient way to do this 
# drop all the No's and NA's 

access_trends <- access_clean %>%
  select(-total, -self_pay, -direct_pay) %>% 
  group_by(YEAR) %>% 
  
  add_tally(where == "Yes") %>%
  rename(where_tally = "n") %>%
  
  add_tally(doc_moved == "Yes") %>%
  rename(doc_moved_tally = "n") %>%
  
  add_tally(far == "Yes") %>%
  rename(far_tally = "n") %>%
  
  add_tally(language == "Yes") %>%
  rename(language_tally = "n") %>%
  
  add_tally(dislike_doc == "Yes") %>%
  rename(dislike_doc_tally = "n") %>%
  
  add_tally(noneed_doc == "Yes") %>%
  rename(noneed_doc_tally = "n") %>%
  
  add_tally(other == "Yes") %>%
  rename(other_tally = "n") %>%
  
  add_tally(jobrelated == "Yes") %>%
  rename(jobrelated_tally = "n") %>%
  
  add_tally(noinsurance == "Yes") %>%
  rename(noinsurance_tally = "n") %>%
  
  select(-where, -doc_moved, -far, -language, 
         -dislike_doc, -noneed_doc, 
         -other, -jobrelated, -noinsurance) %>% 
  
  # only one row per year
  
  slice_head(n = 1) %>%
  
  # rename with original variable names for ease of interpretation
  
  rename(where = where_tally,
         doc_moved = doc_moved_tally,
         far = far_tally, 
         language = language_tally,
         dislike_doc = dislike_doc_tally,
         noneed_doc = noneed_doc_tally,
         other = other_tally, 
         jobrelated = jobrelated_tally, 
         noinsurance = noinsurance_tally) %>%
  pivot_longer(names_to = "barriers",
               values_to = "count",
               cols = -YEAR) %>%
  filter(YEAR != 2018)

write_rds(access_trends, file = "access_trends.rds")

# remove 2018 because no data exists 
 
# plot access_trends. I create this live in my app, but this is the code for a static version, for future reference

access_plot <- access_trends %>%
  ggplot(aes(x = YEAR, 
             y = count, 
             color = barriers)) +
  geom_point() +
  geom_line() +
  labs(title = "Barriers to Healthcare Access",
       subtitle = "Number of people reporting a specific barrier",
       x = NULL,
       y = "People impacted",
       color = "Barrier",
       caption = "Source: IPUMS") +
  theme_classic()


# a few of the questions I'm curious about: 
# how do barriers to care impact total costs and out of pocket healthcare expenditures? 

# to build prediction models for cost variables, I'll first need to create bins. 
# For my purposes, above vs below average may be sufficient 

x <- access_clean %>%
  filter(total > 0) %>%
  slice_sample(n = 100000)

y <- access_clean %>%
  filter(total > 0) %>%
  mutate(direct_pay = case_when(direct_pay >= mean(direct_pay) ~ "above_ave",
                           direct_pay < mean(direct_pay) ~ "below_ave"),
         self_pay = case_when(self_pay >= mean(self_pay) ~ "above_ave",
                              self_pay < mean(self_pay) ~ "below_ave"))

# for the first model, I'll explore only cost variables.
# note that I filter out 0 in the total variable, as above, and create a sample of my full data set

# this next section includes all of the models I experimented with en route to my final selection (fit_4)

costsmall <- costs %>% 
  filter(total > 0) %>%
  slice_sample(n = 25000)

fit_1 <- stan_glm(costsmall,
                  formula = log(total) ~ self_pay + YEAR + self_pay*YEAR,
                  family = gaussian,
                  refresh = 0,
                  seed = 288)

# this model includes language as a barrier to care, along with out of pocket expenditures and total

fit_2 <- stan_glm(y,
                  formula = log(total) ~ language + self_pay,
                   refresh = 0,
                   family = gaussian,
                   seed = 254)

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

fit_7 <- stan_glm(y,
                  formula = log(total) ~ self_pay + YEAR + self_pay*noinsurance,
                  family = gaussian,
                  refresh = 0,
                  seed = 288)

# compare models using loo_compare. But first, each fit needs a loo object
# Not exactly sure what the k_threshold is but R reccommended it, perhaps due to machine lag

loo1 <- loo(fit_1, k_threshold = 0.7)
loo2 <- loo(fit_2, k_threshold = 0.7)
loo3 <- loo(fit_3, k_threshold = 0.7)
loo4 <- loo(fit_4, k_threshold = 0.7)
loo5 <- loo(fit_5, k_threshold = 0.7)
loo6 <- loo(fit_6, k_threshold = 0.7)
loo7 <- loo(fit_7, k_threshold = 0.7)

compare <- loo_compare(loo2, loo4,loo7)

# it seems fit_4 and fit_7 are the best of the batch, and that 4 > 7
# fit_4 is the preferred model according to my loo_compare. I'll save it and move it to clean_data

saveRDS(fit_4, file = "fit_4.rds")

# let's look at a fit in table form 

tbl_fit_4 <- tbl_regression(fit_4, 
               intercept = TRUE, 
               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
  as_gt() %>%
  tab_header(title = md("**Geographic Barriers to Healthcare**"),
             subtitle = "What's the relationship between cost and access?") %>%
  tab_source_note(md("Source: IPUMS")) %>% 
  cols_label(estimate = md("**Parameter**"))

# save this table and move into clean_data / my shiny app 

saveRDS(tbl_fit_4, "tbl_fit_4.rds")

# I'll attempt to use tidybayes to prep a newobs tibble for displaying fit_4 

# reference gender and ideology plot (ideology / income men and women class day)
# use doc_moved as fill variable and focus on interaction 
# total x axis
# posterior for different levels of direct Moved relationship //
# highlight plot at top of model page, analysis below

# unique(y$self_pay)

self_pay <- "above_ave"
where <- "Yes"
doc_moved <- c("Yes", "No")
far <- "No"
direct_pay <- unique(y$direct_pay) 
# YEAR <- unique(y$YEAR)

newobs <- expand_grid(self_pay, where, doc_moved, far, direct_pay) %>%
  as_tibble()

pe <- posterior_epred(fit_4, 
                      newdata = newobs) %>%
  as_tibble()

z <- add_fitted_draws(newobs, fit_4) 

# u <- z %>% group_by(self_pay, where, doc_moved, far, direct_pay) %>%
#   summarize(avg = median(.value)) %>%
#   arrange(desc(avg)) 

# should be no overlap between filter and ggplot()

z %>% 
  # filter(self_pay == "above_ave", 
  #        far == "No",
  #     #  where == "Yes",
  #       direct_pay == "above_ave") %>% 
       
  ggplot(aes(x = .value,
             y = direct_pay,
             fill = doc_moved)) +
  stat_slab(alpha = 0.7) +
  labs(title = "Direct Pay versus Doctor Moved or is Unavailable and Total Cost",
       subtitle = "", 
       x = "Total Cost of Care",
       y = "Annual Direct Payment Amount",
       caption = "Source: IPUMS") +
  theme_light()

# plots for fit_4 

fit_4_tibble <- fit_4 %>%
  as_tibble()

farYes <- fit_4 %>% 
  as_tibble() %>% 
  ggplot(aes(farYes)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 100) +
  labs(title = "Posterior Distribution of Distance as a Barrier to Care",
       y = "Probability",
       x = "Coefficient of distance as a barrier",
       caption = "Source: IPUMS") + 
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme_classic()

whereYes <- fit_4 %>% 
  as_tibble() %>% 
  ggplot(aes(whereYes)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 100) +
  labs(title = "Posterior Distribution: Barriers to Care and Cost",
       subtitle = "Don't know where to go as barrier to care",
       y = "Probability",
       x = "Coefficient of whereYes",
       caption = "Source: IPUMS") + 
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme_classic()

intercept <- fit_4 %>% 
  as_tibble() %>% 
  ggplot(aes(`(Intercept)`)) + 
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 100) +
  labs(title = "Posterior Distribution of Intercept",
       y = "Probability",
       x = "Coefficient of Intercept",
       caption = "Source: IPUMS") + 
  scale_y_continuous(labels = scales::percent_format(1)) +
  theme_classic()


# ABOUT THE DATA

# explanation of cost variables:

# P	EXPTOT	Annual total of direct health care payments 
# EXPTOT captures the sum of direct payments for care provided during the year
# including out-of-pocket payments and payments by private insurance, Medicaid, Medicare, and other sources

# P	CHGTOT	Annual total of charges for health care
# sum of fully established charges for care received during the year, 
# excluding those for prescribed medicines
# does not usually reflect actual payments made for services


###################################################################

# sleep data cleaning. This is interesting, but I won't use for now

# recoding data such that all those with:
# more than 7 hours of sleep = Optimal,
# less than 5 hours of sleep = "Deprived"
# and 6 hours = suboptimal 
# based on definitions from American Sleep Association

ideal_sleep <- sleep_data %>% 
  mutate(HRSLEEP = case_when(HRSLEEP >= 7 ~ "Optimal",
                             HRSLEEP < 5  ~ "Deprived",
                             HRSLEEP == 6 ~ "Suboptimal",
                             TRUE ~ NA_character_)) %>%
  select(YEAR, HRSLEEP)

# sleep_costs <- inner_join(ideal_sleep, costs_clean)

# Error: vector memory exhausted (limit reached?)

# I need to make these tibbles smaller so I can build a model and not crash my machine

ccs <- costs_clean %>%
  slice_sample(n = 30000)

iss <- ideal_sleep %>%
  slice_sample(n = 30000)

sleep_costs <- inner_join(ccs, iss) %>%
  select(YEAR, HRSLEEP, direct_pay, self_pay, total)

scs <- sleep_costs %>%
  slice_sample(n = 100000) %>%
  filter(total > 0) %>%
  drop_na()

# dropped_na because I couldn't add NA to new obs tibble to later run posterior_epred

fit_8 <- stan_glm(scs,
                  formula = log(total) ~ YEAR + HRSLEEP + self_pay*HRSLEEP,
                  family = gaussian, 
                  refresh = 0, 
                  seed = 325)


# fit_8 in a table: 

tbl_fit_8 <- tbl_regression(fit_8, 
                            intercept = TRUE, 
                            estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
  as_gt() %>%
  tab_header(title = md("**Sleep and Health Care Costs**"),
             subtitle = "Those who get optimal sleep cost less?") %>%
  tab_source_note(md("Source: IPUMS")) %>% 
  cols_label(estimate = md("**Parameter**"))

# new obs for building a posterior 

YEAR <- unique(scs$YEAR)
HRSLEEP <- unique(scs$HRSLEEP)
self_pay <- unique(scs$self_pay)

# (unique for a cost variable won't work -- need to create buckets')

newobs <- expand_grid(YEAR, HRSLEEP, self_pay) %>%
  as_tibble()

pe <- posterior_epred(fit_8, 
                      newdata = newobs) %>%
  as_tibble()

z <- add_fitted_draws(newobs, fit_8) 


