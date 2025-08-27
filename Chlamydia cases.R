# load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(flextable)

# set params
max_date <- ymd("2024-05-31")
min_date_1yr <- max_date - years(1) + days(1)

# load pop_table
pop_table <- readxl::read_xlsx(here::here("Data", "LGAPopulationData.xlsx")) %>% 
  rename(lga=LGA, 
         pop_2021 = Pop_2021)

# load data
raw_data <- readxl::read_xlsx(here::here("Data", "NEPHUCaseLinelist_2022_2024.xlsx")) %>% 
  clean_names() %>% 
  rename(event_id = phess_id, 
         defn = most_recent_event_classfication, 
         age = age_in_years_from_event_date, 
         postcode = postcode_20, 
         lga = local_government_area, 
         lphu = local_public_health_unit, 
         sexual_exp = sexual_exposure_206) 
  


# gen_functions
percent <- function (x) {round(x*100, digits=0)}

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# configure data
agecat5_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
agegrp_levels <- c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")

nephu_lgas <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", "Hume (C)", "Knox (C)", "Manningham (C)", "Maroondah (C)", "Nillumbik (S)", 
                "Whitehorse (C)", "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")

ch_data <- raw_data %>% 
  filter(condition=="Chlamydia trachomatis infection") %>% 
  filter(defn=="Confirmed" | defn=="Probable") %>% 
  filter(event_type=="Case") %>% 
  remove_empty(which="cols") %>% 
  select(event_id, event_date, condition, event_type, defn, age, sex, postcode, lga, lphu, sexual_exp) %>% 
  mutate(age = if_else(age==999, NA, age), 
         agegrp = case_when(between(age, 0, 14) ~ "0-14", 
                            between(age, 15, 24) ~ "15-24", 
                            between(age, 25, 34) ~ "25-34", 
                            between(age, 35, 44) ~ "35-44", 
                            between(age, 45, 54) ~ "45-54", 
                            between(age, 55, 64) ~ "55-64", 
                            between(age, 65, 110) ~ "65+", 
                             TRUE ~ NA_character_), 
         agegrp = factor(agegrp, levels=agegrp_levels), 
         defn = factor(defn, levels=c("Confirmed", "Probable")), 
         sex = factor(sex, levels=c("Female", "Male", "Not stated", "Other")), 
         event_date = ymd(event_date), 
         epiweek = epiweek(event_date), 
         epimonth = month(event_date, label=TRUE), 
         epiyear = epiyear(event_date), 
         epi_MY = my(paste(epimonth, epiyear, sep="-")), 
         epi_Y = ymd(paste0(epiyear, "-01-01")), 
         epi_WY = ceiling_date(event_date, "week") -1, 
         yr = year(event_date), 
         mth_yr_fmt = my(paste(month(event_date, label=TRUE), year(event_date))), 
         mth_yr = paste(epimonth, year(event_date))) %>% 
  arrange(event_date)

#tabyl(sti_data, condition, cond)
#tabyl(sti_data, cond)

  
# LGA graph
lga_tab <- ch_data %>% 
  group_by(lga) %>% 
  summarise(count=n()) %>% 
  adorn_totals(name="NEPHU") %>% 
  left_join(., pop_table, by="lga") %>% 
  mutate(rate = round(count/pop_2021*100000, 1), 
         rate_1000 = round(count/pop_2021*1000, 1))

lga_tab %>% 
  mutate(lga = factor(lga, levels=c(nephu_lgas, "NEPHU"))) %>% 
  ggplot() + 
  geom_col(aes(x=lga, y=rate_1000), fill="#5BC788") + 
  geom_text(aes(x=lga, label=count), y=2, size=4) + 
  scale_x_discrete(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=14)) + 
  labs(x="", y="Chlamydia rate per 1000 population", caption = "numbers in bars represent case numbers")


# age-sex pyramid
pyramid.dat <- ch_data %>% 
    select(age, agegrp, sex, defn) %>% 
    filter(sex %in% c("Male", "Female")) %>% 
    mutate(sex=fct_rev(fct_drop(sex)))
  
apyramid::age_pyramid(data=pyramid.dat, age_group="agegrp", split_by="sex", show_midpoint=FALSE) + 
    cowplot::theme_cowplot() + 
    theme(plot.title = element_text(size=12), 
          legend.position="right") + 
    labs(y="cases", x="age group (years)", title=paste0("Age and sex distribution of Chlamydia cases in NEPHU"))
  