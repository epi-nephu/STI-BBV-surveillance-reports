# collection of discarded code

# playground for testing epicurve plots (histogram) ----
sti_data %>% 
  filter(condition %in% main_sti) %>% 
  filter(lga %in% nephu_lgas) %>% 
  ggplot() +
  geom_histogram(aes(x = event_date, group = cond, fill = cond), breaks=epimonth_breaks, closed="left", , col="gray") +
  #scale_fill_manual(values=c("#F8766D", "#00BFC4"), name="Case") +
  scale_x_date(expand=c(0,0), date_breaks="1 month", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), breaks=integer_breaks(), name="Monthly notified cases") + 
  cowplot::theme_cowplot() + 
  theme(plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1))

sti_data %>% 
  filter(condition %in% main_sti) %>% 
  filter(lga %in% nephu_lgas) %>% 
  ggplot() +
  geom_histogram(aes(x = event_date, group = cond, fill = cond), breaks=epimonth_breaks, closed="left", , col="gray") +
  #scale_fill_manual(values=c("#F8766D", "#00BFC4"), name="Case") +
  scale_x_date(expand=c(0,0), date_breaks="1 month", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), breaks=integer_breaks(), name="Monthly notified cases") + 
  cowplot::theme_cowplot() + 
  theme(plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1)) + 
  facet_wrap(~cond, nrow=2, scales="free_y")

# playground for age-sex pyramid plots ----

pyramid.dat <- sti_data %>% 
  filter(disease=="Syphilis") %>% 
  filter(lga %in% nephu_lgas) %>% 
  select(age, agecat5, sex, defn, cond, condition) %>% 
  filter(sex %in% c("Male", "Female")) %>%
  mutate(sex = fct_rev(fct_drop(sex)))

# plot
plot_Ch_Go <- apyramid::age_pyramid(data=pyramid.dat %>% filter(cond %in% c("Chlamydia", "Gonorrhea")), age_group="agecat5", split_by="sex",stack_by="cond", show_midpoint=FALSE, pal=c("#F8766D", "#00BFC4")) + 
  #scale_fill_manual(values=c("#F8766D", "#00BFC4")) + 
  #scale_x_continuous(limits=c(-1000, 1000)) + 
  cowplot::theme_cowplot() + 
  theme(legend.position = "none") + 
  labs(y="cases", x="age group (years)", title="") + 
  facet_wrap(~cond, nrow=1, scales="free_x")

plot_Syph <- apyramid::age_pyramid(data=pyramid.dat %>% filter(cond %in% c("Syphilis - Late", "Syphilis - Infectious")), age_group="agecat5", split_by="sex",stack_by="cond", show_midpoint=FALSE) + 
  scale_fill_manual(values=c("#F8766D", "#00BFC4")) + 
  cowplot::theme_cowplot() + 
  theme(legend.position = "none") + 
  labs(y="cases", x="age group (years)", title="") + 
  facet_wrap(~cond, nrow=1, scales="free_x")

ggpubr::ggarrange(plot_Ch_Go, plot_Syph, nrow=2)

# Alternative plot using ggplot
# begin ggplot
ggplot(mapping = aes(x = age, fill = sex)) +
  
  # female histogram
  geom_histogram(data = pyramid.dat %>% filter(cond %in% c("Chlamydia", "Gonorrhea")) %>% 
                   filter(sex == "Female"), 
                 breaks = seq(0,95,5),
                 colour = "white") +
  
  # male histogram (values converted to negative)
  geom_histogram(data = pyramid.dat %>% filter(cond %in% c("Chlamydia", "Gonorrhea")) %>% 
                   filter(sex == "Male"), 
                 breaks = seq(0,95,5), 
                 mapping = aes(y = ..count..*(-1)),
                 colour = "white") +
  
  # flip the X and Y axes
  coord_flip() +
  
  # adjust counts-axis scale
  scale_y_continuous(limits = c(-2000, 1500),
                     breaks = seq(-2000,1500,500),
                     labels = abs(seq(-2000, 1500, 500))) + 
  
  # adjust age-axis scale
  scale_x_continuous(breaks = seq(0, 95, 5), labels = seq(0, 95, 5)) + 
  
  # themes
  cowplot::theme_cowplot() + 
  labs(y="cases", x="age group (years)", title="") + 
  
  # facet wrap
  facet_wrap(~cond, ncol=2)

# playground for plotting case rate graphs ----
sti_nephu_mth_count <- sti_data %>% 
  filter(condition %in% main_sti) %>% 
  filter(lga %in% nephu_lgas) %>% 
  group_by(mth_yr_fmt, condition) %>% 
  summarise(count = n()) %>% 
  mutate(lga="NEPHU")

sti_nephu_mth_rate <- left_join(sti_nephu_mth_count, pop_table, by="lga") %>% 
  mutate(mth_rate = count/pop_2021*100000)

ggplot(sti_nephu_mth_rate) + 
  geom_line(aes(x=mth_yr_fmt, y=mth_rate, group=condition, col=condition)) + 
  scale_x_date(expand=c(0,0), date_breaks="3 months", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), name="Monthly notified rate") + 
  cowplot::theme_cowplot() + 
  theme(plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1, size=7)) + 
  facet_wrap(~lga, nrow=4, scales = "free_y")


ggplot(vic_mth_rate) + 
  geom_line(aes(x=mth_yr_fmt, y=mth_rate, group=condition, col=condition)) + 
  scale_x_date(expand=c(0,0), date_breaks="3 months", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), name="Monthly notified rate") + 
  cowplot::theme_cowplot() + 
  theme(plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1, size=7))


ggplot(sti_nephu_mth_rate) + 
  geom_line(aes(x=mth_yr_fmt, y=mth_rate, group=condition, col=condition)) + 
  geom_line(data=vic_mth_rate %>% rename(state=lga), aes(x=mth_yr_fmt, y=mth_rate, group=condition, col=condition), lty="dotted") +
  scale_x_date(expand=c(0,0), date_breaks="3 months", date_minor_breaks="month", date_labels="%b %Y", name="") +
  scale_y_continuous(expand=c(0,0), name="Monthly notified rate") + 
  cowplot::theme_cowplot() + 
  theme(plot.caption = element_text(size=11), 
        axis.text.x = element_text(angle = 45, hjust=1, size=7)) + 
  facet_wrap(~lga, nrow=4, scales = "free_y")


vic_monthly_count.row <- as.data.frame(t(vic_monthly_count)) %>% 
  row_to_names(1) %>% 
  mutate(across(1:nrow(vic_monthly_count), ~as.numeric(.))) %>% 
  tibble::rownames_to_column("LGA") %>% 
  rename(NEPHU = Sum) # use NEPHU so that it can append as row to nephu_lga table below

NEPHU <- sum # customise function label for use with addmargins

lga_cond.tab <- addmargins(xtabs(~nephu_vic + epi_MY, data=vic_data, exclude="Vic"), c(1,2), FUN = NEPHU, quiet=TRUE) %>% as.data.frame.matrix() %>% 
  tibble::rownames_to_column("LGA") %>% 
  add_row(vic_monthly_count.row) %>% 
  left_join(., pop_data, by="LGA") %>% 
  mutate(Rate = round(NEPHU/Pop_2021*100000, 1)) %>% 
  rename(`Total cases` = NEPHU, "LGA/Area" = LGA) %>% 
  select(-Pop_2021)
