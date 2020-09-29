#Statistics

source("libs_and_paths.R")

stat_data <- read.delim2(paste0(data_path, "stat_data.txt")) %>% 
  mutate(year = factor(year),
         site_number = factor(site_number))

#Compare percent algae biomass in apical vs basal parts for year 2018 and 2019 using beta regression
prop_alge_data <- stat_data %>% 
  filter(variable == "proportion_algedel")

prop_alge_beta <- betareg(value~year*algae_part, data = prop_alge_data)

emmip(prop_alge_beta, year~algae_part, CIs = TRUE)

summary(prop_alge_beta)

joint_tests(prop_alge_beta)

plot(resid(prop_alge_beta))
hist(resid(prop_alge_beta))

marginal <- emmeans(prop_alge_beta, ~year*algae_part)

pairs(marginal, simple = list("algae_part", "year", c("algae_part", "year")), adjust = "tukey")

#Average biomass in spring vs summer
#Mann-whitney u test
wilcox.test(fig_3_c_data[fig_3_c_data$year == "2018",]$chara,
            fig_3_c_data[fig_3_c_data$year == "2019",]$chara)

wilcox.test(fig_3_c_data[fig_3_c_data$year == "2018",]$surface,
            fig_3_c_data[fig_3_c_data$year == "2019",]$surface)


#Calcium release in apical and basal parts with varying ph
ca_release_data <- fig_4_data[-1,]

shapiro.test(ca_release_data$ca_ueq)
shapiro.test(log(ca_release_data$ca_ueq))

ca_release_lm <- lm(log(ca_ueq) ~ ph * shoot, data = ca_release_data)
ca_release_lm1 <- lm(log(ca_ueq) ~ ph + shoot, data = ca_release_data)
anova(ca_release_lm, ca_release_lm1)

emmip(ca_release_lm1, shoot ~ ph, cov.reduce = range, CIs = TRUE)

summary(ca_release_lm1)

anova(ca_release_lm1)

plot(resid(ca_release_lm1))
hist(resid(ca_release_lm1))

emtrends(ca_release_lm, pairwise ~ shoot, var = "ph")

#Total p concentration in apical and basal parts in summer and spring
tot_p_data <- stat_data %>% 
  filter(variable == "plante_tot_p") %>% 
  na.omit()

shapiro.test(tot_p_data$value)

shapiro.test(sqrt(tot_p_data$value))

tot_p_lm1 <- lm(sqrt(value)~year*algae_part, data = tot_p_data)
tot_p_lm2 <- lm(sqrt(value)~year+algae_part, data = tot_p_data)
anova(tot_p_lm1, tot_p_lm2)

emmip(tot_p_lm2, year~algae_part, CIs = TRUE)

summary(tot_p_lm2)

anova(tot_p_lm2)

plot(resid(tot_p_lm2))
hist(resid(tot_p_lm2))

#Compare ca-release using 3.8 (low) and 38 % (high) acetic acid 
treatment_high <- c(88.02956464, 81.14094145, 121.7967761, 86.52423111)
treatment_low <- c(60.18338462, 71.91856061, 113.1749495, 53.75222025, 54.35669097)

t.test(treatment_low, treatment_high)
