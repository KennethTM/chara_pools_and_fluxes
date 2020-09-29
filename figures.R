#Figures

source("libs_and_paths.R")

#Aq. Sci.:For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.

#Translate site names numbered from north to south
site_names <- tribble(~site, ~site_new,
                      "Lille Dam", "6",
                      "Åledam", "1",
                      "Igeldam", "2",
                      "Räppl-stor", "10",
                      "Lille-Nord", "3",
                      "Dam 12", "8",
                      "Dam 9", "9",
                      "Snogedam", "4") %>%
  mutate(site_new = factor(site_new, levels = as.character(1:10)))

#Figure 2
fig_2_a <- fig_2_a_b_data %>%
  filter(variable == "perc_plant") %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = mean, fill = shoot))+
  geom_linerange(aes(ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.4))+
  geom_point(position = position_dodge(width = 0.4), shape = 21)+
  scale_fill_manual(values = c("white", "black"), labels = c("Apical", "Basal"))+
  facet_grid(.~year, scales = "free")+
  ylab("Tissue (%)")+
  xlab(NULL)+
  ylim(10, 50)+
  geom_text(data = data.frame(year = c("2018", "2019"), x = -Inf, y = Inf, label = c("Summer 2018", "Spring 2019")), 
            aes(x,y,label=label), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  theme(axis.text.x = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.8))

fig_2_b <- fig_2_a_b_data %>%
  filter(variable != "perc_plant") %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = mean, fill = shoot))+
  geom_hline(yintercept = 1, linetype = 3)+
  geom_linerange(aes(ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.4), show.legend = FALSE)+
  geom_point(position = position_dodge(width = 0.4), show.legend = FALSE, shape = 21)+
  scale_fill_manual(values = c("white", "black"))+
  ylim(0.5, 2.5)+
  facet_grid(.~year, scales = "free")+
  ylab(expression(C[org]*bold(":")*C[carbonate]))+
  xlab(NULL)+
  theme(axis.text.x = element_blank(),
        strip.text = element_blank())

fig_2_c_data <- read.delim2(paste0(data_path, "fig_2_c.txt"))

fig_2_c <- fig_2_c_data %>% 
  gather(variable, value, chara, surface) %>% 
  mutate(perc = value/total*100,
         label = ifelse(variable == "chara", NA, paste0(round(perc, 0), " %"))) %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = value, fill = variable))+
  geom_col(col = "black")+
  scale_fill_manual(values = c("grey", "white"), labels = c("Tissue", "Encrustation"))+
  geom_text(aes(label = label, group = variable),
            position = position_stack(vjust = 0.5))+
  facet_grid(.~year, scales = "free")+
  ylab(expression("Biomass (g DW"~m^{-2}*")"))+
  theme(strip.text = element_blank())+
  xlab(NULL)+
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8))

fig_2 <- rbind(ggplotGrob(fig_2_a), ggplotGrob(fig_2_b), ggplotGrob(fig_2_c), size = "last")

grid.newpage()
grid.arrange(fig_2)

ggsave(paste0(fig_path, "fig_2.png"), grid.arrange(fig_2), width = 174, height = 180, units = "mm")
ggsave(paste0(fig_path, "fig_2.svg"), grid.arrange(fig_2), width = 174, height = 180, units = "mm")

#Figur 3
fig_3 <- fig_3_data[-1,] %>% 
  select(-ca_ug) %>% 
  gather(variable, value, ca_ueq, alk_ueq) %>% 
  ggplot(aes(x = ph, y = value, shape = variable))+
  geom_point()+
  scale_shape_manual(values=c(19, 1), labels = c("ANC", expression(Ca^{"2+"})), 
                     guide = guide_legend(label.hjust = 0))+
  geom_hline(yintercept = 0, linetype = 3)+
  facet_grid(.~shoot)+
  ylim(-2, 6)+
  ylab(expression(Ca^{"2+"}~"and"~ANC~(mu*eq~g~DW^{-1}~d^{-1})))+
  xlab("pH")+
  scale_x_continuous(breaks = seq(7, 10, 0.5))+
  geom_text(data = data.frame(shoot = c("apical", "basal"), x = -Inf, y = Inf, label = c("Apical", "Basal")), 
            aes(x,y,label=label), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  theme(strip.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8, 0.8))

ggsave(paste0(fig_path, "fig_3.png"), fig_3, width = 174, height = 84, units = "mm")
ggsave(paste0(fig_path, "fig_3.svg"), fig_3, width = 174, height = 84, units = "mm")

#Figur 4
fig_4_a <- fig_4_data %>% 
  filter(variable == "p_tot") %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = mean, fill = shoot))+
  geom_linerange(aes(ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.4))+
  geom_point(position = position_dodge(width = 0.4), shape = 21)+
  scale_fill_manual(values = c("white", "black"), labels = c("Apical", "Basal"))+
  facet_grid(.~year, scales = "free")+
  ylab(expression("Total P ("*mu*g~P~mg~DW^{-1}*")"))+
  xlab(NULL)+
  geom_text(data = data.frame(year = c("2018", "2019"), x = -Inf, y = Inf, label = c("Summer 2018", "Spring 2019")), 
            aes(x,y,label=label), inherit.aes = FALSE, hjust = -0.2, vjust = 1.2)+
  theme(axis.text.x = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.8))

fig_4_b <- fig_4_data %>% 
  filter(variable == "p_algae") %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = mean, fill = shoot))+
  geom_linerange(aes(ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.4), show.legend = FALSE)+
  geom_point(position = position_dodge(width = 0.4), shape = 21, show.legend = FALSE)+
  scale_fill_manual(values = c("white", "black"), labels = c("Apical", "Basal"))+
  facet_grid(.~year, scales = "free")+
  ylab(expression("Tissue P ("*mu*g~P~mg~DW^{-1}*")"))+
  xlab(NULL)+
  scale_y_continuous(breaks = seq(0, 4, 1), limits = c(0, 3.5))+
  theme(axis.text.x = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.8))

fig_4_c <- fig_4_data %>% 
  filter(variable == "p_precip") %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = mean, fill = shoot))+
  geom_linerange(aes(ymin = mean-sd, ymax = mean+sd), position = position_dodge(width = 0.4), show.legend = FALSE)+
  geom_point(position = position_dodge(width = 0.4), shape = 21, show.legend = FALSE)+
  scale_fill_manual(values = c("white", "black"), labels = c("Apical", "Basal"))+
  facet_grid(.~year, scales = "free")+
  ylab(expression("Encrustation P ("*mu*g~P~mg~DW^{-1}*")"))+
  xlab(NULL)+
  theme(axis.text.x = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.8))

fig_4_d <- fig_4_data %>% 
  filter(variable %in% c("p_algae_area", "p_precip_area")) %>% 
  select(-sd, -shoot) %>% 
  spread(variable, mean) %>% 
  mutate(sum = p_algae_area+p_precip_area) %>% 
  gather(variable, value, p_algae_area, p_precip_area) %>% 
  mutate(perc = value/sum*100,
         label = ifelse(variable == "p_precip_area", NA, paste0(round(perc, 0), " %"))) %>% 
  left_join(site_names) %>% 
  ggplot(aes(x = site_new, y = value, fill = variable))+
  geom_col(col = "black")+
  scale_fill_manual(values = c("grey", "white"), labels = c("Tissue", "Encrustation"))+
  geom_text(aes(label = label, group = variable),
            position = position_stack(vjust = 0.5))+
  facet_grid(.~year, scales = "free")+
  ylab(expression("Areal P (mg P"~m^{-2}*")"))+
  theme(strip.text = element_blank())+
  xlab(NULL)+
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8))

fig_4 <- rbind(ggplotGrob(fig_4_a), ggplotGrob(fig_4_b), ggplotGrob(fig_4_c), ggplotGrob(fig_4_d), size = "last")

grid.newpage()
grid.arrange(fig_4)

ggsave(paste0(fig_path, "fig_4.png"), grid.arrange(fig_4), width = 174, height = 234, units = "mm")
ggsave(paste0(fig_path, "fig_4.svg"), grid.arrange(fig_4), width = 174, height = 234, units = "mm")

#Figure 5
shapiro.test(p_crust_algae$p_algae)
shapiro.test(p_crust_algae$p_crust)
shapiro.test(sqrt(p_crust_algae$p_algae))
shapiro.test(sqrt(p_crust_algae$p_crust))

p_crust_algae_lm <- lm(sqrt(p_crust)~sqrt(p_algae), data = p_crust_algae)
plot(resid(p_crust_algae_lm))
summary(p_crust_algae_lm)
confint(p_crust_algae_lm)

p_crust_algae_fig <- p_crust_algae %>% 
  ggplot(aes(y=sqrt(p_crust), x=sqrt(p_algae)))+
  geom_point(shape = 1)+
  geom_smooth(method="lm", col = "black")+
  xlab(expression(sqrt(P[algae])~"("*mu*g~P~mg~DW^{-1}*")"))+
  ylab(expression(sqrt(P[crust])~"("*mu*g~P~mg~DW^{-1}*")"))

ggsave(paste0(fig_path, "fig_5.png"), p_crust_algae_fig, width = 84, height = 84, units = "mm")
ggsave(paste0(fig_path, "fig_5.svg"), p_crust_algae_fig, width = 84, height = 84, units = "mm")

#Figure S1
shapiro.test(fig_s1_data$calcium)
shapiro.test(fig_s1_data$alkalinity)

fig_s1_lm <- lm(calcium~alkalinity, data = fig_s1_data)
summary(fig_s1_lm)
confint(fig_s1_lm)

fig_s1 <- fig_s1_data %>% 
  ggplot(aes(alkalinity, calcium))+
  geom_abline(slope = 1, intercept = 0, linetype = 3)+
  geom_point(shape = 1)+
  geom_smooth(method="lm", col = "black")+
  coord_cartesian(xlim = c(0.1, 0.7), ylim = c(0.1, 0.7))+
  scale_x_continuous(breaks = seq(0.1, 0.7, 0.1))+
  scale_y_continuous(breaks = seq(0.1, 0.7, 0.1))+
  ylab(expression(Ca^{"2+"}~(meq~g~DW^{-1}~d^{-1})))+
  xlab(expression(ANC~(meq~g~DW^{-1}~d^{-1})))

ggsave(paste0(fig_path, "fig_s1.png"), fig_s1, width = 84, height = 84, units = "mm")
ggsave(paste0(fig_path, "fig_s1.svg"), fig_s1, width = 84, height = 84, units = "mm")
