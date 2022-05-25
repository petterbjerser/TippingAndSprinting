
path = "/home/petter/data/ThesisCode/SOM/Data/Super-R-SOM (1986-2019) v2020.1.csv" 

createData = function(path) {
  data = read.csv(path, encoding="UTF-8", head=TRUE) %>% as_tibble()
  }
som_df = createData(path)

save(som_df, file= "/home/petter/data/ThesisCode/Topic Model/Rdata2/som_df.Rdata")


load(file= "/home/petter/data/ThesisCode/Topic Model/RData/Rdata2/som_df.Rdata")
#KLIMAT Year and Party
som_df = som_df %>% filter(year >= 2011)

som_df_2011 = som_df %>%
mutate(v = ifelse(cb10 %in% 1, 1, 0),
            s = ifelse(cb10 %in% 2, 1, 0),
            c = ifelse(cb10 %in% 3, 1, 0),
            l = ifelse(cb10 %in% 4, 1, 0),
            m = ifelse(cb10 %in% 5, 1, 0),
            kd = ifelse(cb10 %in% 6, 1, 0),
            mp = ifelse(cb10 %in% 7, 1, 0),
            sd = ifelse(cb10 %in% 10, 1, 0))

som_df_2011 = som_df_2011 %>%
  mutate(samprob1_2011_klimat = ifelse(samprob1_2011 %in% 33300, 1, 0), 
         samprob2_2011_klimat = ifelse(samprob2_2011 %in% 33300, 1, 0),
         samprob3_2011_klimat = ifelse(samprob3_2011 %in% 33300, 1, 0),
         samprob1_2011_miljö  = ifelse(samprob1_2011 %in% 33000, 1, 0), 
         samprob2_2011_miljö  = ifelse(samprob2_2011 %in% 33000, 1, 0),
         samprob3_2011_miljö  = ifelse(samprob3_2011 %in% 33000, 1, 0),
                      samprob_all_2011_klimat = samprob1_2011_klimat + samprob2_2011_klimat + samprob3_2011_klimat,
                      samprob_all_2011_miljö = samprob1_2011_miljö + samprob2_2011_miljö + samprob3_2011_miljö) %>%
  group_by(year, cb10) %>% #cb10
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(klimat_share = samprob_all_2011_klimat,
         miljö_share = samprob_all_2011_miljö) %>%
dplyr::select(year, cb10, klimat_share, miljö_share, v, s, c, l, m, kd, mp, sd) %>% 
   filter(cb10 <= 10) %>%
   ungroup()

df = som_df_2011 %>%
mutate(v = ifelse(cb10 %in% 1, 1, 0),
            s = ifelse(cb10 %in% 2, 1, 0),
            c = ifelse(cb10 %in% 3, 1, 0),
            l = ifelse(cb10 %in% 4, 1, 0),
            m = ifelse(cb10 %in% 5, 1, 0),
            kd = ifelse(cb10 %in% 6, 1, 0),
            mp = ifelse(cb10 %in% 7, 1, 0),
            sd = ifelse(cb10 %in% 10, 1, 0))

som_df_2011$cb10[som_df_2011$cb10=="1"] <- "V"
som_df_2011$cb10[som_df_2011$cb10=="2"] <- "S"
som_df_2011$cb10[som_df_2011$cb10=="3"] <- "C"
som_df_2011$cb10[som_df_2011$cb10=="4"] <- "L"
som_df_2011$cb10[som_df_2011$cb10=="5"] <- "M"
som_df_2011$cb10[som_df_2011$cb10=="6"] <- "Kd"
som_df_2011$cb10[som_df_2011$cb10=="7"] <- "Mp"
som_df_2011$cb10[som_df_2011$cb10=="10"] <- "Sd"

som_df_2011_plot = som_df_2011 %>% 
  mutate(vote = v+s+c+l+m+kd+mp+sd) %>% 
  select(year, cb10, klimat_share, miljö_share, vote) %>% 
  mutate(ClimateVote = klimat_share/vote, EnvironmentVote=miljö_share/vote)

dftest = som_df_2011_plot %>%
 group_by(cb10) %>%
 summarise_all(sum, na.rm = TRUE)


dftest = select(som_df_2011_plot, year, cb10, ClimateVote, EnvironmentVote)


##both variables
dftest$year = as.factor(dftest$year)
pivot = dftest %>% 
  tidyr::pivot_longer(names_to = "key", 
                    values_to = "value", 
                    cols = c(EnvironmentVote,
                             ClimateVote))

pivot %>% 
  ggplot(aes(x = year, y = value, group = key, colour = key)) +
  facet_wrap(vars(cb10), ncol = 2, scales="free_y") +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Share of Voter's Indicating Climate or Environment as Most Important Issue") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  theme(legend.position="top") +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 14),
    strip.text = element_text(size=14)) +
  scale_x_continuous("Year", labels = as.character(year), breaks = year) 
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/SOM_pivot2.png")


#misc?