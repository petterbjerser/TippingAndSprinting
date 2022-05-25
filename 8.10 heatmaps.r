load( file="/home/petter/data/ThesisCode/Topic Model/RData10/EstimateEffect10_30_sd.Rdata")


library(pheatmap)
library(heatmap3)


#Create data

EE_30_tbl = EstimateEffect10_30_sd %>% as_tibble() %>% dplyr::select(topic, covariate.value, moderator.value, estimate)

EE_30_tbl_meta = pivot_wider(EE_30_tbl, names_from = topic, values_from=estimate) %>% dplyr::select(-covariate.value) #-moderator.value

df_2 = EE_30_tbl_meta %>% group_by(moderator.value) %>% 
summarise_all(sum, na.rm = TRUE) %>% dplyr::select(-moderator.value)

df_date = EE_30_tbl  %>% mutate(
  covariate.value = round(as.numeric(covariate.value)),
  moderator.value = moderator.value,
  date = ymd("2010-10-04") + days(covariate.value),
)

#Change dok_rm intervals
data2 = as_tibble(data.frame(stringsAsFactors = FALSE,
           term_yy = c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22"),
           beginning = as.Date(c("2010-08-01", "2011-08-01","2012-08-01", "2013-08-01","2014-08-01", "2015-08-01","2016-08-01", "2017-08-01","2018-08-01", "2019-08-01","2020-08-01", "2021-08-01")),
           end = as.Date(c("2011-07-30","2012-07-30","2013-07-30","2014-07-30","2015-07-30","2016-07-30","2017-07-30","2018-07-30","2019-07-30","2020-07-30","2021-07-30", "2022-07-30"))
           ))
result = sqldf("select * from df_date
                left join data2
                on df_date.date between data2.beginning and data2.end")

result2 = result %>% as_tibble() %>%
  filter(topic != "30") %>%
  mutate(estimate = ifelse(estimate < 0, 0, estimate))



#heatmap 30, group by topic, moderater min_max_norm

result2 = resul2 %>%
  group_by(topic, moderator.value) %>% #moderatorvalue) %>% #, term_yy) %>% #moderatorvalue) %>% #, term_yy) %>% 
  summarize(estimate = sum(estimate))

min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
resultnorm = as.data.frame(lapply(result2[3], min_max_norm))

dfold = select(result2, topic, moderator.value)
dfnew = cbind(dfold, resultnorm)

result_pivot = pivot_wider(dfnew, names_from = topic, values_from=estimate) 

r2 = result_pivot[, c(2:30)]
m = as.matrix(r2)
rownames(m) <- c('C', 'Kd', 'L', 'M', 'Mp', 'S', 'Sd', 'V')
colnames(m) <- c("1. Effects of Climate Change", "2. EU Directives and Regulations", "3. European & Nordic Cooperation", 
"4. Climate Change, Politics, and Parties", "5. Government Proposals and Regulations", "6. Private cars and Low-Emissions Vehicles", 
"7. Forests, Carbon Taxes, Market Economy", "8. Questions to Government Representatives", "9. Industry, Infrastructure, and Cities", 
"10. Green Transition...", "11. Environment Management", "12. Oceans, Biodiversity, Water & Air", 
"13. Climate Impact, Buildings, Parliamentary Debate", "14. Funding for Climate and Environment", "15. Fossil Fuels and Biofuels", "16. Climate Targets", 
"17. Sweden as a Climate Leader", "18. Sustainable Development and Human Rights", "19. State Owned Energy Utility Vattenfall", 
"20. Subsidies", "21. Mode-shift and Alternative Transport", 
"22. Energy Politics", "23. Aviation and Bio Fuels", "24. Green Tax Reform", "25. Circular Economy", "26. International Climate Conferences", 
"27. Emissions Reductions", "28. Electrification", "29. Emission Trading")
pheatmap(m,  cutree_rows = 5,
         cutree_cols = 8, fontsize_col=7, fontsize_row=10, angle_col = 45, 
         filename = "/home/petter/data/ThesisCode/Topic Model/PNG10/heatmap3_30_mod_min_max_norm.jpg")


#heatmap 30, group by topic, term_yy min_max_norm


result2 = result %>% as_tibble() %>%
  filter(topic != "30") %>%
  group_by(topic, term_yy) %>% 
  summarize(estimate = sum(estimate))

min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

resultnorm = as.data.frame(lapply(result2[3], min_max_norm))
dfold = select(result2, topic, term_yy)
dfnew = cbind(dfold, resultnorm)
result_pivot = pivot_wider(dfnew, names_from = topic, values_from=estimate) 

r2 = result_pivot[, c(2:30)]
m = as.matrix(r2)
rownames(m) <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21")
colnames(m) <- c("1. Effects of Climate Change", "2. EU Directives and Regulations", "3. European & Nordic Cooperation", 
"4. CC, Politics, and Parties", "5. Government Proposals and Regulations", "6. Private cars and Low-Emissions Vehicles", 
"7. Forests, Carbon Taxes, Market Economy", "8. Questions to Government Representatives", "9. Industry, Infrastructure, and Cities", 
"10. Green Transition and Rural Transition", "11. Environment Management", "12. Oceans, Biodiversity, Water & Air", 
"13. Climate Impact, Buildings, Parliamentary Debate", "14. Funding Climate & Environ...", "15. Fossil Fuels and Biofuels", "16. Climate Targets", 
"17. Sweden as a Climate Leader", "18. Sustainable Development and Human Rights", "19. State Owned Energy Utility Vattenfall", 
"20. Subsidies", "21. Mode-shift and Alternative Transport", 
"22. Energy Politics", "23. Aviation and Bio Fuels", "24. Green Tax Reform", "25. Circular Economy", "26. International Climate Conferences", 
"27. Emissions Reductions", "28. Electrification", "29. Emission Trading")
pheatmap(m, cutree_rows = 5, cutree_cols = 8, fontsize_col=7, fontsize_row=10, 
         angle_col = 45, filename = "/home/petter/data/ThesisCode/Topic Model/PNG10/heatmap3_30_term_min_max.jpg")


#heatmap 30, group by moderater topic, moderator.value, term_yy min_max_norm


result2 = result %>% as_tibble() %>%
  filter(topic != "30") %>%
  group_by(topic, moderator.value, term_yy) %>% 
  summarize(estimate = sum(estimate))


min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
resultnorm = as.data.frame(lapply(result2[3], min_max_norm))

dfold = select(result2, topic, moderator.value)
dfnew = cbind(dfold, resultnorm)

result_pivot = pivot_wider(result2, names_from = topic, values_from=estimate) 
result_pivot_p_y = select(result_pivot, moderator.value, term_yy) 
result_pivot_p_y$party_year = paste(result_pivot_p_y$moderator.value, result_pivot_p_y$term_yy)

r2 = result_pivot[, c(3:31)]
m = as.matrix(r2)
rownames(m) <- c(result_pivot_p_y$party_year)
colnames(m) <- c("1. Effects of CC", "2. EU Directives & Regulation", "3. European & Nordic Cooperation", 
"4. CC, Politics, and Parties", "5. Government Proposals...", "6. Private cars and LVEs", 
"7. Forests, Carbon Taxes...", "8. Questions to Gov. Representatives", "9. Industry, Infrastructure, and Cities", 
"10. Green Transition...", "11. Environment Management", "12. Oceans, Biodiversity, Water & Air", 
"13. Climate Impact, Buildings, Debate", "14. Funding Climate & Env", "15. Fossil Fuels and Biofuels", "16. Climate Targets", 
"17. Sweden as a Climate Leader", "18. Sustainable Dev. and HR", "19. State Owned Energy Utility", 
"20. Subsidies", "21. Mode-shift and Alt. Transport", 
"22. Energy Politics", "23. Aviation and Bio Fuels", "24. Green Tax Reform", "25. Circular Economy", "26. Int. Climate Conferences.", 
"27. Emissions Reductions", "28. Electrification", "29. Emission Trading")

pheatmap(m,  cutree_rows = 8,
         cutree_cols = 9, fontsize_col=5, fontsize_row=6, angle_col = 45, 
         filename = "/home/petter/data/ThesisCode/Topic Model/PNG10/heatmap3_30_term_mod_norm.jpg")#

