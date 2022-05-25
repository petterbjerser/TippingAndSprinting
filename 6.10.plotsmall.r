load(file="/home/petter/data/ThesisCode/Topic Model/RData10/estimateEffect10_30.Rdata")


colsSd = c("V"= "#911810", "S"="#E8112d", "Mp"="#83CF39", "C"="#009933","L" = "#006AB3","M" = "#52BDEC", "Kd" = "#000077", "Sd" = "#DDDD00")
cols = c("V"= "#911810", "S"="#E8112d", "Mp"="#83CF39", "C"="#009933","L" = "#006AB3","M" = "#52BDEC", "Kd" = "#000077")


EstimateEffect <- lapply(c("V", "S", "Mp", "C", "L", "M", "Kd", "Sd"), function(.x, .y) {
  extract.estimateEffect(x = estimateEffect10_30,
                      covariate = "antaldagar",
                      method = "continuous",
                      model = topic_models10_29_31_1[[2]][[2]],
                      n = 4,
                      moderator = "parti",
                      nsims = 100,
                      moderator.value = .x)})
EstimateEffect10_30_sd <- do.call("rbind", EstimateEffect)

EstimateEffect10_30_sd = EstimateEffect10_30_sd %>% mutate(
  date = as.Date("2010-10-04") + days(as.integer(covariate.value)))

save(EstimateEffect10_30_sd, file="/home/petter/data/ThesisCode/Topic Model/RData10/EstimateEffect10_30_sd.Rdata")

load(file="/home/petter/data/ThesisCode/Topic Model/RData10/EstimateEffect10_30_sd.Rdata")

EstimateEffect10_30  = EstimateEffect10_30_sd
  #filter(!topic %in% c("30"))

ggplot(EstimateEffect10_30, aes(x = date, y = estimate,
                   ymin = 0, ymax = 0.2,
                   group = moderator.value,
                    fill = factor(moderator.value)
                   )) +
  facet_wrap(~topic, nrow = 6) + 
  geom_stream(lwd = 0.25, type = "ridge") + 
  labs(x = "Year",
    y = "Expected Topic Proportion",
    fill = "Parti") +
  scale_fill_manual(values=c(colsSd), name='Political Party') +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)
  
               theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 14),
    strip.text = element_text(size=14))

  
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/EstimateEffect10_30_Full_ordertest2.png")

ggplot(EstimateEffect10_30, aes(x = date, y = estimate,
                   ymin = 0, ymax = 0.2,
                   group = topic,
                    fill = factor(topic)
                   )) +
  facet_wrap(~moderator.value, nrow = 6) + 
  geom_bar() +
  labs(x = "Year",
    y = "Expected Topic Proportion",
    fill = "Topic") +
theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/EstimateEffect10_30_Full_PARTYtest.png")


## faceting by year, filtering by party, ploting topics and estimates

data2 = as_tibble(data.frame(stringsAsFactors = FALSE,
           term_yy = c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22"),
           beginning = as.Date(c("2010-08-01", "2011-08-01","2012-08-01", "2013-08-01","2014-08-01", "2015-08-01","2016-08-01", "2017-08-01","2018-08-01", "2019-08-01","2020-08-01", "2021-08-01")),
           end = as.Date(c("2011-07-30","2012-07-30","2013-07-30","2014-07-30","2015-07-30","2016-07-30","2017-07-30","2018-07-30","2019-07-30","2020-07-30","2021-07-30", "2022-07-30"))
           ))
result = sqldf("select * from EstimateEffect10_30
                left join data2
                on EstimateEffect10_30.date between data2.beginning and data2.end")

ggplot(result, aes(x = date, y = estimate,
                   ymin = 0, ymax = 0.2,
                   group = moderator.value,
                  fill = factor(moderator.value)
                   )) +
  facet_wrap(~topic, nrow = 6) + 
  geom_stream(lwd = 0.25, type = "proportional") + 
  labs(x = "Year",
    y = "Expected Topic Proportion")
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/EstimateEffect10_30_proportional.png")

