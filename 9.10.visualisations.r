load(file="/home/petter/data/ThesisCode/Topic Model/RData10/dataParagraph10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/dataParagraphFull10.Rdata")


cols = c("V"= "#DA291C", "S"="#E8112d", "Mp"="#83CF39", "C"="#009933","L" = "#006AB3","M" = "#52BDEC", "Kd" = "#000077", "Sd" = "#DDDD00")

#Share climate per party


dataFullParagraph = dataParagraphFull10 %>% 
  mutate(climateParagraph = ifelse(climateSum==1, 1, 0),
  paragraphCount = length(paragraph_id)/length(paragraph_id)) %>% 
  group_by(dok_datum) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(shareClimateParagraph = climateParagraph/paragraphCount) %>% 
  select(dok_datum, paragraph_id, paragraphCount, climateParagraph, shareClimateParagraph, climateSum, sentCount)

dataFullParagraph$dok_datum = as.Date(dataFullParagraph$dok_datum)
dataFullParagraphRM = dataFullParagraph %>%
arrange(desc(dok_datum)) %>%
group_by(dok_datum) %>%
summarise(shareClimateParagraph = sum(shareClimateParagraph)) %>%
mutate(shareClimateParagraph90 = zoo::rollmean(shareClimateParagraph, k=90, fill=0)) %>% 
      dplyr::ungroup()

date <- as.Date(c("2010-11-29", "2011-11-28", "2012-12-26", "2013-11-11", "2014-09-14",
 "2014-12-01", "2015-12-15", "2016-11-07", "2017-06-01", "2017-11-06", "2018-01-01", "2018-08-20", "2018-12-02", "2019-12-02", "2020-03-11"))      
event = c( "COP16", "COP17", "COP18", "COP19", 
"Elections 2014 - Gov Change", "COP20", "COP21 - Paris Summit", "COP22", "Climate Law Agreed & Us Withdrawal from Paris ","COP23","Climate Law Entered Into Force", 
"Greta Thunberg Starts School Strike, Elections 2018", "COP24", "COP25", "WHO declares corona pandemic")
d = data.frame(date, event)

dataFullParagraphRM %>% 
  tidyr::pivot_longer(names_to = "rolling_mean_key", 
                    values_to = "rolling_mean_value", 
                    cols = c(shareClimateParagraph90
                             )) %>%

  dplyr::filter(dok_datum >= lubridate::as_date("2011-04-01") &
                 dok_datum <= lubridate::as_date("2021-01-01")) %>%
  ggplot2::ggplot(aes(x = dok_datum, 
                      y = rolling_mean_value)) +
   ggplot2::geom_line() +  
           geom_vline(data=d, mapping=aes(xintercept=date), color="grey") +
           geom_text(data=d, mapping=aes(x=date, y=0, label=event), size=4, angle=90, vjust=-0.4, hjust=0) + 
  ggplot2::labs(y = "Rolling 90-Day Average Share of Climate Paragraphs", 
                  x = "Year") + 
                 theme(
    axis.text.x = element_text(size = 14, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 14),
    strip.text = element_text(size=14)) +
  hrbrthemes::theme_ipsum_rc()
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/Rolling 90-Day Average Share of Climate Paragraphs Test.jpg")


#Share of total climate para by party

dataFullParagraph = dataParagraphFull10 %>% 
  mutate(climateParagraph = ifelse(climateSum > 0, 1, 0),
  paragraphCount = length(paragraph_id)/length(paragraph_id)) %>%
  mutate(paragraph_id = as.factor(paragraph_id), sent_id=as.factor(sent_id)) %>% 
  group_by(dok_datum, parti) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate(shareClimateParagraph = climateParagraph/paragraphCount) %>% 
  mutate(paragraphTS = sum(paragraphCount)) %>% 
  mutate(climateParaOfTotal = climateParagraph/paragraphTS) %>%
  select(dok_datum, parti,  paragraphCount, climateParaOfTotal, paragraphTS, shareClimateParagraph, climateParagraph, climateSum, sentCount)

#two variables

dataFullParagraph$dok_datum = as.Date(dataFullParagraph$dok_datum)
dataFullParagraphRM = dataFullParagraph %>%
arrange(desc(dok_datum)) %>%
group_by(parti, dok_datum) %>%  
summarise(shareClimateParagraph = sum(shareClimateParagraph), climateParaOfTotal= sum(climateParaOfTotal)) %>%
mutate(Priority = zoo::rollmean(shareClimateParagraph, k=90, fill=0),
      Influence = zoo::rollmean(climateParaOfTotal, k=90, fill=0)) %>% 
      dplyr::ungroup()

dataFullParagraphRM %>% 
  tidyr::pivot_longer(names_to = "rolling_mean_key", 
                    values_to = "rolling_mean_value", 
                    cols = c(Influence,
                             Priority)) %>%

  dplyr::filter(dok_datum >= lubridate::as_date("2011-04-01") &
                 dok_datum <= lubridate::as_date("2021-01-01")) %>%
  ggplot2::ggplot(aes(x = dok_datum, 
                      y = rolling_mean_value, 
                      shape= rolling_mean_key,
                      group = parti,
                      color = parti)) +
  facet_wrap(~rolling_mean_key, ncol = 1, scale="free_y") +
  theme(axis.text.x = element_text(size = 12)) +
  ggplot2::geom_line() + 
  ggplot2::labs(y = "Rolling 90-day Average Share", 
                  color = "Parti",
                  x = "Year") + 
                  scale_colour_manual(values=cols) +
                  scale_shape_manual(values=c("V", "S", "Mp", "C", "L", "M", "Kd", "Sd")) +
                 theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust=0.5),
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 14),
    strip.text = element_text(size=14))
ggsave("/home/petter/data/ThesisCode/Topic Model/PNG10/Rolling 90-day Influence and Priority.png")
