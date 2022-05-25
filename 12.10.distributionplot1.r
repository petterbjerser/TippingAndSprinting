library(tidyverse)
library(hrbrthemes)

data = read.csv("/home/petter/data/ThesisCode/FeatherCode/FeatherData/FeatherDataFull.csv", encoding = "UTF-8", head=TRUE)

data %>%
data$cs = as.numeric(data$ClimateScore)

hist(data$cs, freq=FALSE)
dplyr::
data1 = data %>%
filter(cs >= 0.99)
hist(data1$cs, freq=FALSE)


# Kernel density plot
d = density(data$cs)
d1 = density(data1$cs)

jpeg(file="/home/petter/data/ThesisCode/Topic Model/PNGs/KernelDensityPlot0.99.jpg")
plot(d1, lwd = 4, main = "Climate Score kernel density plot")
dev.off()

#count climate paragraph
dataFull = read.csv("/home/petter/data/ThesisCode/FeatherCode/FeatherData/FeatherDataFull.csv", encoding = "UTF-8", head=TRUE)
dataPara = read.csv("/home/petter/data/ThesisCode/FeatherCode/FeatherData/dataParagraph.csv", encoding = "UTF-8", head=TRUE)

sum(dataFull$anfCount)
unique(dataFull$anf_id) %>% length()

dataFullCs = dataFull %>% filter(ClimateScore > 0.99) 
unique(dataFullCs$anf_id) %>% length()


unique(dataFullCs$anf_id) %>% length() / unique(dataFull$anf_id) %>% length()

dataCS = dataPara %>% filter(climateSum > 0)
sum(dataCS$sentCount)
unique(dataCS$anf_id) %>% length()

