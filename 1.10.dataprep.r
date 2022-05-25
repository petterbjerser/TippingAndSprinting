createDateData = function(path) {
  data = read.csv(path, encoding = "UTF-8", head=TRUE) %>%
  filter(climateSum > 0) %>%
  as_tibble() %>%
  mutate(date = as.Date(c(dok_datum)),
    startdate = "2010-10-05",
    antaldagar = round(difftime(date, startdate, units='days'), 0))
  }

createDateData10 = function(path) {
  data = read.csv(path, encoding = "UTF-8", head=TRUE) %>%
  as_tibble() %>%
  filter(climateSum > 0) %>% 
  filter(dok_datum >= lubridate::as_date("2010-10-05") & dok_datum <= lubridate::as_date("2021-07-31")) %>%
  mutate(date = as.Date(c(dok_datum)),
    startdate = "2010-10-05",
    antaldagar = round(difftime(date, startdate, units='days'), 0))
  }

createDateDataFull = function(path) {
  data = read.csv(path, encoding = "UTF-8", head=TRUE) %>%
  as_tibble() %>%
  mutate(date = as.Date(c(dok_datum)),
    startdate = "2010-10-05",
    antaldagar = round(difftime(date, startdate, units='days'), 0))
  }

createDateDataFull10 = function(path) {
  data = read.csv(path, encoding = "UTF-8", head=TRUE) %>%
  as_tibble() %>%
  filter(dok_datum >= lubridate::as_date("2010-10-05") & dok_datum <= lubridate::as_date("2021-07-31")) %>%
  mutate(date = as.Date(c(dok_datum)),
    startdate = "2010-10-05",
    antaldagar = round(difftime(date, startdate, units='days'), 0))
  }

createData = function(data) {
    data = data %>%
    arrange(date) %>%
    mutate(antaldagar = as.numeric(antaldagar),
          dok_datum = as.factor(dok_datum),
          parti = as.factor(parti),
          talare = as.factor(talare),
          anf_id = as.factor(anf_id),
          titel = as.factor(titel),
          sentCount = as.numeric(sentCount),
          anfCount = as.numeric(anfCount),
          paragraph_id = as.numeric(paragraph_id),
          sent_id = as.numeric(sent_id),
          pattern_match = as.numeric(pattern_match),
          dok_rm = as.factor(dok_rm),
          replik = as.factor(replik),
          annotation_label = as.numeric(annotation_label),
          climateSum = as.numeric(climateSum),
          climateScoreSum = as.numeric(climateScoreSum),
          cSumperSent =  as.numeric(cSumperSent),
          sentCount = as.numeric(sentCount),
          anfCount = as.numeric(anfCount)
    }

path = "/home/petter/data/ThesisCode/FeatherCode/FeatherData/dataParagraph.csv"
data = createDateData(path)
dataParagraph = createData(data)
save(dataParagraph, file='/home/petter/data/ThesisCode/Topic Model/RData/dataParagraph.Rdata')

head(dataParagraph)

data10 = createDateData10(path)
dataParagraph10 = createData(data10)
save(dataParagraph10, file='/home/petter/data/ThesisCode/Topic Model/RData/dataParagraph10.Rdata')

head(dataParagraph10)

dataFull = createDateDataFull(path)
dataParagraphFull = createData(dataFull)
save(dataParagraphFull, file='/home/petter/data/ThesisCode/Topic Model/RData/dataParagraphFull.Rdata')

dataFull10 = createDateDataFull10(path)
dataParagraphFull10 = createData(dataFull10)
save(dataParagraphFull10, file='/home/petter/data/ThesisCode/Topic Model/RData10/dataParagraphFull10.Rdata')