load(file="/home/petter/data/ThesisCode/Topic Model/RData10/dataParagraph10.Rdata")

load(file="/home/petter/data/ThesisCode/Topic Model/RData10/processedPara10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/outPara10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/docsPara10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/vocabPara10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/metaPara10.Rdata")


# Generate STM K=10-100, by 10
topic_models10_10_100_10_SK = searchK(documents = docsPara10, vocab = vocabPara10, prevalence = ~s(antaldagar) + parti, max.em.its = 75, 
                        data = outPara10$meta, K = seq.int(from = 10, to = 100, by=10), heldout.seed=123, M=10, proportion=0.5, init.type="Spectral", cores =20)

save(topic_models10_10_100_10_SK, file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_10_100_10_SK.Rdata")


#Estimate STM
plan(multisession, workers = 20)
topic_models10_5_100_5 <- tibble(K = seq.int(from = 5, to = 100, by=5)) %>%
  mutate(topic_models10_5_100_5_list= future_map(K, ~stm(documents = outPara10$documents, vocab = outPara10$vocab, K = ., 
                        prevalence = ~s(antaldagar) + parti, max.em.its = 75, 
                        data = outPara10$meta, init.type = "Spectral", seed=123), .options = furrr_options(seed = TRUE)))

save(topic_models10_5_100_5, file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_5_100_5.Rdata")


#Generate STM = 25-65, by 1  
topic_models10_35_65_1_SK = searchK(documents = docsPara10, vocab = vocabPara10, prevalence = ~s(antaldagar) + parti, max.em.its = 75, 
                        data = outPara10$meta, K = seq.int(from = 25, to = 65, by=1), heldout.seed=123, M=10, proportion=0.5, init.type="Spectral", cores =40)

save(topic_models10_35_65_1_SK, file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_35_65_1_SK.Rdata")


#Estimate STM K=29-31
plan(multisession, workers = 35)
topic_models10_29_31_1 <- tibble(K = seq.int(from = 29, to = 31, by=1)) %>%
  mutate(topic_models10_29_31_1_list = future_map(K, ~stm(documents = outPara10$documents, vocab = outPara10$vocab, K = ., 
                        prevalence = ~s(antaldagar) + parti, max.em.its = 75, 
                        data = outPara10$meta, init.type = "Spectral", seed=123), .options = furrr_options(seed = TRUE)))

save(topic_models10_29_31_1, file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_29_31_1.Rdata")

#Estimate STM K=40-55
plan(multisession, workers = 35)
topic_models10_40_55_1 <- tibble(K = seq.int(from = 40, to = 55, by=1)) %>%
  mutate(topic_models10_40_55_1_list = future_map(K, ~stm(documents = outPara10$documents, vocab = outPara10$vocab, K = ., 
                        prevalence = ~s(antaldagar) + parti, max.em.its = 75, 
                        data = outPara10$meta, init.type = "Spectral", seed=123), .options = furrr_options(seed = TRUE)))

save(topic_models10_40_55_1, file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_40_55_1.Rdata")
