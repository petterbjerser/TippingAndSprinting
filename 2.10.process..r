
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/dataParagraph10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData/customstopwords.Rdata")

processedPara10 = textProcessor(dataParagraph10$textcat, metadata = dataParagraph10, language="swedish", stem = FALSE, customstopwords = customstopwords, removestopwords = TRUE, removenumbers = TRUE, removepunctuation = TRUE)
outPara10 = prepDocuments(processedPara10$documents, processedPara10$vocab, processedPara10$meta) 
docsPara10 = outPara10$documents
vocabPara10 = outPara10$vocab
metaPara10 = outPara10$meta

save(processedPara10, file="/home/petter/data/ThesisCode/Topic Model/RData10/processedPara10.Rdata")
save(outPara10, file = "/home/petter/data/ThesisCode/Topic Model/RData10/outPara10.Rdata")
save(docsPara10, file= "/home/petter/data/ThesisCode/Topic Model/RData10/docsPara10.Rdata")
save(vocabPara10, file = "/home/petter/data/ThesisCode/Topic Model/RData10/vocabPara10.Rdata")
save(metaPara10, file = "/home/petter/data/ThesisCode/Topic Model/RData10/metaPara10.Rdata")

plot = plotRemoved(processedPara10$documents, lower.thresh = seq(1, 200, by = 10))