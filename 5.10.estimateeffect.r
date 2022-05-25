load(file="/home/petter/data/ThesisCode/Topic Model/RData10/outPara10.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_46_53_1.Rdata")
load(file="/home/petter/data/ThesisCode/Topic Model/RData10/topic_models10_29_31_1.Rdata")

estimateEffect10_30 = estimateEffect(c(1:30) ~ s(antaldagar) + parti + parti:s(antaldagar), 
  stmobj= topic_models10_29_31_1[[2]][[2]], meta = outPara10$meta, uncertainty = "Global")
save(estimateEffect10_30, file="/home/petter/data/ThesisCode/Topic Model/RData10/estimateEffect10_30.Rdata") 

estimateEffect10_52 = estimateEffect(c(1:52) ~ s(antaldagar) + parti + parti:s(antaldagar), 
  stmobj= topic_models10_46_53_1[[2]][[7]], meta = outPara10$meta, uncertainty = "Global")
save(estimateEffect10_52, file="/home/petter/data/ThesisCode/Topic Model/RData10/estimateEffect10_52.Rdata") 
