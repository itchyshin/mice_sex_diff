#export metacombo
#as trait-level table
write.csv(metacombo, "trait_results.csv")

# grouping-term level table 
write.csv(overall2, "GroupingTerm_results.csv")

#procedure-level table
#requires smalladditional meta-analysis on procedures
library(metafor)

meta_results <- read.csv("./trait_results.csv")

CVR_meta_procedure <- rma.mv(lnCVR ~ procedure-1, V = (lnCVR_se^2), data = meta_results)
VR_meta_procedure <- rma.mv(lnVR ~ procedure-1, V = (lnVR_se^2), data = meta_results)
RR_meta_procedure <- rma.mv(lnRR ~ procedure-1, V = (lnRR_se^2), data = meta_results)

procedure_results <- data.frame(lnCVR = coef(CVR_meta_procedure), lnVR = coef(VR_meta_procedure), lnRR = coef(RR_meta_procedure))

row.names(procedure_results) <- gsub("procedure", "", row.names(procedure_results))

write.csv(procedure_results, "procedure_results.csv")







