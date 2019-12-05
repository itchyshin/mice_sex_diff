

library(metafor)

meta_results <- read.csv("./trait_meta_results.csv")

CVR_meta_procedure <- rma.mv(lnCVR ~ procedure-1, V = (lnCVR_se^2), data = meta_results)
 VR_meta_procedure <- rma.mv(lnVR ~ procedure-1, V = (lnVR_se^2), data = meta_results)
 RR_meta_procedure <- rma.mv(lnRR ~ procedure-1, V = (lnRR_se^2), data = meta_results)

procedure_results <- data.frame(lnCVR = coef(CVR_meta_procedure), lnVR = coef(VR_meta_procedure), lnRR = coef(RR_meta_procedure))

row.names(procedure_results) <- gsub("procedure", "", row.names(procedure_results))

write.csv(procedure_results, "procedure_results.csv")

## Grouping
CVR_meta_GroupingTerm <- rma.mv(lnCVR ~ GroupingTerm-1, V = (lnCVR_se^2), data = meta_results)
VR_meta_GroupingTerm <- rma.mv(lnVR ~ GroupingTerm-1, V = (lnVR_se^2), data = meta_results)
RR_meta_GroupingTerm <- rma.mv(lnRR ~ GroupingTerm-1, V = (lnRR_se^2), data = meta_results)

GroupingTerm_results <- data.frame(lnCVR = coef(CVR_meta_GroupingTerm), lnVR = coef(VR_meta_GroupingTerm), lnRR = coef(RR_meta_GroupingTerm))

row.names(GroupingTerm_results) <- gsub("GroupingTerm", "", row.names(GroupingTerm_results))

write.csv(GroupingTerm_results, "GroupingTerm_results.csv")