
# Felix laptop
setwd("C:/Users/felix/OneDrive - UNSW/research UNSW/Project - mice sex power allometry 2018")

library(readr)
library(dplyr)
library(metafor)

#save in a folder called "export"
source("meta_analysis.R")
source("data_load_clean2.R")
source("calc_pop_stats.R")


# Prepare data 

# Load raw data - save cleaned dataset as RDS for reuse
#data_raw <- load_raw("data/dr7.0_all_control_data.csv") %>%
#    clean_raw_data()
#dir.create("export", F, F)
#saveRDS(data_raw, "export/data_clean.rds")

data1 <- readRDS("export/data_clean.rds")

# CLEAN DATA: select traits with at least 2 centers
dat1 <-
  data1 %>%
  group_by(parameter_name) %>%
  summarize(center_per_trait = length(unique(production_center, na.rm = TRUE)))
#dat1$center_per_trait

dat2 <- merge(data1, dat1) #sollte auch im grossen Datensatz so funktionieren, also ohne die uebereinstimmende Variable (trait) anzugeben
dat_moreThan1center <-
  dat2  %>%
  filter(center_per_trait >= 2)

data2 <- dat_moreThan1center
min(data2$center_per_trait) #=2;ok!

# Define population variable
data3 <- data2 %>%
mutate(population = sprintf("%s-%s", production_center, strain_name))

# Assign each unique parameter_name (=trait) a unique number ('id')
data <- transform(data3, id = match(parameter_name, unique(parameter_name)))
tail(data)
n <- length(unique(data$id)) #=1:232 = 232 unique traits

#############################################################################################################################################

# Create matrix to store results for all traits
results.alltraits <- as.data.frame(cbind(c(1:n), matrix(rep(0, n*9), ncol = 9))) #number of individual results per trait = 9
names(results.alltraits) <- c("id", "CVR", "CVR_lower", "CVR_upper", "CV", "CV_lower", "CV_upper", "mean", "mean_lower", "mean_upper")

# LOOP

for(i in 1:n) {

tryCatch({

data_par_age <- data_subset_parameterid_individual_by_age(data, i, age_min = 0, age_center = 100)

population_stats <- calculate_population_stats(data_par_age)

results <- create_meta_analysis_effect_sizes(population_stats)

# for CVR
cvr <- metafor::rma.mv(yi = effect_size_CVR, V = sample_variance_CVR, random = list(~1| strain_name, ~1|production_center, ~1|err), data = results)
results.alltraits[i, 2] <- cvr$b
results.alltraits[i, 3] <- cvr$ci.lb
results.alltraits[i, 4] <- cvr$ci.ub

# for VR
cv <- metafor::rma.mv(yi = effect_size_VR, V = sample_variance_VR, random = list(~1| strain_name, ~1|production_center, ~1|err), data = results)
results.alltraits[i, 5] <- cv$b
results.alltraits[i, 6] <- cv$ci.lb
results.alltraits[i, 7] <- cv$ci.ub

# for means
means <- metafor::rma.mv(yi = effect_size_RR, V = sample_variance_RR, random = list(~1| strain_name, ~1|production_center, ~1|err), data = results)
results.alltraits[i, 8] <- means$b
results.alltraits[i, 9] <- means$ci.lb
results.alltraits[i, 10] <- means$ci.ub

		}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
	}

results.alltraits

# Errors, in order (n=22, look for 0.000 in results.alltraits)
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Processing terminated since k <= 1. 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Optimizer (nlminb) did not achieve convergence (convergence = 1). 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 
ERROR : Processing terminated since k <= 1. 

results.alltraits$parameter_name <- data$parameter_name[match(results.alltraits$id, data$id)]

write.csv(results.alltraits, file = "results_alltraits_4-10-2018.csv")