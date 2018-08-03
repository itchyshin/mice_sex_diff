
# Bring in data
	data <- readRDS("export/body_weight.rds")

# Packages
	devtools::install_github("thomasp85/patchwork") # Make sure to download first in a new R session
	install.packages("gridExtra")
	library(gridExtra)
	library(ggplot2)
	library(tidyverse)

# Subset data based on body weight
	bw <- subset(data, parameter_name == "body weight")

# Check first whether weight and data_point column match
	any(bw$weight == bw$data_point)

# Subset the data to get between 100 & 300 days
	bw_100_300 <- subset(bw, age_in_days > 98 & age_in_days <300)

# PLot and see what the distribution looks like
	ggplot(bw_100_300, aes(x=data_point, fill = sex)) + geom_density(alpha = 0.5)

# Function for cutting data into age bins and then calculating SD and CV for each sex and strain
sd_bin <- function(data, bins, strains){
	data$cut_age <- cut(data$age_in_days, bins)

	sd <- data %>% filter(strain_name %in% strains) %>% group_by(sex, cut_age, strain_name) %>% summarise(sd = sd(data_point), cv = sd(data_point) / mean(data_point), n = n())

	return(sd)
}

# Generate a new dataset containing SD and CV for the following three strains using the full data set so we don't restrict age.
age_sd <- sd_bin(data, 10, strains = c("C57BL/6N", "C57BL/6NTac", "C57BL/6NJ"))

# Check data
print(arrange(age_sd, strain_name), n = 30)

# Simple plot
ggplot(age_sd, aes(x=cut_age, y=cv, col = sex)) + geom_point() + facet_grid(~strain_name) + theme_classic()

# Main plot: PLOT THIS
p1 <- ggplot(bw, aes(x=age_in_days, y=data_point, col = sex)) + geom_point(alpha = 0.5) + labs(x = "Age (days)", y = "Body weight")

p2 <- ggplot(age_sd, aes(x=cut_age, y=sd, col = sex)) + geom_point(size=2) + facet_grid(~strain_name) +theme_bw() + theme(axis.text.x=element_text(size=rel(0.4))) + labs(x = "Age bin", y = "SD") 

# One way
grid.arrange(p1, p2)

# Another way
p1 / p2

## Simulating a power analysis - didn't get far with this....
# Get standard deviation in two groups
   sd <- bw_100_300 %>% group_by(sex) %>% summarise(sd = sd(data_point))
 mean <- bw_100_300 %>% group_by(sex) %>% summarise(mean = mean(data_point))

# Sample sizes
	n <- c(5, 10, 20, 30)

# Means of males and females for a trait
	m_male   <- as.numeric(mean[mean$sex == "male", "mean"])
	m_female <- as.numeric(mean[mean$sex == "female", "mean"])

# SD of males and females for a trait
	sd_male   <- as.numeric(sd[sd$sex == "male", "sd"])
	sd_female <- as.numeric(sd[sd$sex == "female", "sd"])

# pwr.t2n.test