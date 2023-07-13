# R workshop -- This is the script I actually typed out while you were watching. 

total <- 15 #variable 

a_vector <- c("nasutus","decorus","guttatus")
a_vector <- c(a_vector, "laciniatus")

another_vector <- c(1,2,3,4)
another_vector <- 1:4
another_vector <- seq(4)
another_vector <- seq(from=1,to=4, by=1)

a_matrix <- matrix(cbind(a_vector,another_vector), ncol=2)

a_dataframe <- as.data.frame(cbind(a_vector,another_vector))
colnames <- c("species","index_no")
colnames(a_dataframe) <- colnames


a_list <- list(a=45, b=c(36,99),c="corolla",d=total)


################### Data 

flowering_time <- read.table("flowering_data.txt", header=TRUE)
germination <- read.csv("germination_data.csv")

flowering_time_clean <- na.omit(flowering_time)

germination_clean <- na.omit(germination)
germination_clean <- germination[!(germination$Name==""),]

# Of all the pots planted, how many germinated? 

test <- germination_clean[germination_clean$Germinated=="Yes",]

nrow(germination_clean[germination_clean$Germinated=="Yes",]) / nrow(germination_clean)

nrow(germination_clean[germination_clean$Germinated=="Yes",]) / nrow(germination_clean) * 100

################### tidy 

library(tidyverse)
library(ggplot2)

test <- germination_clean %>% filter(Germinated=="Yes") %>% group_by(Population) %>% tally(name="total")



##################################################
## Day2 
##################################################

flowering_time_clean$flowering_time <- 
  flowering_time_clean$Date_of_first_flower - 
  flowering_time_clean$Date_of_planting


plot <- ggplot(flowering_time_clean, aes(Population,flowering_time))
plot + geom_boxplot() + 
  annotate("text", x="ODG", y=65, label="Total Sample Size = 69")

big_label <- paste("Total Sample Size = ",nrow(flowering_time_clean),sep="")

plot + geom_boxplot() + 
  annotate("text", x="ODG", y=65, label=big_label)

plot + geom_boxplot() + 
  annotate("text", x=c("CM","CMG","OD","ODG","ODW","ODWD","ODWG"), y=65, label=c(21,4,6,10,17,9,2))

sample_sizes <- c(21,4,6,10,17,9,2)
many_labels <- paste("n",sample_sizes,sep = "=")
plot + geom_boxplot() + geom_point(data=flowering_time_clean, aes(x=Population,y=flowering_time)) + annotate("text", x=c("CM","CMG","OD","ODG","ODW","ODWD","ODWG"), y=65, label=many_labels)


plot + geom_boxplot() + geom_text(data=flowering_time_clean %>% group_by(Population) %>% summarise(top=max(flowering_time), n=n()), aes(x=Population, y=top+2, label= paste0("n = ", n)), nudge_y=1)


germination_summary <- germination_clean %>% group_by(Population,Germinated,GA_treated) %>% tally()

ggplot(germination_summary, aes(Germinated,n, fill=Population)) + geom_col() + scale_fill_viridis(discrete=TRUE)


