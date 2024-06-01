library(tidyverse)
data = data %>% na.omit()

plot(data$tvHours, data$collegeGPA)

#2
plot(data$tvHours, data$collegeGPA)
cor.test(data$tvHours, data$collegeGPA)

#4
ggplot(data, aes(x=fincialAid, y=collegeGPA)) +
  geom_boxplot()

IF <- which(data$fincialAid == "Y")
IN <- which(data$fincialAid == "N")
var.test(data[IF, "collegeGPA"], data[IN, "collegeGPA"])
t.test(data[IF, "collegeGPA"], data[IN, "collegeGPA"], var.equal=TRUE, paired=FALSE)

#6
data$collegeYear <- as.factor(data$collegeYear)
anova <- aov(tvHours ~ collegeYear, data = data)
summary(anova)