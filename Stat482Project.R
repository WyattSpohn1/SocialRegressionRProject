library(ggplot2)
library(psych)
library(car)
library(ggResidpanel)
library(plotly)
library(datasets)
library(interplot)
library(interactions)
library(dplyr)
library(tidyr)

# Load in data set
df = read.csv('Stat 482 Project Dataset.csv', header=T)
df

# Base model and summary
model = lm(df$course_grade ~ df$avg_ex1_ex2 + df$exam3 + df$semester + df$year + df$sex);model
summary(model)

# Make year a categorical variable
df$year <- factor(df$year, levels = c(2000, 2001, 2002, 2003), labels = c("2000", "2001", "2002", "2003"))

#------------------------------------------Question 1-----------------------------------------------------------------

table(df$sex)### count for woman and man 

table(df$year)### count for years 2000, 2001, 2002, and 2003 

table(df$semester)### count for semester 1 and 2 

table_1 = as.table(table(df$sex))### calculates proportion for sex category and labels it as table 1 

prop.table(table_1)### summary of proportions for sex category(repeat for categories year and semester) 

describe(df) # Describe the data set

#----------------------------------------------Question 2-------------------------------------------------------------

pairs(~avg_ex1_ex2+exam3+course_grade, data=df,  
      
      main="Scatterplot Matrix",  
      
      labels=c("Average of Exam 1 and Exam 2", "Average of Exam 3", "Overall Course Grade"))  

### Creates the scatter plot matrix for our data 

cormatrix <- cor(df[,c(6, 7, 8)], use="pairwise.complete.obs") ### Creates the correlation matri_+x 

cormatrix 

#---------------------------------------------Question 3--------------------------------------------------------------

###Question 6 Partial Correlation for first picture 

partial.r(df,c(7,8),6) 

###Question 6 Partial Correlation for second picture 

partial.r(df,c(7,6),8) 

#--------------------------------------------Question 4---------------------------------------------------------------

mean(df$avg_ex1_ex2, na.rm=TRUE) # Calculate mean for the average exam 1 and 2 grades

df$cavg_ex1_ex2 <- df$avg_ex1_ex2 - mean(df$avg_ex1_ex, na.rm=TRUE) ### Centering average of exam 1 and 2 

mean(df$exam3, na.rm=TRUE)  # Calculate mean for exam 3 grades

df$cexam3 <- df$exam3 - mean(df$exam3, na.rm=TRUE) ### Centering average of exam 3 

pairs(df[,c(7,4,8)], main= "Project Scatterplot Matrix", pch =16, 
      
      labels=c("course_grade", "exam3", "avg_ex_ex2")) ### code to create scatterplot matrix to use our variables we want to observe in the order they appear in our dataset and label them 

#----------------------------Question 5------------------------------------------------------------------

A <- lm(course_grade ~ avg_ex1_ex2*exam3, data = df) #Interaction model with no interaction

interact_plot(model=A, pred = avg_ex1_ex2, modx = exam3, plot.points=TRUE,
              interval=TRUE, x.label="Average Exam 1 and 2 Grade", y.label="Course Grade", 
              main.title="Simple Regression Lines", legend.main="Exam 3", colors="CUD", 
              line.thickness=1)+theme_bw()+  
  theme(plot.title = element_text(hjust=0.5, size = rel(1.5)))

C <- lm(course_grade ~ year*avg_ex1_ex2 , data = df) # Interaction model with interactions

interact_plot(model=C, pred = avg_ex1_ex2, modx = year, plot.points=TRUE,
              interval=TRUE, x.label="Average Exam 1 and 2 Grade", y.label="Course Grade", 
              main.title="Simple Regression Lines", legend.main="Year", colors="CUD", 
              line.thickness=1)+theme_bw()+  
  theme(plot.title = element_text(hjust=0.5, size = rel(1.5)))

E <- lm(course_grade ~ year*exam3 , data = df) # interaction model with interactions

interact_plot(model=E, pred = exam3, modx = year, plot.points=TRUE,
              interval=TRUE, x.label="Exam 3 Grade", y.label="Course Grade", 
              main.title="Simple Regression Lines", legend.main="Year", colors="CUD", 
              line.thickness=1)+theme_bw()+  
  theme(plot.title = element_text(hjust=0.5, size = rel(1.5)))

#---------------------------------------------Question 6--------------------------------------------------------------

Part2 = lm(df$course_grade~df$avg_ex1_ex2+df$exam3+df$sex+df$semester+df$year) 
            
outPart2 = fortify(Part2)  

##Create residual plot with predicted values on x-axis and standardized residuals on y-axis 

ggplot(outPart2, aes(x=.fitted, y=.stdresid)) + geom_point(shape=1, size=3) +  
  
  labs(x = "Predicted Overall Grade",y="Standardized Residuals",title="Standardized Residual Plot")+ 
  
  theme_bw()+geom_hline(yintercept=0)+geom_hline(yintercept=2)+ 
  
  geom_hline(yintercept=-2)+geom_hline(yintercept=3)+geom_hline(yintercept=-3)+ 
  
  theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+ 
  
  theme(axis.title.y = element_text(size = rel(1.4)))+ 
  
  theme(axis.title.x = element_text(size = rel(1.4)))+ 
  
  theme(axis.text.x = element_text(size = rel(1.6)))+ 
  
  theme(axis.text.y = element_text(size = rel(1.6)))+  
  
  scale_y_continuous(breaks=seq(-4,4,1)) 

qqnorm(outPart2$.resid, main = "Normal Q-Q Plot", 
       
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals", 
       
       plot.it = TRUE, datax = FALSE)  ##To run this code, you need to highlight all 3 rows at the same time, then click run. 

qqline(outPart2$.resid)  ##Adds line to plot 

hist(outPart2$.resid, main="Histogram of Residuals", xlab="Residuals") 

##Step 1: Create two groups 

outPart2$yHatCategory <- ifelse(outPart2$.fitted < median(outPart2$.fitted), c("group1"), c("group2"))  

##Step 2: Run test 

leveneTest(.resid ~ yHatCategory, data=outPart2) 

shapiro.test(outPart2$.resid) 

#-------------------------------------------Question 7----------------------------------------------------------------

modelA <- lm(df$course_grade~df$cavg_ex1_ex2+df$cexam3+df$semester+df$year+df$sex) 

summary(modelA) ### Gives us summary for model A 

modelB = lm(df$course_grade ~ df$avg_ex1_ex2 + df$exam3 + df$year + df$sex + df$year*df$exam3)

summary(modelB) ### Gives us summary for model B

modelC <- lm(df$course_grade~df$avg_ex1_ex2+df$exam3) 

summary(modelC) ### Gives us summary for model C 

modelD <- lm(df$course_grade~df$avg_ex1_ex2+df$exam3+df$semester+df$year+df$sex) 

summary(modelD) ### Gives us summary for model D 

confint(modelA, level=0.95) # code for model A confidence intervals

confint(modelB, level=0.95) # code for model B confidence intervals

confint(modelC, level=0.95) # code for model C confidence intervals

confint(modelD, level=0.95) # code for model D confidence intervals

#------------------------------------------Question 9-----------------------------------------------------------------

ModelGrades<-lm(df$course_grade~df$avg_ex1_ex2+df$exam3+df$year+df$sex+df$exam3*df$year) ### code to arrange our regression equation and add an interaction between exam3 and year. 
                
summary(ModelGrades) ### code to show us the intercept and slopes for our least squares regression equation 

#------------------------------------------Question 10-----------------------------------------------------------------

Part3 <- lm(df$course_grade~df$avg_ex1_ex2+df$exam3+df$sex+df$year+df$year*df$exam3) 

##Checking Assumptions 

outPart3 = fortify(Part3) 

##Create residual plot with predicted values on x-axis and standardized residuals on y-axis 

ggplot(outPart3, aes(x=.fitted, y=.stdresid)) + geom_point(shape=1, size=3) +  
  
  labs(x = "Predicted Overall Grade",y="Standardized Residuals",title="Standardized Residual Plot")+ 
  
  theme_bw()+geom_hline(yintercept=0)+geom_hline(yintercept=2)+ 
  
  geom_hline(yintercept=-2)+geom_hline(yintercept=3)+geom_hline(yintercept=-3)+ 
  
  theme(plot.title = element_text(hjust=0.5, size = rel(1.6)))+ 
  
  theme(axis.title.y = element_text(size = rel(1.4)))+ 
  
  theme(axis.title.x = element_text(size = rel(1.4)))+ 
  
  theme(axis.text.x = element_text(size = rel(1.6)))+ 
  
  theme(axis.text.y = element_text(size = rel(1.6)))+  
  
  scale_y_continuous(breaks=seq(-4,4,1)) 

qqnorm(outPart3$.resid, main = "Normal Q-Q Plot", 
       
       xlab = "Theoretical Normal Quantiles", ylab = "Sample Residuals", 
       
       plot.it = TRUE, datax = FALSE)  ##To run this code, you need to highlight all 3 rows at the same time, then click run. 

qqline(outPart3$.resid)  ##Adds line to plot 

hist(outPart3$.resid, main="Histogram of Residuals", xlab="Residuals") 

##Step 1: Create two groups 

outPart3$yHatCategory <- ifelse(outPart3$.fitted < median(outPart3$.fitted), c("group1"), c("group2"))  

##Step 2: Run test 

leveneTest(.resid ~ yHatCategory, data=outPart3) 

shapiro.test(outPart3$.resid) 

#------------------------------------------Question 11-----------------------------------------------------------------

k <- 9 

n <- nrow(df) 

# Calculate cutoffs
(lev.cutoff.low<-2*(k+1)/n) 

(lev.cutoff.high<-3*(k+1)/n) 

outPart4 <- merge(df, outPart3, by = "row.names") 

# create graph for interaction plot
levPlotPart3 <- ggplot(outPart4, aes(x=.hat, y=.stdresid, size=.cooksd))+ 
  
  geom_point() + 
  
  labs(x = "Leverage Values",y="Studentized Residuals",title="Leverage and Influence Plot")+ 
  
  theme_bw()+ 
  
  xlim(0, .45)+ylim(-5, 5)+ 
  
  geom_hline(yintercept=0)+ 
  
  geom_hline(yintercept=2, colour="blue")+ 
  
  geom_hline(yintercept=-2, colour="blue")+ 
  
  geom_hline(yintercept=3, colour="red")+ 
  
  geom_hline(yintercept=-3, colour="red")+ 
  
  geom_vline(xintercept=lev.cutoff.low, colour="blue")+ 
  
  geom_vline(xintercept=lev.cutoff.high, colour="red")+ 
  
  geom_text(aes(x=-.5,y=4,label="leverage cut-off=0.0803",size=NULL),vjust=1.5)+ 
  
  geom_text(aes(x=1,y=-4,label="leverage cut-off=0.1204",size=NULL),vjust=1.5)+ 
  
  theme(plot.title = element_text(hjust=0.5, size = rel(2)))+ 
  
  theme(axis.title.y = element_text(size = rel(1.4)))+ 
  
  theme(axis.title.x = element_text(size = rel(1.4)))+ 
  
  theme(axis.text.x = element_text(size = rel(1.6)))+ 
  
  theme(axis.text.y = element_text(size = rel(1.6)))+ 
  
  guides(size=guide_legend("Cook's D")) 

InteractiveLevPlot=ggplotly(levPlotPart3) 

InteractiveLevPlot 

#------------------------------------------Question 12-----------------------------------------------------------------

# Summary and anova analysis
summary(Part3) 

anova(Part3) 

#------------------------------------------Question 13-----------------------------------------------------------------

confint(Part3, level=0.95) 

###CI for table 

#------------------------------------------Question 14-----------------------------------------------------------------

# Reshape the data to calculate averages
reshaped_data <- aggregate(cbind(exam1, exam2, exam3) ~ year + sex + semester, data = df, FUN = mean)
reshaped_data

data_long <- reshape2::melt(reshaped_data, id.vars = c("year", "sex", "semester"))
data_long

# Combine the 'variable' and 'Sex' columns into a new column
data_long$combined <- paste(data_long$variable, data_long$sex, sep = "_")
# Combine the 'year' and 'semester' columns into a new column
data_long$yearsem <- paste(data_long$year, data_long$semester, sep = "_")
data_long

# Plotting the line graph with the combined column
ggplot(data_long, aes(x = yearsem, y = value, color = combined)) +
  geom_line(aes(group = interaction(combined, sex))) +
  geom_point() +
  labs(title = "Average Exam Grades by Year and Gender",
       x = "Year",
       y = "Average Exam Grades") +
  scale_color_manual(values = c("exam1_Man" = "red", "exam2_Man" = "orange", "exam3_Man" = "gold",
                                "exam1_Woman" = "green", "exam2_Woman" = "blue", "exam3_Woman" = "purple")) +
  theme_minimal()






















