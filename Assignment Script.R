#Set working directory
setwd("C:/Users/pie93/Desktop/Data Analytics/Assignment")

#Load environment
load('myEnvironment.RData')
theme_set(theme_bw(base_size=18))

rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

#Libraries Used
library(VIM)
library(caret)
library(janitor)
library(rpart) #Classification Tree
library(outliers)
library(ggrepel)
library(randomForest)
library(randomForestExplainer)
library(e1071)
library(iml)
library(devtools)
library(pdp)

#Save Environment
save.image(file='myEnvironment.RData')

#Import Dataset
diamond<-read.csv("diamonddata.csv",header = TRUE)

#Exploration of Data
#Ensure Variables are Stored Correctly
summary(diamond)
str(diamond)

#Check for Duplicate Entries
chdup<-duplicated(diamond)
table(chdup)
#No duplicate entries found

#Table %
length(which(diamond$table%%1 == 0))

#Check for Missing Data
oaggr<- aggr(diamond)
summary(oaggr)
#No missing data
#Check for 0s in carat weight
which(diamond$carat < 0.000001)
which(diamond$depth < 0.000001)
which(diamond$table < 0.000001)
#Check for 0s in size dimensions
which(diamond$x < 0.000001)
which(diamond$y < 0.000001)
length(which(diamond$z < 0.000001))
tabyl(which(diamond$x < 0.000001), which(diamond$y < 0.000001), which(diamond$z < 0.000001))
diamond[which(diamond$z < 0.000001),]
#Remove values
diamond <- diamond[-which(diamond$z < 0.000001),]

#Check for near zero variance in any variables
x<-nearZeroVar(diamond,saveMetrics=TRUE)
x
#No variables with a large proportion of identical values

#Missing Classifications
levels(diamond$clarity)

#Store data with outliers in case
diamondWithOutliers <- diamond

#Outliers
summary(diamond$carat)
#Carat
id1 <- boxplot.stats(diamond$carat, coef = 2)
diamond <- diamond[-which(diamond$carat > id1$stats[5]),]
# No. of outliers: 250
#None below lower cutoff
#Depth
id1 <- boxplot.stats(diamond$depth, coef = 2)
diamond <- diamond[-which(diamond$depth > id1$stats[5]),]
diamond <- diamond[-which(diamond$depth < id1$stats[1]),]
# No. of outliers: 1130
#None below lower cutoff
#Table
id1 <- boxplot.stats(diamond$table, coef = 2)
diamond <- diamond[-which(diamond$table > id1$stats[5]),]
diamond <- diamond[-which(diamond$table < id1$stats[1]),]
# No. of outliers: 84
#X
id1 <- boxplot.stats(diamond$x, coef = 2)
# No. of outliers: 2
#None left
#Y
id1 <- boxplot.stats(diamond$y, coef = 2)
# No. of outliers: 3
diamond <- diamond[-which(diamond$y > id1$stats[5]),]
#None below lower cutoff
#Z
id1 <- boxplot.stats(diamond$z, coef = 1.5)
# No. of outliers: 29
diamond <- diamond[-which(diamond$z > id1$stats[5]),]
diamond <- diamond[-which(diamond$z < id1$stats[1]),]

#Examine Relationships

#Correlation between depth and depth %
#Depth vs Depth %
ggplot(data = diamond[-which(diamond$z > 30),], mapping = aes(x = depth, y = z)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond Depth (mm)", x="Diamond Depth (%)") + ggtitle("Diamond Depth Versus Depth Percentage") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
cor(diamond$depth, diamond$z)
#Pearson correlation coefficient
#Suggests weak positive relationship, not enough to remove from study
#Depth % vs Depth/Length
ggplot(data = diamond[-which(diamond$z > 30),], mapping = aes(x = depth, y = z/x)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond Depth/Diamond Length", x="Diamond Depth (%)") + ggtitle("Diamond Depth/Diamond Length Versus Depth Percentage") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
cor(diamond[-which(diamond$x < 0.000001),]$depth, diamond[-which(diamond$x < 0.000001),]$z/diamond[-which(diamond$x < 0.000001),]$x)
#Depth % vs Depth/Width
ggplot(data = diamond[-which(diamond$z > 30),], mapping = aes(x = depth, y = z/y)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond Depth/Diaomnd Width", x="Diamond Depth (%)") + ggtitle("Diamond Depth/Diamond Width Versus Depth Percentage") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
cor(diamond$depth, diamond$z/diamond$y)

#Correlation between width and table %
#Width vs Table %
temp <- diamond[-which(diamond$y > 30),]
ggplot(data = temp[which(temp$table < 90),], mapping = aes(x = table, y = y)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond Width (mm)", x="Diamond Table (%)") + ggtitle("Diamond Width Versus Table Percentage") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
cor(diamond$table, diamond$y)
#Pearson correlation coefficient
#Suggests weak positive relationship, not enough to remove from study
#Table % vs Width/Length
ggplot(data = diamond[-which(diamond$y > 30),], mapping = aes(x = table, y = y/x)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond Width/Diamond Length ", x="Diamond Table (%)") + ggtitle("Diamond Width/Diamond Length Versus Table Percentage") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
cor(diamond[-which(diamond$x < 0.000001),]$table, diamond[-which(diamond$x < 0.000001),]$y/diamond[-which(diamond$x < 0.000001),]$x)
#Table % vs Width/Depth
ggplot(data = diamond, mapping = aes(x = table, y = y/z)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond Width/Diamond Depth", x="Diamond Table (%)") + ggtitle("Diamond Width/Diamond Depth Versus Table Percentage") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
cor(diamond[-which(diamond$z == 0),]$table, diamond[-which(diamond$z == 0),]$y/diamond[-which(diamond$z == 0),]$z)


#Price vs Carat
ggplot(data = diamond, mapping = aes(x = carat, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Carat weight") + ggtitle("Diamond Price Versus Carat Weight")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#With line of best fit
ggplot(data = diamond, mapping = aes(x = carat, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Carat weight") + ggtitle("Diamond Price Versus Carat Weight")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm')
cor(diamond$price, diamond$carat)
#Investigate the weird vertical lines at intervals 1, 1.5 and 2 (may be large no at these specific weights, but why is distribution so wide)

#Price vs Cut
ggplot(data = diamond, mapping = aes(cut, price, fill = cut)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Cut", y="Price of Diamond") + ggtitle("Diamond Price by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("Ideal", "Premium", "Very Good", "Good", "Fair"))
#Doesnt seem to be much correlation between cut and price, if anything slightly negative relationship

#Price vs Color
ggplot(data = diamond, mapping = aes(color, price, fill = color)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Colour", y="Price of Diamond") + ggtitle("Diamond Price by Colour")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#These seem to be negatively correlated with price in terms of what is supposedly best

#Price vs clarity
ggplot(data = diamond, mapping = aes(clarity, price, fill = clarity)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Clarity", y="Price of Diamond") + ggtitle("Diamond Price by Clarity")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))
#General tend towards a negative relationship; as the clarity increases, the prices decreased.

#Price vs Depth
depthPlot <- ggplot(data = diamond, mapping = aes(x = depth, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Diamond Depth Percentage") + ggtitle("Diamond Price Versus Depth Percentage")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
depthPlot + stat_function(colour = "red", size = 0.9, fun = function(x) dnorm(x, mean = mean(diamond$depth), sd = sd(diamond$depth*2)) * max(diamond$price) * 6)
cor(diamond$price, diamond$depth)
#Vast majority of values between 55 and 70 percent mark, says almost nothing about price
#Is there a case for removing this?

#Price vs table
tablePlot <- ggplot(data = diamond, mapping = aes(x = table, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Diamond Table (%)") + ggtitle("Diamond Price Versus Table Percentage")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
tablePlot  + stat_function(colour = "red", size = 0.9, fun = function(x) dnorm(x, mean = mean(diamond$table), sd = sd(diamond$table*1.5)) * max(diamond$price) * 9)
#Similar to depth, with one outlier at well over 90% depth, large lines occuring at regular intervals (percentages recorded to whole number?)
#Huge number of values at fixed intervals; were some diamonds rounded to nearest % when being meausured?

#Price vs x
ggplot(data = diamond, mapping = aes(x = x, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Diamond length(mm)") + ggtitle("Diamond Price Versus Length")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Entries for 0 - how does a diamond have 0 length? Nothing between 0 and roughly 3.3
#Clear exponential growth in price as length increases
cor(diamond$price, diamond$x)
#Cor captures linear relationship, not necessarily exponential relationship

#Price vs y
ggplot(data = diamond, mapping = aes(x = y, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Diamond width(mm)") + ggtitle("Diamond Price Versus Width")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Entries for 0 - how does a diamond have 0 width? Nothing between 0 and roughly 4/5
#Clear exponential growth in price as width increases
#One outlier with width near 60mm - shoudl this be removed?
cor(diamond$price, diamond$y)
#Cor captures linear relationship, not necessarily exponential relationship

#Price vs z
ggplot(data = diamond, mapping = aes(x = z, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Diamond depth(mm)") + ggtitle("Diamond Price Versus Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Entries for 0 - how does a diamond have 0 width? Nothing between 0 and roughly 4/5
#Clear exponential growth in price as depth increases
#One outlier with depth near 30mm - should this be removed?
cor(diamond$price, diamond$z)
#Cor captures linear relationship, not necessarily exponential relationship

#Does carat peak on price due to negative correlation with another important variable?
#Carat vs clarity
ggplot(data = diamond, mapping = aes(clarity, carat, fill = clarity)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Clarity", y="Carat Weight") + ggtitle("Diamond Carat by Clarity")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))
#Very strong negative relationship is evident, alligning with price (In fact this may be what causes neg trend of clarity)

#Carat vs x
ggplot(data = diamond, mapping = aes(x = x, y = carat)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Carat Weight", x="Diamond length(mm)") + ggtitle("Carat of Diamond Versus Length")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Very clear exponential growth, again matching price

#Carat vs y
ggplot(data = diamond, mapping = aes(x = y, y = carat)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Carat Weight", x="Diamond width(mm)") + ggtitle("Carat of Diamond Versus Width")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Very clear exponential growth, again matching price

#Carat vs z
ggplot(data = diamond, mapping = aes(x = z, y = carat)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Carat Weight", x="Diamond depth(mm)") + ggtitle("Carat of Diamond Versus Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Very clear exponential growth, again matching price, however there is several more outliers present

#Relationship between x, y and z
#X vs Y
ggplot(data = diamond, mapping = aes(x = x, y = y)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond width(mm)", x="Diamond length(mm)") + ggtitle("Diamond Length Versus Width")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + geom_smooth(method='lm', color = "red")
cor(diamond$x, diamond$y)
#Extremely high correlation, good grounds for combining them

#X vs Z
ggplot(data = diamond, mapping = aes(x = x, y = z)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond depth(mm)", x="Diamond length(mm)") + ggtitle("Diamond Length Versus Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(diamond$x, diamond$z)
#Extremely high correlation, good grounds for combining them

#Y VS Z
ggplot(data = diamond, mapping = aes(x = z, y = y)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond width(mm)", x="Diamond depth(mm)") + ggtitle("Diamond Depth Versus Width")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(diamond$z, diamond$y)
#Extremely high correlation, good grounds for combining them

#Derived variable: size
diamond$size <- diamond$x * diamond$y * diamond$z
diamondWithAllVariables <- diamond
diamond<- diamond[,-c(9,10,11)]

#Price vs Size
ggplot(data = diamond, mapping = aes(x = size, y = price)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Price of Diamond", x="Diamond size (mm^3)") + ggtitle("Diamond Price Versus Size")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))

cor(diamond[,c(2,6,7,9)])

#Size vs Carat
ggplot(data = diamond, mapping = aes(x = size, y = carat)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Carat Weight of Diamond", x="Diamond size (mm^3)") + ggtitle("Carat Weight Versus Size")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(diamond$size, diamond$carat)
var(diamond$size)
var(diamond$carat)

#Remove carat weight
diamond <- diamond[,-2]

#Cut: Why is it uncorrelated to price
#VS Colour
table(diamond$cut, diamond$color)
#VS Clarity
table(diamond$cut, diamond$clarity)
#VS Depth
ggplot(data = diamond, mapping = aes(cut, depth, fill = cut)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Cut", y="Diamond Depth") + ggtitle("Diamond Depth by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("Fair","Good","Very Good","Premium", "Ideal"))
#Negative relationship
#VS Table
ggplot(data = diamond, mapping = aes(cut, table, fill = cut)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Cut", y="Diamond Table") + ggtitle("Diamond Table by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("Fair","Good","Very Good","Premium", "Ideal"))
#VS Size
ggplot(data = diamond, mapping = aes(cut, size, fill = cut)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Cut", y="Diamond Size") + ggtitle("Diamond Size by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("Fair","Good","Very Good","Premium", "Ideal"))

#Color
#VS Clarity
table(diamond$color, diamond$clarity)
#VS Depth
ggplot(data = diamond, mapping = aes(color, depth, fill = color)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Depth", y="Price of Diamond") + ggtitle("Diamond Price by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#VS Table
ggplot(data = diamond, mapping = aes(color, table, fill = color)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Table", y="Price of Diamond") + ggtitle("Diamond Price by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#VS Size
ggplot(data = diamond, mapping = aes(color, size, fill = color)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Colour", y="Size of Diamond") + ggtitle("Diamond Size by Colour")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Positive relationship between colour and size

#Clarity
#VS Depth
ggplot(data = diamond, mapping = aes(clarity, depth, fill = clarity)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Clarity", y="Diamond Depth") + ggtitle("Diamond Depth by Clarity")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))
#Variation of clarity tended to decrease for higher clarities
#VS Table
ggplot(data = diamond, mapping = aes(clarity, table, fill = clarity)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Table", y="Price of Diamond") + ggtitle("Diamond Price by Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))
#VS Size
ggplot(data = diamond, mapping = aes(clarity, size, fill = clarity)) +  geom_boxplot() + stat_boxplot(geom="errorbar") +labs(x="Diamond Clarity", y="Diamond Size") + ggtitle("Diamond Size by Clarity")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))
#Diamonds with better clarity tended to be smaller

#Depth
#VS Table
ggplot(data = diamond, mapping = aes(x = depth, y = table)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond depth(mm)", x="Diamond length(mm)") + ggtitle("Diamond Length Versus Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
#VS Size
ggplot(data = diamond, mapping = aes(x = depth, y = size)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond depth(mm)", x="Diamond length(mm)") + ggtitle("Diamond Length Versus Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")

#Table
#VS Size
ggplot(data = diamond, mapping = aes(x = table, y = size)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Diamond depth(mm)", x="Diamond length(mm)") + ggtitle("Diamond Length Versus Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")

#CreateDataPartition, removing ID
set.seed(5)
?createDataPartition
inTrain<-createDataPartition(y=diamond[,-1]$price, p=0.8,list=FALSE)
diamondTrain<-diamond[inTrain,-1]
diamondTest<-diamond[-inTrain,-1]
prop.table(table(diamond$color))
prop.table(table(diamondTrain$color))
prop.table(table(diamondTest$color))

#Analysis using trees
start_time <- Sys.time()
fitTree = rpart(price ~.,data=diamondTrain, cp=0, method = "anova")
end_time <- Sys.time()
timeRegTree <- end_time - start_time

fitTree = rpart(price ~.,data=diamondTrain, cp=0, method = "anova")
#Determine cutoff
cpt<-fitTree$cptable # where fit is the name of the object created by rpart.
i.min<-which.min(cpt[,4]) #Min xerror
i.se <-which.min (abs(cpt[,4]-(cpt[i.min,4]+cpt[i.min,5]))) #xerror closest to min xerror+its SE
cp.best<-cpt[i.se,1] #Best cp = cp of xerror equal to xerrormin - SE
fitTree1<-prune.rpart(fitTree,cp.best, method = "anova")
printcp(fitTree1)
#Variable Importance
#Size
fitTree1$variable.importance[1]/sum(fitTree1$variable.importance)
fitTree1$variable.importance[2]/sum(fitTree1$variable.importance)
fitTree1$variable.importance[3]/sum(fitTree1$variable.importance)
fitTree1$variable.importance[4]/sum(fitTree1$variable.importance)
fitTree1$variable.importance[5]/sum(fitTree1$variable.importance)
fitTree1$variable.importance[6]/sum(fitTree1$variable.importance)
#Model Effectiveness
treePred <- predict(fitTree1, diamondTest)
treeValues <- data.frame(diamondTest$price)
treeValues$observed <- treeValues$diamondTest.price
treeValues$predicted <- treePred
ggplot(data = treeValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Regression Tree")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(treeValues$observed, treeValues$predicted)
rmse(treeValues$predicted, treeValues$observed)

#Linear Regression Model
start_time <- Sys.time()
linreg <- glm(price~.^2, family = gaussian, diamondTrain)
end_time <- Sys.time()
timeLinReg <- end_time - start_time

linreg <- glm(price~.^2, family = gaussian, diamondTrain)
summary(linreg)
#Refine (remove all with >5% P-value)
formula <-price~cut + color + clarity + cut * color + cut * clarity + cut * depth + cut * table + cut * size + color * clarity + color * depth + color * table + color * size + clarity * table + clarity * size + table * size
linreg2 <- glm(formula, family = gaussian, diamondTrain)
summary(linreg2)
#Model Effectiveness
lregPred <- predict(linreg2, diamondTest)
lregValues <- data.frame(diamondTest$price)
lregValues$observed <- lregValues$diamondTest.price
lregValues$predicted <- lregPred
ggplot(data = lregValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Linear Regression")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(lregValues$observed, lregValues$predicted)
rmse(lregValues$observed, lregValues$predicted)
#Performance so high due to linearity of relationships between price and othe rvariables of importance
#Test their contribution to the prediciton of the interaction terms
#price~cut + color + clarity + cut * color + cut * clarity + cut * depth + cut * table + cut * size + color * clarity + color * depth + color * table + color * size + clarity * table + clarity * size + table * size'
formula2 <-price~cut + color + clarity  + cut * color + cut * clarity + cut * depth + cut * table + cut * size  + color * clarity + color * depth + color * table + color * size +  clarity * table + clarity * size
linregTemp <- glm(formula2, family = gaussian, diamondTrain)
#Model Effectiveness
lregPred2 <- predict(linregTemp, diamondTest)
lregValues2 <- data.frame(diamondTest$price)
lregValues2$observed <- lregValues2$diamondTest.price
lregValues2$predicted <- lregPred2
rmse(lregValues2$observed, lregValues2$predicted)

#Tuning random forest
#Build 10 different random samples of 2000 from the training data, create RF of each hyperparameter combination from these and determine best performance using OoB cases
subDiamondTrain <- diamondTrain[sample(nrow(diamondTrain), 2000), ]
grid <- expand.grid(mtry = c(2,sqrt(7),6), ntree = c(200, 500, 1000), cp = c(0.000, cp.best, 0.01), boot.size = c(0.33 * nrow(subDiamondTrain), 0.66 * nrow(subDiamondTrain), 1 * nrow(subDiamondTrain)))
msr <- matrix(0L, nrow = 81, ncol = 10)
colnames(msr) <- c("MSR1","MSR2","MSR3","MSR4","MSR5","MSR6","MSR7","MSR8","MSR9", "MSR10")
varExp <- matrix(0L, nrow = 81, ncol = 10)
colnames(varExp) <- c("varExp1","varExp2","varExp3","varExp4","varExp5","varExp6","varExp7","varExp8","varExp9", "varExp10")
best <- data.frame( bestMSR = numeric(10), bestVarExp = numeric(10))
for(j in 1:10)
{
  subDiamondTrain <- diamondTrain[sample(nrow(diamondTrain), 2000), ]
  perf <- grid
  perf$msr <- 0
  perf$varExp <- 0
  for (i in 1:nrow(perf))
  {
    fitTemp <- randomForest(price~.,data=subDiamondTrain, mtry = perf$mtry[i], ntree=perf$ntree[i], importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = perf$cp[i]), sampsize = perf$boot.size[i])
    perf$msr[i] <- fitTemp$mse[fitTemp$ntree]
    perf$varExp[i] <- fitTemp$rsq[fitTemp$ntree]
    msr[i,j] <- fitTemp$mse[fitTemp$ntree]
    varExp[i,j] <- fitTemp$rsq[fitTemp$ntree]
  }
  #Determine best hyperparameters
  best$bestMSR[j] <- which(perf$msr == min(perf$msr))
  best$bestVarExp[j] <-which(perf$varExp == max(perf$varExp))
}
write.csv(msr, "C:/Users/pie93/Desktop/Data Analytics/Assignment/msrData.csv")
write.csv(grid, "C:/Users/pie93/Desktop/Data Analytics/Assignment/grid.csv")
write.table(varExp, "C:/Users/pie93/Desktop/Data Analytics/Assignment/varExpData.txt", sep="\t")
write.table(best, "C:/Users/pie93/Desktop/Data Analytics/Assignment/bestData.txt", sep="\t")
best <- read.table("C:/Users/pie93/Desktop/Data Analytics/Assignment/bestData.txt", sep="\t")
msr <- read.table("C:/Users/pie93/Desktop/Data Analytics/Assignment/msrData.txt", sep="\t")
for(i in 1:10)
{
  print(grid[which(msr[,i] == max(msr[,i])),])
}
#Occurance in best performing model
bestPerfVar <- data.frame(NumberOfVariables = c("2","3","6"), AverageMSE = c(mean(unlist(msr[which(grid$mtry == 2),], use.names = F)),mean(unlist(msr[which(grid$mtry == grid$mtry[2]),], use.names = F)),mean(unlist(msr[which(grid$mtry == 6),], use.names = F))), VarMSE = c(sd(unlist(msr[which(grid$mtry == 2),], use.names = F)),sd(unlist(msr[which(grid$mtry == grid$mtry[2]),], use.names = F)),sd(unlist(msr[which(grid$mtry == 6),], use.names = F))))
bestPerfTreeNo <- data.frame(NumberOfTrees = c(200, 500, 1000), AverageMSE = c(mean(unlist(msr[which(grid$ntree == 200),], use.names = F)),mean(unlist(msr[which(grid$ntree == 500),], use.names = F)),mean(unlist(msr[which(grid$ntree == 1000),], use.names = F))), VarMSE = c(sd(unlist(msr[which(grid$ntree == 200),], use.names = F)),sd(unlist(msr[which(grid$ntree == 500),], use.names = F)),sd(unlist(msr[which(grid$ntree == 1000),], use.names = F))))
bestPerfSampSize <- data.frame(SampleSize = c("33%", "66%", "100%"), AverageMSE = c(mean(unlist(msr[which(grid$boot.size == 660),], use.names = F)),mean(unlist(msr[which(grid$boot.size == 1320),], use.names = F)),mean(unlist(msr[which(grid$boot.size == 2000),], use.names = F))), VarMSE = c(sd(unlist(msr[which(grid$boot.size == 660),], use.names = F)),sd(unlist(msr[which(grid$boot.size == 1320),], use.names = F)),sd(unlist(msr[which(grid$boot.size == 2000),], use.names = F))))
bestPerfTreeSize <- data.frame(TreeSize = c("0","7.1x10-6 ","0.01"), AverageMSE = c(mean(unlist(msr[which(grid$cp == grid$cp[1]),], use.names = F)),mean(unlist(msr[which(grid$cp == grid$cp[10]),], use.names = F)),mean(unlist(msr[which(grid$cp == grid$cp[81]),], use.names = F))), VarMSE = c(sd(unlist(msr[which(grid$cp == grid$cp[1]),], use.names = F)),sd(unlist(msr[which(grid$cp == grid$cp[10]),], use.names = F)),sd(unlist(msr[which(grid$cp == grid$cp[81]),], use.names = F))))

ggplot(bestPerfVar, aes(x = NumberOfVariables, y = AverageMSE)) + geom_point(col = "Dodgerblue") +
  geom_errorbar(aes(ymax = AverageMSE + 1.96 * VarMSE, ymin = AverageMSE - 1.96 * VarMSE), col = "red") + labs(x="Mtry", y="MSE") + ggtitle("Average Performance of Random Forest Models with Different Mtry") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
ggplot(bestPerfTreeNo, aes(x = NumberOfTrees, y = AverageMSE)) + geom_point(col = "Dodgerblue") +
  geom_errorbar(aes(ymax = AverageMSE + 1.96 * VarMSE, ymin = AverageMSE - 1.96 * VarMSE), col = "red") + labs(x="Number of Trees Used", y="MSE") + ggtitle("Average Performance of Random Forest Models for Different Number of Trees") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
ggplot(bestPerfSampSize, aes(x = SampleSize, y = AverageMSE)) + geom_point(col = "Dodgerblue") +
  geom_errorbar(aes(ymax = AverageMSE + 1.96 * VarMSE, ymin = AverageMSE - 1.96 * VarMSE), col = "red") + labs(x="Bootstrap Sample (%)", y="MSE") + ggtitle("Average Performance of Random Forest Models for Different Bootstrap Samples") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
ggplot(bestPerfTreeSize, aes(x = TreeSize, y = AverageMSE)) + geom_point(col = "Dodgerblue") +
  geom_errorbar(aes(ymax = AverageMSE + 1.96 * VarMSE, ymin = AverageMSE - 1.96 * VarMSE), col = "red") + labs(x="Size of Trees (CP)", y="MSE") + ggtitle("Average Performance of Random Forest Models with Tree Sizes") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))

xAxis <- c("Number of Parameters","Number of Trees","Sample Size","Size of Trees (cp)")
condition <- rep(c("Small" , "Medium" , "Large") , 4)
value <- c(0,5,9,2,0,3,0,5,10,2,1,3)
data <- data.frame(xAxis,condition,value)
labelMaker <- c("", "","","200","500","1000","","66%","100%","0","7.1x10-6","0.01")
ggplot(data, aes(fill=condition, y=value, x=xAxis)) + geom_bar(position="stack", stat="identity")+ theme(legend.position = "none")+ labs(x="Hyperparameter", y="Number of Occurences") + ggtitle("Frequency of Hyperparameters Occuring in Best Performing Tree") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + geom_text(label=labelMaker, size = 5, position = position_stack(vjust = 0.5))+ annotate("text", x=1, y=5, label= "6", size = 5)

#Ideal model:
start_time <- Sys.time()
fitIdeal<-randomForest(price~.,data=diamondTrain, mtry = grid$mtry[69], ntree=grid$ntree[69], importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = grid$cp[69]), sampsize = nrow(diamondTrain))
end_time <- Sys.time()
timeExtraRF <- end_time - start_time

fitIdeal<-randomForest(price~.,data=diamondTrain, mtry = grid$mtry[69], ntree=grid$ntree[69], importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = grid$cp[69]), sampsize = nrow(diamondTrain))
fitWorst<-randomForest(price~.,data=diamondTrain, mtry = grid$mtry[10], ntree=grid$ntree[10], importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = grid$cp[10]), sampsize = 27684)
fitTerrible <- randomForest(price~.,data=diamondTrain, mtry = grid$mtry[10], ntree=1, importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = grid$cp[10]), sampsize = 27684)

#Evaluate performance

#Predictive power of the model
#Training data
predRFTrain=predict(fitIdeal,diamondTrain)
RFValuesTrain <- data.frame(diamondTrain$price)
RFValuesTrain$observed <- RFValuesTrain$diamondTrain.price
RFValuesTrain$predicted <- predRFTrain
ggplot(data = RFValuesTrain, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Random Forest (Training Data)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(RFValuesTrain$predicted, RFValuesTrain$observed)
rmse(RFValuesTrain$predicted, RFValuesTrain$observed)

#Test data
predRF=predict(fitIdeal,diamondTest)
RFValues <- data.frame(diamondTest$price)
RFValues$observed <- RFValues$diamondTest.price
RFValues$predicted <- predRF
ggplot(data = RFValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Random Forest (Test Data)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(RFValues$predicted, RFValues$observed)
rmse(RFValues$predicted, RFValues$observed)

#Worst Fit
predRFW=predict(fitWorst,diamondTest)
RFWValues <- data.frame(diamondTest$price)
RFWValues$observed <- RFWValues$diamondTest.price
RFWValues$predicted <- predRFW
ggplot(data = RFWValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Random Forest (Worst Model)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(RFWValues$predicted, RFWValues$observed)
rmse(RFWValues$predicted, RFWValues$observed)

#One tree
predRFW=predict(fitTerrible,diamondTest)
RFTValues <- data.frame(diamondTest$price)
RFTValues$observed <- RFTValues$diamondTest.price
RFTValues$predicted <- predRFW
ggplot(data = RFWValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Random Forest (Worst Model)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(RFWValues$predicted, RFWValues$observed)
rmse(RFWValues$predicted, RFWValues$observed)

#Compare Models
#Random Forest vs Regression Tree
RFAgainstRT <- data.frame(diamondTest$price)
RFAgainstRT$RandomForest <- predRF
RFAgainstRT$RegressionTree <- treePred
ggplot(data = RFAgainstRT, mapping = aes(x = RegressionTree, y = RandomForest, color = diamondTest.price)) +  geom_point(stat="identity") + labs(y="Random Forest Predictions", x="Regression Tree Predictions") + ggtitle("Random Forest Predictions vs Regression Tree Predictions")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(color = "Observed Prices")
cor(RFAgainstRT$RandomForest, RFAgainstRT$RegressionTree)
rmse(RFAgainstRT$RandomForest, RFAgainstRT$RegressionTree)

#Random Forest vs Linear Regression
RFAgainstLR <- data.frame(diamondTest$price)
RFAgainstLR$RandomForest <- predRF
RFAgainstLR$LinearRegression <- lregPred
ggplot(data = RFAgainstLR, mapping = aes(x = LinearRegression, y = RandomForest, color = diamondTest.price)) +  geom_point(stat="identity") + labs(y="Random Forest Predictions", x="Linear Regression Predictions") + ggtitle("Random Forest Predictions vs Linear Regression Predictions")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(color = "Observed Prices")
cor(RFAgainstLR$RandomForest, RFAgainstLR$LinearRegression)
rmse(RFAgainstLR$RandomForest, RFAgainstLR$LinearRegression)

#Random Forest vs Worst Tree
RFAgainstWT <- data.frame(diamondTest$price)
RFAgainstWT$RandomForest <- predRF
RFAgainstWT$WorstTree <- predRFW
ggplot(data = RFAgainstWT, mapping = aes(x = WorstTree, y = RandomForest, color = diamondTest.price)) +  geom_point(stat="identity") + labs(y="Random Forest Predictions", x="Random Forest (Worst Model) Predictions") + ggtitle("Random Forest Optimal Parameters vs Worst Parameters Predictions")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(color = "Observed Prices")
cor(RFAgainstWT$RandomForest, RFAgainstWT$WorstTree)
rmse(RFAgainstWT$RandomForest, RFAgainstWT$WorstTree)

#Variable Importance to prediction
importance(fitIdeal)
varImpPlot(fitIdeal)

#Error rate of trees
errorByTrees <- data.frame(Error = fitIdeal$mse, numberOfTrees = seq(1, 500, by=1))
ggplot(data = errorByTrees, mapping = aes(x = numberOfTrees, y = Error)) +  geom_line(col = "Dodgerblue") + labs(x="Number of Trees", y="Error") + ggtitle("Error rate over trees") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))

#Model Interpretation
diamondData <- diamond[,-1]
X <- diamondData[which(names(diamondData) != "price")]
predictor = Predictor$new(fitIdeal, data = X, y = diamondData$price)

#Variable Importance
#Measured by contribution to prediction (Measure reduction in prediction accuracy using MSE (or MAE))
imp2 = FeatureImp$new(predictor, loss = "mae", n.repetitions = 10)
plot(imp2)
imp.dat = imp2$results
ggplot(imp.dat, aes(x = feature, y = importance)) + geom_point(col = "Dodgerblue")  + scale_x_discrete(limits=c("size","clarity","color","depth", "cut", "table")) +
  geom_errorbar(aes(ymax = importance.95, ymin = importance.05), col = "red") + labs(x="Variable", y="Fall in Accuracy (MAE)") + ggtitle("Variable Importance (Random Forest)") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))

#Friedman H statistic
interact = Interaction$new(predictor)
interactGraph <- ggplot(data = interact$results, mapping = aes(x = .interaction, y = .feature)) +  geom_point(stat="identity", col = "skyblue2", size = 3) + geom_segment(aes(x = 0, y = 6, xend = 0.25, yend = 6), col = "red") + labs(y="Variable", x="Overall Interaction Strength") + ggtitle("Interaction Strength (Random Forest)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ scale_y_discrete(limits=c("table","cut","depth","clarity", "color", "size"))
interactGraph <- interactGraph + geom_segment(aes(x = 0, y = 5, xend = 0.2316, yend = 5), col = "red")
interactGraph <- interactGraph + geom_segment(aes(x = 0, y = 4, xend = 0.155, yend = 4), col = "red")
interactGraph <- interactGraph + geom_segment(aes(x = 0, y = 3, xend = 0.036, yend = 3), col = "red")
interactGraph <- interactGraph + geom_segment(aes(x = 0, y = 2, xend = 0.032, yend = 2), col = "red")
interactGraph <- interactGraph + geom_segment(aes(x = 0, y = 1, xend = 0.015, yend = 1), col = "red")
interactGraph
#Consider specific variables
#Size
interactSize = Interaction$new(predictor, feature = "size")
interactGraphSize <- ggplot(data = interactSize$results, mapping = aes(x = .interaction, y = .feature)) +  geom_point(stat="identity", col = "skyblue2", size = 3) + geom_segment(aes(x = 0, y = 5, xend = 0.238, yend = 5), col = "red") + labs(y="Interaction term", x="Interaction Strength") + ggtitle("Strength of Interactions (Size)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ scale_y_discrete(limits=c("table:size","depth:size","cut:size","clarity:size", "color:size"))
interactGraphSize <- interactGraphSize + geom_segment(aes(x = 0, y = 4, xend = 0.18, yend = 4), col = "red")
interactGraphSize <- interactGraphSize + geom_segment(aes(x = 0, y = 3, xend = 0.021, yend = 3), col = "red")
interactGraphSize <- interactGraphSize + geom_segment(aes(x = 0, y = 2, xend = 0.011, yend = 2), col = "red")
interactGraphSize <- interactGraphSize + geom_segment(aes(x = 0, y = 1, xend = 0.007, yend = 1), col = "red")
interactGraphSize
#Colour
interactColor = Interaction$new(predictor, feature = "color")
interactGraphColor <- ggplot(data = interactColor$results, mapping = aes(x = .interaction, y = .feature)) +  geom_point(stat="identity", col = "skyblue2", size = 3) + geom_segment(aes(x = 0, y = 5, xend = 0.268, yend = 5), col = "red") + labs(y="Interaction term", x="Interaction Strength") + ggtitle("Strength of Interactions (Colour)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ scale_y_discrete(limits=c("table:color","depth:color","cut:color","size:color", "clarity:color"))
interactGraphColor <- interactGraphColor + geom_segment(aes(x = 0, y = 4, xend = 0.177, yend = 4), col = "red")
interactGraphColor <- interactGraphColor + geom_segment(aes(x = 0, y = 3, xend = 0.0268, yend = 3), col = "red")
interactGraphColor <- interactGraphColor + geom_segment(aes(x = 0, y = 2, xend = 0.0263, yend = 2), col = "red")
interactGraphColor <- interactGraphColor + geom_segment(aes(x = 0, y = 1, xend = 0.014, yend = 1), col = "red")
interactGraphColor
#Clarity
interactClarity = Interaction$new(predictor, feature = "clarity")
interactGraphClarity <- ggplot(data = interactClarity$results, mapping = aes(x = .interaction, y = .feature)) +  geom_point(stat="identity", col = "skyblue2", size = 3) + geom_segment(aes(x = 0, y = 5, xend = 0.245, yend = 5), col = "red") + labs(y="Interaction term", x="Interaction Strength") + ggtitle("Strength of Interactions (Clarity)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ scale_y_discrete(limits=c("table:clarity","depth:clarity","cut:clarity","size:clarity", "color:clarity"))
interactGraphClarity <- interactGraphClarity + geom_segment(aes(x = 0, y = 4, xend = 0.1505, yend = 4), col = "red")
interactGraphClarity <- interactGraphClarity + geom_segment(aes(x = 0, y = 3, xend = 0.0205, yend = 3), col = "red")
interactGraphClarity <- interactGraphClarity + geom_segment(aes(x = 0, y = 2, xend = 0.015, yend = 2), col = "red")
interactGraphClarity <- interactGraphClarity + geom_segment(aes(x = 0, y = 1, xend = 0.01, yend = 1), col = "red")
interactGraphClarity

#Partial Dependence plot
pdpSize = FeatureEffect$new(predictor, feature = "size", method = "pdp")
#Compare to ale plot to see if theres a major difference between their methods
aleSize = FeatureEffect$new(predictor, feature = "size", method = "ale")
pdpColour = FeatureEffect$new(predictor, feature = "color", method = "pdp")
pdpClarity = FeatureEffect$new(predictor, feature = "clarity", method = "pdp")
pdpCut = FeatureEffect$new(predictor, feature = "cut", method = "pdp")
pdpDepth = FeatureEffect$new(predictor, feature = "depth", method = "pdp")
pdpTable = FeatureEffect$new(predictor, feature = "table", method = "pdp")
pdpColourSize = FeatureEffects$new(predictor, features = c("color","size"), method = "pdp")
pdpColourClarity = FeatureEffects$new(predictor, features = c("color","clarity"), method = "pdp")
pdpClaritySize = FeatureEffects$new(predictor, features = c("clarity","size"), method = "pdp")
#Plots
aleSize$plot() + ggtitle("Importance of Size")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
pdpSize$plot() + ggtitle("Importance of Size")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
pdpColour$plot() + ggtitle("Importance of Colour")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_col(col = "skyblue2",fill = "skyblue2")
pdpClarity$plot() + ggtitle("Importance of Clarity")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))+ geom_col(col = "skyblue2",fill = "skyblue2")
pdpCut$plot() + ggtitle("Importance of Cut")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_col(col = "skyblue2",fill = "skyblue2")+ scale_x_discrete(limits=c("Fair","Good","Very Good","Premium", "Ideal"))
pdpDepth$plot() + ggtitle("Importance of Depth")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
pdpTable$plot() + ggtitle("Importance of Table")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
pdpColourSize$plot() + ggtitle("Importance of Colour-Size Interaction")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
pdpColourClarity$plot() + ggtitle("Importance of Colour-Clarity Interaction")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
pdpClaritySize$plot() + ggtitle("Importance of Size-Clarity Interaction")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
#Plot the interactions
diamondTrain2 <- diamondTrain
#Convert color and clarity to continuous
diamondTrain2$color <- sapply(diamondTrain2$color, as.numeric)
diamondTrain2$clarity<- factor(diamondTrain2$clarity, levels = c("IF","VVS1","VVS2","VS1", "VS2", "SI1", "SI2", "I1"))
diamondTrain2$clarity <- sapply(diamondTrain2$clarity, as.numeric)
fitIdeal2 <- randomForest(price~.,data=diamondTrain2, mtry = grid$mtry[69], ntree=grid$ntree[69], importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = grid$cp[69]), sampsize = nrow(diamondTrain2))
colourSize <- partial(fitIdeal2, pred.var = c("color","size"), plot = TRUE, plot.engine = "ggplot2", chull = TRUE, rug = TRUE)
colourSize <- colourSize + ggtitle("Importance of Colour-Size Interaction")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + labs(x="Colour", y="Size", fill = "Price")
colourClarity <- partial(fitIdeal2, pred.var = c("color","clarity"), plot = TRUE, plot.engine = "ggplot2", chull = TRUE, rug = TRUE)
colourClarity <- colourClarity + labs(x="Colour", y="Clarity", fill = "Price")+ ggtitle("Importance of Colour-Clarity Interaction")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))
claritySize <- partial(fitIdeal2, pred.var = c("clarity","size"), plot = TRUE, plot.engine = "ggplot2", chull = TRUE, rug = TRUE)+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(x="Clarity", y="Size", fill = "Price")
claritySize <- claritySize + labs(x="Clarity", y="Size", fill = "Price")+ ggtitle("Importance of Size-Clarity Interaction")
tableSize <- partial(fitIdeal2, pred.var = c("table","size"), plot = TRUE, plot.engine = "ggplot2", chull = TRUE, rug = TRUE)+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(x="Table %", y="Size", fill = "Price")

#Shapley Values: Could use this to explain the reasoning of a model for predicting new prices (very useful for new predictions)
shapley <- Shapley$new(predictor, x.interest = X[6278,])
shapleyLabel1 <- paste("Actual Prediction =", round(shapley$y.hat.interest,2))
shapleyLabel2 <- paste("Average Prediction =", round(shapley$y.hat.average,2))
shapley$results$feature.value[6] <- "size=145.97"
ggplot(shapley$results, aes(x = feature.value, y = phi))+ geom_col(col = "skyblue2",fill = "skyblue2", width = 0.7) + coord_flip()+ labs(y="Feature Variable Contribution", x="Feature Value") + ggtitle("Shapley Graph for a Random Diamond")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + scale_x_discrete(limits=c("clarity=SI1","cut=Very Good","table=58","depth=61.6","color=F", "size=145.97")) + geom_text(label=shapleyLabel1, x=2.3, y=147, size = 6, col = "dodgerblue")  + geom_text(label=shapleyLabel2, x=1.7, y=152, size = 6, color = "dodgerblue")

#Random Forest Explainer

#variable Importance Measures - compare to contribution to fit
importance_frame <- measure_importance(fitIdeal)
plot_multi_way_importance(importance_frame) + ggtitle("Variable Importance (Mean Minimum Depth)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(y="Number of Times Variable was a Root", x="Mean Minimum Depth")
meanMinDepth <- ggplot(data = importance_frame, mapping = aes(x = variable, y = mean_min_depth)) +  geom_point(stat="identity", col = "skyblue2", size = 3) + geom_segment(aes(x = 2, y = 0, xend = 2, yend = 2), col = "red") + labs(y="Mean Minimum Depth", x="Variable") + ggtitle("Variable Importance (Mean Minimum Depth)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ scale_x_discrete(limits=c("size","clarity","color","cut","depth", "table"))
meanMinDepth <- meanMinDepth + geom_segment(aes(x = 3, y = 0, xend = 3, yend = 3.182), col = "red")
meanMinDepth <- meanMinDepth + geom_segment(aes(x = 4, y = 0, xend = 4, yend = 5.298), col = "red")
meanMinDepth <- meanMinDepth + geom_segment(aes(x = 5, y = 0, xend = 5, yend = 5.898), col = "red")
meanMinDepth <- meanMinDepth + geom_segment(aes(x = 6, y = 0, xend = 6, yend = 5.958), col = "red")
meanMinDepth

#Extra Tree
#Determine complexity parameter
gridExtra <- expand.grid(ntree = c(200, 500, 1000), cp = c(0.000, cp.best, 0.01))
samp <- sample(nrow(diamondTrain), 2000)
subDiamondTrain <- diamondTrain[samp, ]
subDiamondTrainTest <-diamondTrain[-samp, ]
rmseExtra <- matrix(0L, nrow = 9, ncol = 10)
colnames(rmseExtra) <- c("RMSE1","RMSE2","RMSE3","RMSE4","RMSE5","RMSE6","RMSE7","RMSE8","RMSE9", "RMSE10")
bestExtra <- data.frame( bestRMSE = numeric(10))
for(j in 1:10)
{
  for (i in 1:9)
  {
    fitExtraTemp <- randomForest(price~.,data=diamondTrain, mtry = 1, ntree=gridExtra$ntree[i], importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = gridExtra$cp[i]), sampsize = 2000, replace = FALSE)
    predET=predict(fitExtraTemp,subDiamondTrainTest)
    ETValues <- data.frame(subDiamondTrainTest$price)
    ETValues$observed <- ETValues$subDiamondTrainTest.price
    ETValues$predicted <- predET
    rmseExtra[i,j] <- rmse(ETValues$predicted, ETValues$observed)
  }
  #Determine best hyperparameters
  bestExtra$bestRMSE[j] <- which(rmseExtra[,j] == min(rmseExtra[,j]))
}
write.csv(rmseExtra, "C:/Users/pie93/Desktop/Data Analytics/Assignment/rmseDataExtra.csv")
write.table(bestExtra, "C:/Users/pie93/Desktop/Data Analytics/Assignment/bestDataExtra.txt", sep="\t")

xAxis <- c("Number of Trees","Size of Trees (cp)")
condition <- rep(c("Small" , "Medium" , "Large") , 2)
value <- c(5,2,2,4,3,4)
data <- data.frame(xAxis,condition,value)
labelMaker <- c("200","500","1000","0","7.1x10-6","0.01")
ggplot(data, aes(fill=condition, y=value, x=xAxis)) + geom_bar(position="stack", stat="identity")+ theme(legend.position = "none")+ labs(x="Hyperparameter", y="Number of Occurences") + ggtitle("Frequency of Hyperparameters Occuring in Best Performing Model") + theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5)) + geom_text(label=labelMaker, size = 5, position = position_stack(vjust = 0.5))

#Best Extra Trees
start_time <- Sys.time()
fitExtra <- randomForest(price~.,data=diamondTrain, mtry = 1, ntree=200, importance=TRUE, na.action=na.omit, control= rpart.control(split="Gini", cp = 0), sampsize = nrow(diamondTrain), replace = FALSE)
end_time <- Sys.time()
timeExtra <- end_time - start_time

#Extra Trees Performance
predET=predict(fitExtra,diamondTest)
ETValues <- data.frame(diamondTest$price)
ETValues$observed <- ETValues$diamondTest.price
ETValues$predicted <- predET
ggplot(data = ETValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Extra Trees (Test Data)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(ETValues$predicted, ETValues$observed)
rmse(ETValues$predicted, ETValues$observed)

#Random Forest vs Extra Trees
RFAgainstET <- data.frame(diamondTest$price)
RFAgainstET$RandomForest <- predRF
RFAgainstET$ExtraTree <- predET
ggplot(data = RFAgainstET, mapping = aes(x = ExtraTree, y = RandomForest, color = diamondTest.price)) +  geom_point(stat="identity") + labs(y="Random Forest Predictions", x="Extra Trees Predictions") + ggtitle("Random Forest vs Extra Trees Predictions")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ labs(color = "Observed Prices")
cor(RFAgainstET$RandomForest, RFAgainstET$ExtraTree)
rmse(RFAgainstET$RandomForest, RFAgainstET$ExtraTree)

#Extra Trees - Training Data
predET=predict(fitExtra,diamondTrain)
ETValues <- data.frame(diamondTrain$price)
ETValues$observed <- ETValues$diamondTrain.price
ETValues$predicted <- predET
ggplot(data = ETValues, mapping = aes(x = predicted, y = observed)) +  geom_point(stat="identity", col = "skyblue2") + labs(y="Observed Prices", x="Predicted prices") + ggtitle("Diamond Prices with Extra Trees (Training Data)")+ theme(plot.title = element_text(color = "Dodgerblue", size = 15, face = "bold", hjust = 0.5))+ geom_smooth(method='lm', color = "red")
cor(ETValues$predicted, ETValues$observed)
rmse(ETValues$predicted, ETValues$observed)