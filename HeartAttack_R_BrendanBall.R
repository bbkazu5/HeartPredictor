# Heart Attack Indicators R Script
# Code by Brendan Ball

# 1. INTRODUCTION

# Install necessary packages for the code
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(rgl)) install.packages("rgl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", 
                                       repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", 
                                          repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", 
                                       repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readr", 
                                      repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", 
                                        repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", 
                                    repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", 
                                     repos = "http://cran.us.r-project.org")

# Downlaod the latest version of mixOmics if needed
# You may be prompted to "update all/some/none?", then type [a/s/n]


# No warning messages, open the installed packages if needed
suppressWarnings(library (MASS, verbose = FALSE))
suppressWarnings(library (rgl, verbose = FALSE))
suppressWarnings(library (ggplot2, verbose = FALSE))
suppressWarnings(library (lattice, verbose = FALSE))
suppressWarnings(library (factoextra, verbose = FALSE))
suppressWarnings(library (dplyr, verbose = FALSE))
suppressWarnings(library (stringr, verbose = FALSE))
suppressWarnings(library (readr, verbose = FALSE))
suppressWarnings(library (corrplot, verbose = FALSE))
suppressWarnings(library (caret, verbose = FALSE))
suppressWarnings(library (klaR, verbose = FALSE))
suppressWarnings(library (e1071, verbose = FALSE))
suppressWarnings(library (mixOmics, verbose = FALSE))

# Download the URL file from the bbkazu5 github account
# https://github.com/bbkazu5/HeartPredictor

# Copy URL and read the CSV file
urlfile = "https://github.com/bbkazu5/HeartPredictor/raw/main/HeartData.csv"
heartdata <-read_csv(url(urlfile))

# Remove the additional column that was generated 
suppressWarnings(heartdata <- subset(heartdata, select = -c(X16) ))

# Remove the url file from the environment
suppressWarnings(rm(urlfile))

# 2. METHODS & ANALYSIS
## 2.1. BASICS OF THE DATA

# Determine the class of the heart data
class(heartdata)

# Glimpse the data set of "heartdata"
glimpse(heartdata)

# Inspect the result summaries of the results
summary(heartdata)

## 2.2. EXPLORATION OF HEART ATTACK DATA

# Prepare a data frame (heartdata_new) that contains string characters instead
# Mutate the data to make sex be a factor of female or male
heartdata_new <- heartdata %>% 
  mutate(sex = factor(sex,
                      levels = c(0, 1),
                      labels = c("female", "male"))) 

# Mutate to make exng (exercise induced angina) be a factor of yes or no
heartdata_new <- heartdata_new %>% 
  mutate(exng = factor(exng,
                       levels = c(0, 1),
                       labels = c("no", "yes"))) 

# Mutate to make cp (chest pain type) be a factor of name
heartdata_new <- heartdata_new %>% 
  mutate(cp = factor(cp,
                     levels = c(0, 1, 2, 3),
                     labels = c("typical angina", "atypical angina", 
                                "non-anginal pain", "asymptomatic"))) 

# Mutate to make output (heart attack risk) be a factor of yes or no
heartdata_new <- heartdata_new %>% 
  mutate(output = factor(output,
                         levels = c(1, 0),
                         labels = c("higher chance", "lower chance"))) 

# Create a visualization between males and female subjects
ggplot(data=heartdata_new,aes(x=sex,fill=sex))+geom_bar(col="black") + 
  xlab("Sex") + ylab("Count") + ggtitle("Count Between Males and Females") +
  labs(caption = "Source: [2]")

# Create a visualization between males and female subjects and age
ggplot(data=heartdata_new,aes(x=age,fill=sex))+geom_histogram(binwidth=1, 
                                                              col="black") +
  xlab("Age") + ylab("Count") + ggtitle("Age Groups Between Males and Females") + 
  labs(caption = "Source: [2]")

# Create a density plot between both sex groups to understand age distribution
ggplot(data=heartdata_new,aes(x=age,fill=sex))+geom_density(alpha = 0.5) + 
  xlab("Age") + ylab("Count") + 
  ggtitle("Age Group Density Plot Between Males and Females") + 
  labs(caption = "Source: [2]")

# Create a density plot between both sex groups to understand age distribution
ggplot(data=heartdata_new,aes(x=chol,fill=output))+geom_density(alpha = 0.5) + 
  xlab("Age") + ylab("Count") + 
  ggtitle("Chance of Heart Attack with Cholesterol Levels") + 
  labs(caption = "Source: [2]")

# Create a plot showing the different groups of chest pain types among sex
ggplot(data=heartdata_new,aes(x=cp,fill=sex))+geom_bar(col="black")+
  theme(axis.text=element_text(size = 8)) + xlab("Chest Pain Type") + 
  ylab("Count") + ggtitle("Chest Paint Type Between Males and Females") + 
  labs(caption = "Source: [2]")

# Plot the chance for a heart attack between the two sex groups
ggplot(data=heartdata_new,aes(x=output,fill=sex))+geom_bar(col="black") + 
  xlab("Chance for Heart Attack") + ylab("Count") + 
  ggtitle("Distribution Between Male and Female Heart Attack Chance") + 
  labs(caption = "Source: [2]")

# Plot the exercise induced angina (EIA) and sex relationship
ggplot(data=heartdata_new,aes(x=exng,fill=sex))+geom_bar(col="black") + 
  xlab("Exercise Induced Angina") + ylab("Count") + 
  ggtitle("Exercise Induced Angina Between Males and Females") + 
  labs(caption = "Source: [2]")

## 2.3. PRINCIPAL COMPONENT ANALYSIS (PCA) MODELING

# Standardize the data
heartdata_pr <- prcomp(heartdata[c(2:15)], center = TRUE, scale = TRUE)

# Prepare the Scree Plot for Heart Data
screeplot(heartdata_pr, type = "l", npcs = 15, 
          main = "Scree Plot of the First 15 PCs")
abline(h = 1, col="red", lty=20)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

# Create a summary of the results in a table format
summary(heartdata_pr)

# Cumulative Variance Plot for 80% Variance
cumVar <- cumsum(heartdata_pr$sdev^2 / sum(heartdata_pr$sdev^2))
plot(cumVar[0:15], xlab = "Principle Component #", 
     ylab = "Amount of Explained Variance", 
     main = "Cumulative Variance Plot for Heart Data")
abline(v = 9, col="blue", lty=5)
abline(h = 0.80, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC #9"),
       col=c("blue"), lty=5, cex=0.6)

# Using the which() command to determine the PC# greater than or equal to 80%
print(c("Princial components at 80% variance is:", which(cumVar >= 0.80)[1]))

# Plotting the PCA of heart data to observe the explained variance, ncomp = 9
heartdata_PCA <- pca(heartdata, center = TRUE, scale = TRUE, ncomp = 9)
plot(heartdata_PCA, main = "Amount of Variance Explained for Each PC in Heart")

# Mutate the data to make chance be a factor, so it can color code the PCA figure
heartdata_output <- heartdata %>% 
  mutate(output = factor(output,
                         levels = c(1, 0),
                         labels = c("higher chance", "lower chance"))) 

# Plot the PCA with color coding of chance of heart attack
fviz_pca_ind(heartdata_pr, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = heartdata_output$output,
             col.ind = "black",
             pallette = "jco",
             addEllipses = TRUE,
             label = "var", 
             col.var = "black",
             repel = TRUE,
             legend.title = "Heart Attack Risk") + 
  ggtitle("Heart Data and Risk for Heart Attack Correlation") + 
  theme(plot.title = element_text(hjust = 0.5))

# Mutate the data to make sex be a factor, so it can color code the PCA figure
heartdata_sex <- heartdata %>% 
  mutate(sex = factor(sex,
                      levels = c(0, 1),
                      labels = c("female", "male"))) 

# Plot the PCA with color coding of Sex
fviz_pca_ind(heartdata_pr, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = heartdata_sex$sex,
             col.ind = "black",
             pallette = "jco",
             addEllipses = TRUE,
             label = "var", 
             col.var = "black",
             repel = TRUE,
             legend.title = "Sex") + 
  ggtitle("Heart Data and Sex Correlation") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot the PCA with color coding of Age
fviz_pca_ind(heartdata_pr, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = cut(heartdata$age, breaks = c(20, 40, 60, 80)),
             col.ind = "black",
             pallette = "jco",
             addEllipses = TRUE,
             label = "var", 
             col.var = "black",
             repel = TRUE,
             legend.title = "Age") + 
  ggtitle("Heart Data and Age Correlation") + 
  theme(plot.title = element_text(hjust = 0.5))

# Mutate the data to make exng (exercise induced angina) be a factor 
# This way, it can color code the PCA figure
heartdata_exng <- heartdata %>% 
  mutate(exng = factor(exng,
                       levels = c(0, 1),
                       labels = c("no", "yes"))) 

# Plot the PCA with color coding of Exercised Induced Angina (EIA)
fviz_pca_ind(heartdata_pr, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = heartdata_exng$exng,
             col.ind = "black",
             pallette = "jco",
             addEllipses = TRUE,
             label = "var", 
             col.var = "black",
             repel = TRUE,
             legend.title = "EIA") + 
  ggtitle("Heart Data and EIA Correlation") + 
  theme(plot.title = element_text(hjust = 0.5))

# Total cholesterol ranges based on adults [3]
# Desirable Range: under 200mg/dl
# Borderline Range: 200-239mg/dl
# High Range: above 240mg/dl

# Plot the PCA with color coding of cholesterol
fviz_pca_ind(heartdata_pr, geom.ind = "point", pointshape = 21,
             pointsize = 2,
             fill.ind = cut(heartdata$chol, breaks = c(0, 200, 239, 565)),
             col.ind = "black",
             pallette = "jco",
             addEllipses = TRUE,
             label = "var", 
             col.var = "black",
             repel = TRUE,
             legend.title = "Cholesterol (mg/dl)") + 
  ggtitle("Heart Data and Cholesterol Correlation") + 
  theme(plot.title = element_text(hjust = 0.5))

## 2.5. Linear Discriminant Analysis (LDA)

# Mutate to make output (heart attack risk) be a factor of yes or no
heartdata_prep_lda <- heartdata %>% 
  mutate(output = factor(output,
                         levels = c(1, 0),
                         labels = c("higher chance", "lower chance"))) 

# LDA analysis
heart_lda <- lda(output~., data = heartdata_prep_lda, center = TRUE, 
                 scale = TRUE)
# Print the results out
heart_lda$prior

## 3. NAIVE BAYES THEOREM

# Rename columns to be better associated in following figures
heart1 <- heartdata
colnames(heart1)[4] <- "chest.pain.type"
colnames(heart1)[5] <- "resting.BP"
colnames(heart1)[6] <- "cholesterol.level"
colnames(heart1)[7] <- "fasting.blood.sugar"
colnames(heart1)[8] <- "rest.ecg"
colnames(heart1)[9] <- "max.heart.rate"
colnames(heart1)[10] <- "exc.induced.angina"
colnames(heart1)[11] <- "exc.rel.rest"
colnames(heart1)[12] <- "slope.peak.ST"
colnames(heart1)[13] <- "#.major.vessels"
colnames(heart1)[14] <- "thallium.stress"
colnames(heart1)[15] <- "heart.atk.risk"

# Create a correlation matrix
# Rectangles around the plot is based on hierarchical clustering.
corrMatrix <- cor(heart1[, 2:15])
corrplot(corrMatrix, order = "hclust", tl.cex = 1, addrect = 8)

# Create a correlation matrix plot
corrplot(corrMatrix, method = 'color', order = 'hclust') 

# Find attributes that is correlated highly
correlated <- findCorrelation(corrMatrix, cutoff = 0.9)

# Print any correlated number of variables
print(correlated)

## 3.2. CREATING THE NAIVE BAYES MODEL

# Maintain reproducibility
set.seed(1) 

# Shuffle the rows, but consistently every time it is ran
rows <- sample(nrow(heartdata))
heartdata_shuffle <- heartdata[rows, ]

# Mutate to make output (heart attack risk) be a factor of yes or no
heart_model <- heartdata_shuffle %>% 
  mutate(output = factor(output,
                         levels = c(1, 0),
                         labels = c("True", "False"))) 

heart_model <- heart_model[-1]

heart_model$output <- factor(heart_model$output, levels = c("True", "False"), 
                             labels = c("High.Risk", "Low.Risk"))

# Split data into training and test data sets
indxTrain <- createDataPartition(y = heart_model$output,p = 0.7,list = FALSE)
training <- heart_model[indxTrain,]
testing <- heart_model[-indxTrain,] 

# Check dimensions of the split 
round(prop.table(table(heart_model$output)) * 100, digits = 1)
round(prop.table(table(training$output)) * 100, digits = 1)
round(prop.table(table(testing$output)) * 100, digits = 1)

# Create objects x (predictor variables) and y (response variables)
x = training[,-14]
y = training$output

# Naive Bayes Model
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
suppressWarnings(model)

# Confusion matrix generation
Predict <- predict(model,newdata = testing )
confusionMatrix(Predict, testing$output )

## References: Please refer to the .RMD file for all references to health data





