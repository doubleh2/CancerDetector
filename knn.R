# a cancer machine learning project adapted from https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
# by hunter hansen (myjkh2@outlook.com); doubleh2@github
library(data.table)
library(class) # for knn
library(gmodels) #for CrossTable()

## updates original demonstration utilizing data.tables and random selection of cases, with a repeated samples 
## design. This allows us to view the frequency and distribution of folse negatives (e.g. malignant tumors 
## labled benign)  

# prc <- read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE)    #This command imports the required data set and saves it to the prc data frame.
prc <- fread("Prostate_Cancer.csv",stringsAsFactors = FALSE)    #This command imports the required data set and saves it to the prc data frame.
   #This command helps to convert every character vector to a factor wherever it makes sense.

str(prc)    #We use this command to see whether the data is structured or not.
# prc <- prc[-1]  #removes the first variable(id) from the data set.

prc <- prc[, id:=NULL]  # removes the first variable(id) from the data set.
table(prc$diagnosis_result)  # it helps us to get the numbers of patients
prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(prc$diagnosis)) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)

# normalized function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

prc_n <- as.data.frame(lapply(prc[,2:9], normalize))

str(prc_n)

summary(prc_n$radius)

all_negs <- list() # set results list

# loop for repeated sammpling and measurement 
for(i in 1:100){
  # divide the prc_n data frame into prc_train and prc_test data frames
  my_rows <- sample(nrow(prc_n),65) # randomly select subjects
  
  # set training, test and lables vars
  prc_train <- prc_n[my_rows, ] 
  prc_test <- prc_n[!(1:nrow(prc) %in% my_rows), ]
  
  prc_train_labels <-prc[my_rows, 1]
  prc_test_labels <- prc[!(1:nrow(prc) %in% my_rows), 1]  
  
  # run knn
  prc_test_pred <- knn(train = prc_train, test = prc_test,cl = unlist(prc_train_labels), k=10)
  
  # table rsults, pull false negatives, add to results list 
  results <- CrossTable(x = unlist(prc_test_labels), y = prc_test_pred, prop.chisq = FALSE)
  false_neg <- results$prop.tbl[2]
  all_negs <- c(all_negs, false_neg)
}

# plots
barplot(unlist((all_negs)), ylim=c(0,1))
boxplot(unlist(all_negs))

