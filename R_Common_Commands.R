#Installing packages
install.packages("e1071")
install.packages("dplyr")
install.packages("C50")
install.packages("ROCR")
install.packages("randomForest")
install.packages("monmlp")
install.packages("neuralnet")
install.packages("RWeka")
install.packages("NeuralNetTools")
install.packages("RSNNS")
install.packages("stringi")
install.packages("sjPlot")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("rattle")
install.packages("tcltk2")
install.packages("RSQLite")
install.packages("proto")
install.packages("gsubfn")
install.packages("sqldf")
install.packages("xslx")
install.packages("synthpop")
install.packages("tidyr")
install.packages("lubridate")
install.packages("data.table")
install.packages("ggplot2")
install.packages("readr")
install.packages("readxl")
install.packages("yaml")
install.packages("RPostgreSQL")
install.packages("DT")
install.packages("reshape2")
install.packages("shinydashboard")
install.packages("plotly")
install.packages("DBI")

library("e1071")
library("data.table")
library("dplyr")
library("C50")
library("ROCR")
library("randomForest")
library("monmlp")
library("neuralnet")
library("RWeka")
library("NeuralNetTools")
library("RSNNS")
library("stringi")
library("sjPlot")
library("corrplot")
library("ggcorrplot")
library("rattle")
library("tcltk2")
library("RSQLite")
library("proto")
library("gsubfn")
library("sqldf")
library("xslx")
library("synthpop")
library("magrittr")
library("lubridate")
library("data.table")
library("ggplot2")
library("tidyr")
library("readr")
library("readxl")
library("yaml")
library("RPostgreSQL")
library("DT")
library("reshape2")
library("shinydashboard")
library("plotly")
library("DBI")

library(all above packages)

# Change from scientific notation
options(scipen = 999)

#Remove columns 16 to 18 
alldata2 <- alldata[, -c(16:18)]

#Combine Dataframes allusersupdated,newuseracc
allusersupdated <- rbind(allusersupdated,newuseracc)

#Add column with fixed value eg. Prospect
AT$Status <- rep("Prospect",nrow(AT))

#Summarise all columns of a dataframe grouped by Species Can use Sum Max Min etc
df <- iris %>% group_by(Species) %>% summarise_all(sum)

#Find overlapping values in users_accounts which are not in newdata2 by User Id
unique <- anti_join(alldata2,today,by='User Id')
#Find overlapping values in users_accounts which are in newdata2
overlapped <- semi_join(users_accounts,newdata2,)

#Get non-duplicated rows from first table
newuseracc <- grid[!(grid$'User Id' %in% allusers$'User Id'),]

#Synthpop Anonymisation
#Create Synthesised data set
syn1 <- syn(iris)
#With definition of type of synth and cols to synth
syn1<-syn(df,method="norm", visit.sequence = c(1,2))
#Extract synthesised df
temp<-as.dataframe(syn1["syn"])
#Compare datasets with plots
compare.synds(syn1,iris)

#View head of dataset by column
glimpse(iris)

#Create Function which times/multiplies input (num_in) by 4 to output times_4
times_4 <- function(num_in) {
  times_4 <- num_in*4
  return(times_4)
}

#Create Vector a and b
a<-c(2,4,6)
b<-c(3,5,7)

# Create Data Frame
sg_level <- data.frame('year' = 1992:2019
                       ,'sg' = c(3,3,4,5,6,6,7,7,8,8,9,9,9,9,9,9,9,9,9,9,9,9.25,9.5,9.5,9.5,9.5,9.5,9.5)
                       ,'increment' = 28:1
                       ,stringsAsFactors = FALSE)

# data.table
# Filtering Grouping etc
DT <- as.data.table(iris)
filtered_DT <- DT[Species == "sentosa" | Sepal.Length < 5,] # Filter by conditions
filtered_grouped_DT <- DT[Species == "sentosa" | Sepal.Length < 5, 
                          .(mean_width = mean(Sepal.Width)), by = .(Species)] # Can add group by conditions after Species
filtered_grouped_DT <- DT[Species %in% c('setosa', 'versicolor'), 
                          .(sum_Sep_len = sum(Sepal.Length)), by = .(Species)]
merged <- merge(DT, filtered_grouped_DT, by = c("Species"), all.x = T) # Merged / Joined new column
merged[is.na(merged)] <- 0 # Added 0's in place of Nulls / Replace Nulls in data.table
merged[,new_column := 0] # Adds a new column filled with 0's
merged[,Sepal.Length == 0L, new_column := as.numeric(Sepal.Width)] # Checks if Sepal.Length is 0 and if not passes the value in Sepal.Width
merged[,sep_area := Sepal.Length*Sepal.Width] # Functions to create new column
grouped_DT <- DT[,.(
  tot_length = sum(Sepal.Length) # Sum overall
  , cnt_species = .N # Counts overall
  , cnt_uni_species = uniqueN(Sepal.Length) # Counts Unique values
  , tot_width = sum(Sepal.Width) 
), by = .(Species)][order(-tot_length, cnt_uni_species)] # Adds order and can include multiple cols

#Create a lower triagular matrix of the size of pred_mat
temp <- lower.tri(pred_mat)

#Convert Boolean to binary
temp <- ifelse(temp==TRUE,1,0)

# Loop for creating functions and returning providers and fnames
for ( i in 1:5 ){
  dummy <- worker(transactions,pgs[[i]],fnames[i])
}
worker <- function(transactions,providers,fname){
  functionxyz
}

#Create/Combine dataframe
df<-data.frame(a,b)

#Count of unique provider_ID's in the claim data frame
length(unique(claim[["provider_id"]]))

#Create mylist containing this data
mylist<-list(x=c(1,5,7), y=c(4,2,6), z=c(0,3,4))

#Add leading zeros so that the number becomes a 2 digit number
sprintf("%02d",df$paid_month)
df$YM <- paste0(df$paid_year, "-",sprintf("%02d",df$paid_month))

#Calling/returning an item (syn) from list (syn1) and turning it into a dataframe
df <- as.data.frame(syn1["syn"])

newuseracc <- grid[!(grid$'User Id' %in% allusers2$'User Id'),]
newuseracc <- newuseracc[!(newuseracc$'Partner Company Id' %in% accfromSF$'Partner Company Id'),]

#Rename Column 2 from existing to Partner Company Id in df accfromSF
colnames(accfromSF)[2] <- "Partner Company Id"

#Replace string characters
AT$Region <- gsub('NAM','North America',AT$Region)

#SQL commands eg select rows that satisfy conditions
row <- sqldf('select * from grid where `Partner Company Id` > 15000')

#Select row 7 in dataframe result4
yes <- results4[7,]

# Split dataframe by Var1
split(x, x$Var1)

#Find NULL values
newdata <- users_accounts[ is.na(users_accounts$`First_Name`), ]

#Or other filters
newdata2 <- users_accounts[ which(users_accounts$`Partner Company Id`>8000), ]

#Re-Order / arrange by column User Id
df <- df %>% arrange(col1,col2,col3)
upsert <- upsert[with(upsert, order(`User Id`)), ]

# TidyR
billboard2 <- billboard %>% 
  gather(week, rank, wk1:wk76, na.rm = TRUE)

# Get the position of the 4th - in the character string 
unlist(gregexpr('-',string))[4]

#Summary / Description Type of data table
str(extract)
str(df, list.len=ncol(df)) # for all columns not truncated
summary(df) # Gives summary stats on all cols including NAs, Mean Max Min etc

# Select section of string or subset of character string remove the first character or select character 2 to 20 
substr(df$col,2,20)

#Export / Write to xlsx
write.xlsx(x = newdata, file = "test.excelfile.xlsx",sheetName = "TestSheet", row.names = FALSE)

#Write csv
write.csv(newuseracc, file = "Contact upsert7 110417.csv",na="",row.names=FALSE)

library(readxl)
elastic_grid_users_301af661_a50a_4d52_be14_797f2b9b3f29 <- read_excel("C:/Working/Reporting/elastic-grid-users-301af661-a50a-4d52-be14-797f2b9b3f29.xlsx", 
                                                                      sheet = "Table1")
View(elastic_grid_users_301af661_a50a_4d52_be14_797f2b9b3f29)

#Getting the Data
Check011016 <- read.csv("C:/Users/Josh/Dropbox/Uni/Advanced Data Analytics/Assignment 2/Check011016.csv")
View(Check011016)
data <- Check011016

#Removing Rows with Duplicates in Id Column
extracttest <- extract[!duplicated(extract$Id),]

#Removing Rows with Duplicates in Id Column and max LeadCount
sorted <- there[order(there$LaunchpadId, -abs(there$LeadCount)),]
reduced <- sorted[!duplicated(sorted$LaunchpadId),]

#For loop also with vector based on number of columns and character inserted at index position
for(i in synlist){
  index <- which(names(df)==i)
  method.index <- vector("character",ncol(df))
  method.index[index] <- "parametric"
  syn1<-syn(df, method = method.index ,visit.sequence = i)
  df <- as.data.frame(syn1["syn"])
  colnames(df) <- dfcolnames
}

# This code creates dataframes containing the median and average character lengths for each column in the first 30 rows of each table
test <- list(admission_head,claim_head,provider_head,member_head)
names(test) <- c("admission","claims","providers","members")
for (i in 1:length(test)) {
  df <- data.frame(Column_Name = character(),Median_Length = numeric(),Avg_Length = numeric())
  df_m <- data.frame(Column_Name = character())
  head <- test[[i]]
  name <- paste0(names(test)[i],"_cols")
  name_m <- paste0(names(test)[i],"_cols_missing")
  for (j in colnames(head)) {
    ifelse(!is.na(median(nchar(as.character(head[[j]]))))
           ,df <- rbind(df,data.frame(Column_Name = j, 
                                      Median_Length = median(nchar(as.character(head[[j]]))), 
                                      Avg_Length = mean(nchar(as.character(head[[j]])))))
           ,df_m <- rbind(df_m,data.frame(Column_Name = j))
    )
  }
  assign(name, df)
  assign(name_m, df_m)
}

#Convert Time Date
extractold$CreatedDate <- as.POSIXct(extractold$CreatedDate, '%Y-%m-%d %H:%M:%S')

#Returns index of column with name ins_time
which(names(df)=="ins_time")

#Remove Specific Rows eg. 41
extract <- extracttest[-c(41),]

#Restart R session
ctrl + shift + F10

#Send/replace values above 5000 in the dataframe with 5000
x[x>=5000] <-5000

#Count of variables in column using table
df1 <- as.data.frame(table(df$diagnosis_code))

#Table with sum of benefit by each categorical variable (in this case MOS) in df
check <- aggregate(benefit ~ MOS,df,sum)

# Get row sum or row total for selected columns 2 to 21
tbl_final$num_members <- rowSums(tbl_final[,c(2:21)],na.rm = TRUE)

# Get column sum or total for selected columns 18 to 19
colSums(df[,c(18,19)])

#Group time-series data by month
dfmonth <- df %>% group_by(month=floor_date(service_date, "month")) %>% 
  summarise(
    amount=sum(col_name)
    , sum(ifelse(col1 > 0,1,0))
  )

#Add column from another dataframe based on match
acctoadd$CCode <- grid2$Country.Code[match(acctoadd$`User Id`, grid2$User.Id)]
df$count <- df1$Freq[match(df$diagnosis_code,df1$Var1)]

#Scatterplot of dist on X axis and ed on Y axis
scatter.smooth(x=df$dist, y=df$ed, main="Ed ~ Dist")  # scatterplot

# Create column in date table which puts 1 next to months with 5 Fridays and 0 next to months with 4 Fridays
datetbl <- datetbl %>% mutate(five_fridays_in_month = ifelse(sum(ifelse(lubridate::wday(date) == 6,1,0))== 5,1,0))

# Plot function two panes
par(mfrow = c(1,2))

# Histogram quick
hist(df$colA)

# Plot X Y
plot(df$X, df$Y)

# Remove a column col1 from the dataframe
df <- subset(df, select = -c(col1))

#API call example to hit the EG server
req <- httr::POST("https://api2.elasticgrid.com/api/v1/analytics/vendor/partnerengagement/advanced/all",
                  httr::add_headers(
                    "Authorization" = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsIng1dCI6ImEzck1VZ01Gdjl0UGNsTGE2eUYzekFrZnF1RSIsImtpZCI6ImEzck1VZ01Gdjl0UGNsTGE2eUYzekFrZnF1RSJ9.eyJjbGllbnRfaWQiOiJzYWxlc2ZvcmNlLXJlcG9ydGluZyIsImNsaWVudF9yZWZlcmVuY2VfdG9rZW5faWQiOiIzMDk3MDc1MiIsImNsaWVudF91c2VyX21pZ3JhdGlvbl90b2tlbl9pZCI6IjMwOTcwNzUyIiwic2NvcGUiOiJjaGFubmVsLWFuYWx5dGljcy1hcGkiLCJpc3MiOiJodHRwczovL2FwaTEuZWxhc3RpY2dyaWQuY29tL2F1dGgiLCJhdWQiOiJodHRwczovL2FwaTEuZWxhc3RpY2dyaWQuY29tL2F1dGgvcmVzb3VyY2VzIiwiZXhwIjoxNTEwNTM2MTA4LCJuYmYiOjE1MDI3NjAxMDh9.TY3hb_dzIux-ARBVLsvA_ryygSQhskSnQJ7xF1nqSD7v2qlciv1TkhgqwUucdLjDj-yluNuFKEU1cmmtuY7mPVW-4cWVxN3WekAybYh-0uvAc5RBingGKSJIlOQrIq5vgXWbNNtKdQw7feHHU2fn_BPG5Z2rjwqyDDBBPRMYqTbdp8J6omP1pgvEpNY7tHMdiZcI2Rta_8fcvBq7GMypbLgJKk-okgVWErjOPONgIUvdPHjPPSWgfHBjMyObfbqSyf5aJlja3O0yIdV-8OIqnpR_LklKmOPPLUeKl9lh_KJE2Nc6DDofm_EuVGYy13JGZSptDBx4irvmckcw8fXsEg",
                    "Content-Type"="application/json"
                  ),
                  body = "VendorId=80&TimeFrame=AddDays(-90)&Language=en-US&MetricValue=leads&BreakdownValue=1"
);

json <- httr::content(req, as = "parsed")

url <- "https://api2.elasticgrid.com/api/v1/analytics/vendor/partnerengagement/advanced/all"
token <- "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsIng1dCI6ImEzck1VZ01Gdjl0UGNsTGE2eUYzekFrZnF1RSIsImtpZCI6ImEzck1VZ01Gdjl0UGNsTGE2eUYzekFrZnF1RSJ9.eyJjbGllbnRfaWQiOiJzYWxlc2ZvcmNlLXJlcG9ydGluZyIsImNsaWVudF9yZWZlcmVuY2VfdG9rZW5faWQiOiIzMDk3MDc1MiIsImNsaWVudF91c2VyX21pZ3JhdGlvbl90b2tlbl9pZCI6IjMwOTcwNzUyIiwic2NvcGUiOiJjaGFubmVsLWFuYWx5dGljcy1hcGkiLCJpc3MiOiJodHRwczovL2FwaTEuZWxhc3RpY2dyaWQuY29tL2F1dGgiLCJhdWQiOiJodHRwczovL2FwaTEuZWxhc3RpY2dyaWQuY29tL2F1dGgvcmVzb3VyY2VzIiwiZXhwIjoxNTEwNTM2MTA4LCJuYmYiOjE1MDI3NjAxMDh9.TY3hb_dzIux-ARBVLsvA_ryygSQhskSnQJ7xF1nqSD7v2qlciv1TkhgqwUucdLjDj-yluNuFKEU1cmmtuY7mPVW-4cWVxN3WekAybYh-0uvAc5RBingGKSJIlOQrIq5vgXWbNNtKdQw7feHHU2fn_BPG5Z2rjwqyDDBBPRMYqTbdp8J6omP1pgvEpNY7tHMdiZcI2Rta_8fcvBq7GMypbLgJKk-okgVWErjOPONgIUvdPHjPPSWgfHBjMyObfbqSyf5aJlja3O0yIdV-8OIqnpR_LklKmOPPLUeKl9lh_KJE2Nc6DDofm_EuVGYy13JGZSptDBx4irvmckcw8fXsEg"
stay <- postForm(url,"Authorization"=token, .params = list("VendorId=80&TimeFrame=AddDays(-90)&Language=en-US&MetricValue=leads&BreakdownValue=1",.opts = list(ssl.verifypeer = FALSE)))

#Turn the List into a data frame
json <- fromJSON("C:/Users/Admin/Downloads/Vendor - Forcepoint - Partner Executions.json",simplifyDataFrame = FALSE)
json3 <- json$Executions
kjh <- do.call("rbind",json2)
kjh <- do.call("rbind",json2)
kjhg <- as.data.frame(kjh)
myFun <- function(data) {
  ListCols <- sapply(data, is.list)
  cbind(data[!ListCols], t(apply(data[ListCols], 1, unlist)))
}
ert<-myFun(kjhg)


#Copy/Duplicate a column Email and name it Email Id
df <- mutate(newuseracc, `Email Id` = Email )
#Replace Company column with Email Id Column
df$Company <- df$`Email Id`
#Drop Last Column
newuseracc <- df[, -c(8:8)]
#Rename Company column 6 to Email Id
colnames(df)[6] <- "Email Id"

#Parse number readr package
parse_number("Time50great")

#Change Date Formats
rad$`First Execution Date` <- as.Date(rad$`First Execution Date`, "%m/%d/%Y")
rad$`First Execution Date` <- format(rad$`First Execution Date`, "%d/%m/%Y")

rad <- report1492577483336
rad$`Created Date` <- as.Date(rad$`Created Date`, "%m/%d/%Y")
rad$`Created Date` <- format(rad$`Created Date`, "%d/%m/%Y")
rad$`Reg Date` <- as.Date(rad$`Reg Date`, "%m/%d/%Y")
rad$`Reg Date` <- format(rad$`Reg Date`, "%d/%m/%Y")
rad$`First Execution Date` <- as.Date(rad$`First Execution Date`, "%m/%d/%Y")
rad$`First Execution Date` <- format(rad$`First Execution Date`, "%d/%m/%Y")
rad$`Touch Date/Time` <- as.Date(rad$`Touch Date/Time`, "%m/%d/%Y")
rad$`Touch Date/Time` <- format(rad$`Touch Date/Time`, "%d/%m/%Y")
rad$`Date of Last Pipeline` <- as.Date(rad$`Date of Last Pipeline`, "%m/%d/%Y")
rad$`Date of Last Pipeline` <- format(rad$`Date of Last Pipeline`, "%d/%m/%Y")
write.csv(rad, file = "SFDC Juniper API.csv",na="",row.names=FALSE)



splitdf <- function(df, seed=NULL, train_fraction=0.8) {
  if (train_fraction<=0 | train_fraction>=1) stop("Training fraction must be strictly between 0 and 1")
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(df)
  trainindex <- sample(index, trunc(length(index)*train_fraction))
  trainset <- df[trainindex, ]
  testset <- df[-trainindex, ]
  list(trainset=trainset,testset=testset)} 

# using the function to create the partition
splits <- splitdf(data, seed=26587, train_fraction = .8)
Train <- as.data.frame(splits[1])
Test <- as.data.frame(splits[2])
names(Train) <- names(data)
names(Test) <- names(data)

#Normalise Function
norm <- function(x) {
  +     num <- x - min(x)
  +     denom <- max(x) - min(x)
  +     return (num/denom)
  + }
OR
normalized <- function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  return(y)
}

data1 <- as.data.frame(lapply(data[1:34], norm))
data1$Class <- data$Class
data <- data1

#SVM
svm_model <- svm(Class ~ ., data = Train)
svm_p <- predict(svm_model, Test[,-35])
accuracy <- sum(svm_p==Test$Class)/length(svm_p)
accuracy
summary(svm_model)

#Decision_Tree
C50_model <- C5.0(Class ~ ., data = Train)
C50_p <- predict(C50_model, Test[,-35])
accuracy <- sum(C50_p==Test$Class)/length(C50_p)
accuracy

#Naive_Bayes
NB_model <- naiveBayes(Class ~ ., data = Train)
NB_p <- predict(NB_model, Test[,-35])
accuracy <- sum(NB_p==Test$Class)/length(NB_p)
accuracy

#Random_Forest
RF_model <- randomForest(Class ~ ., data = Train)
RF_p <- predict(RF_model, Test[,-35])
accuracy <- sum(RF_p==Test$Class)/length(RF_p)
accuracy

#Boxplot
boxplot(Age~Class, data=Corf, notch=TRUE, col=(c("gold", "darkgreen")), main="Age", xlab="Income"
        
        #Factor to Numeric
        CLnorm <- as.data.frame(lapply(Corf, as.numeric))
        
        #Correlation Matrix
        sjp.corr(CLnorm)
        
        #Remove / Drop / Delete the data frame upsert from the workspace
        rm(upsert)
        
        #Subset all columns containing list of strings/states
        L02XX <- subset(DataEKG,DataEKG$State %in% c("ID","IL","MI","OH","WI","NY","TX"),drop = TRUE)
        
        libatk-1.0-0.dll
        
        #Sample
        sample <- Train[sample(1:nrow(Train), 5000, replace = FALSE),]
        
        #Tune Model
        svm_mtune <- tune.svm(Class ~ ., data = sample, gamma=10^(-6:-1), cost=10^(1:2))
        or
        tune.svm(training, as.factor(training.classes), kernel="radial", gamma=seq(0.1, 0.9, len=9), cost=seq(20, 50, len=5), probability = TRUE, tunecontrol=tune.control(cross=5))
        