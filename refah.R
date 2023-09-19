rm(list = ls())

library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(DMwR)
library(ggplot2)
library(ggthemes)


Sys.setlocale(locale = "persian")

data <- read.csv("./refahdata500000-960331.csv", header = TRUE, sep = ";")
#--------------------------------------------------------------------
zcount <- function(x) sum()###
zc <- sapply(a, zcount)
as.data.frame(zc)

naCountFunc <- function(x) sum(is.na(x))
nac <- sapply(data, naCountFunc)
as.data.frame(table(nac))
as.data.frame(nac)
data <- select(data, -FOROSH_MAX_9504, -FEE_MAX_9504)


#-------------------------------------------------------------------

compCase <- sum(complete.cases(data))

# naCountFunc <- function(x) sum(is.na(x))
# nac <- sapply(data, naCountFunc)
# as.data.frame(nac)
# data <- select(data, -FOROSH_MAX_9504, -FEE_MAX_9504)

daramad <- select(data, -c(2:9), -BIMEH_SALAMAT, -BIMEH_ROOSTAEIAN)
demography <- select(data, c(1:9), BIMEH_SALAMAT, BIMEH_ROOSTAEIAN)
nacDemo <- sapply(demography, naCountFunc)
as.data.frame(nacDemo)
nacD <- sapply(daramad, naCountFunc)
as.data.frame(nacD)
#--------------------------------------------------------------------------------
#miceMod <- mice(daramad[, !names(daramad) %in% "DARAMAD_AZ_HOGHOGH_9504"], method="rf")

daramad$MASKOONI_KHARID_PRICE[is.na(daramad$MASKOONI_KHARID_PRICE)] <- median(daramad$MASKOONI_KHARID_PRICE, na.rm = TRUE)
daramad$MASKOONI_FOROOSH_PRICE[is.na(daramad$MASKOONI_FOROOSH_PRICE)] <- median(daramad$MASKOONI_FOROOSH_PRICE, na.rm = TRUE)
daramad$DALIL_MODIR_BANKI[is.na(daramad$DALIL_MODIR_BANKI)] <- median(daramad$DALIL_MODIR_BANKI, na.rm = TRUE)
daramad$DALIL_ASNAF_MONTAKHAB[is.na(daramad$DALIL_ASNAF_MONTAKHAB)] <- median(daramad$DALIL_ASNAF_MONTAKHAB, na.rm = TRUE)
#-------------------------------------------------------------------------------
modelEbrazi <- rpart(MLT_DARAMAD_EBRAZI ~ . -MLT_DARAMAD_TASHKHISI-MLT_DARAMAD_GHATEI
                     -MLT_MALIAT_TASHKHISI-MLT_MALIAT_GHATE, 
                      data = daramad[!is.na(daramad$MLT_DARAMAD_EBRAZI), ], 
                      method = "anova", na.action = na.omit)
predictEbrazi <- predict(modelEbrazi, daramad[is.na(daramad$MLT_DARAMAD_EBRAZI), ])
daramad$MLT_DARAMAD_EBRAZI[is.na(daramad$MLT_DARAMAD_EBRAZI)] <- predictEbrazi
#---------------------------------------------------------------------------------------
modelTashkhisi <- rpart(MLT_DARAMAD_TASHKHISI ~ . -MLT_DARAMAD_GHATEI
                     -MLT_MALIAT_TASHKHISI-MLT_MALIAT_GHATE, 
                     data = daramad[!is.na(daramad$MLT_DARAMAD_TASHKHISI), ], 
                     method = "anova", na.action = na.omit)
predictTashkhisi <- predict(modelTashkhisi, daramad[is.na(daramad$MLT_DARAMAD_TASHKHISI), ])
daramad$MLT_DARAMAD_TASHKHISI[is.na(daramad$MLT_DARAMAD_TASHKHISI)] <- predictTashkhisi
#---------------------------------------------------------------------------------------
modelGhatei <- rpart(MLT_DARAMAD_GHATEI ~ . -MLT_MALIAT_TASHKHISI-MLT_MALIAT_GHATE, 
                        data = daramad[!is.na(daramad$MLT_DARAMAD_GHATEI), ], 
                        method = "anova", na.action = na.omit)
predictGhatei <- predict(modelGhatei, daramad[is.na(daramad$MLT_DARAMAD_GHATEI), ])
daramad$MLT_DARAMAD_GHATEI[is.na(daramad$MLT_DARAMAD_GHATEI)] <- predictGhatei
#---------------------------------------------------------------------------------------
modelMaliatT <- rpart(MLT_MALIAT_TASHKHISI ~ . -MLT_MALIAT_GHATE, 
                     data = daramad[!is.na(daramad$MLT_MALIAT_TASHKHISI), ], 
                     method = "anova", na.action = na.omit)
predictMaliatT <- predict(modelMaliatT, daramad[is.na(daramad$MLT_MALIAT_TASHKHISI), ])
daramad$MLT_MALIAT_TASHKHISI[is.na(daramad$MLT_MALIAT_TASHKHISI)] <- predictMaliatT
#---------------------------------------------------------------------------------------
modelMaliatG <- rpart(MLT_MALIAT_GHATE ~ ., 
                      data = daramad[!is.na(daramad$MLT_MALIAT_GHATE), ], 
                      method = "anova", na.action = na.omit)
predictMaliatG <- predict(modelMaliatG, daramad[is.na(daramad$MLT_MALIAT_GHATE), ])
daramad$MLT_MALIAT_GHATE[is.na(daramad$MLT_MALIAT_GHATE)] <- predictMaliatG
#----------------------------------------------------------------------------------------
daramad <- knnImputation(daramad)
#daramad <- filter(daramad, complete.cases(daramad))

#--------------------------------------------------------------------------
par(mar = rep(2,4))
boxplot(daramad5$DARAMADAZHOGHOGH9504_SARPARAST)
plot(daramad5$DARAMADAZHOGHOGH9504_SARPARAST)
hist(daramad5$DARAMADAZHOGHOGH9504_SARPARAST, 100)

# daramad4 <- filter(daramad4, DARAMADAZHOGHOGH9504_SARPARAST < 3e+8)
daramad <- filter(daramad, DARAMADAZHOGHOGH9504_SARPARAST < 5e+7)


#-----------------------------------------------------------------------------------
nzv <- nearZeroVar(daramad, saveMetrics = TRUE)
daramadNZV<- select(daramad, which(nzv$nzv == FALSE))

principalcomp <- prcomp(training)
stdDev <- principalcomp$sdev
prVar <- stdDev^2
propVarex <- prVar/sum(prVar)
plot(propVarex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(propVarex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
#---------------------------------------------------------------------------------
forPrediction <- filter(daramadNZV, DARAMADAZHOGHOGH9504_SARPARAST == 0)
forTrain <- filter(daramadNZV, DARAMADAZHOGHOGH9504_SARPARAST != 0)

inTrain <- createDataPartition(forTrain$DARAMADAZHOGHOGH9504_SARPARAST, p = .7, list = FALSE)
training <- forTrain[inTrain, ]
testing <- forTrain[-inTrain, ]

hist(forTrain$DARAMADAZHOGHOGH9504_SARPARAST, 100)
boxplot(forTrain$DARAMADAZHOGHOGH9504_SARPARAST)

modelGbm <- train(DARAMADAZHOGHOGH9504_SARPARAST ~ .-familycode , data = training,
                  method = "gbm")
modelRf <- train(DARAMADAZHOGHOGH9504_SARPARAST ~ . , data = training, 
                 method = "rf", ntree = 10, mtry = 2)
testGbm <- predict(modelGbm, newdata = testing)
confMxGbm <- confusionMatrix(testGbm, testing$DARAMADAZHOGHOGH9504_SARPARAST)
predictGbm <- predict(modelGbm, newdata = forPrediction)

daramad$DARAMADAZHOGHOGH9504_SARPARAST[daramad$DARAMADAZHOGHOGH9504_SARPARAST == 0] <- predictGbm
#---------------------------------------------------------------------------
#demography <- knnImputation(demography) beacuse of factor variables!
nacDemo <- sapply(demography, naCountFunc)
as.data.frame(nacDemo)

##!!!!!!!!!!!!!!!!!!!
demography$HEAD_SEX[is.na(demography$HEAD_SEX)] <- mode(demography$HEAD_SEX)
demography$BIRTH_YEAR[is.na(demography$BIRTH_YEAR)] <- mode(demography$BIRTH_YEAR)

length(which(demography$BIRTH_YEAR == 0))
length(which(demography$OSTAN_NAME == 0))

demography$BIRTH_YEAR[demography$BIRTH_YEAR == "numeric"] <- modeFunc(demography$BIRTH_YEAR)

modeFunc <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

levels(factor(demography$BIRTH_YEAR))
demography$BIRTH_YEAR[demography$BIRTH_YEAR == 0] <- modeFunc(demography$BIRTH_YEAR)

levels(factor(demography$OSTAN_NAME))
demography$OSTAN_NAME[demography$OSTAN_NAME == 0 | demography$OSTAN_NAME == ""] <- modeFunc(demography$OSTAN_NAME)
#---------------------------------------------------------------------------
trainedData <- merge(demography, daramad)
trainedData <- mutate(trainedData, daramadDecile = ntile(DARAMADAZHOGHOGH9504_SARPARAST, 10))
trainedData <- mutate(trainedData, poor = ifelse(daramadDecile > 3, 0, 1))


png("daramadSeni.png", width = 1000, height = 1000)
daramadSeni <- ggplot(trainedData, aes(x = BIRTH_YEAR, y = "count", fill = poor ))
daramadSeni + geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "poverty dist. based on supervisers birth year", y = "Number of families")
dev.off()

png("daramadOstani.png", width = 1200)
daramadOstani <- ggplot(trainedData, aes(x = OSTAN_NAME, y = "count", fill = poor))
daramadOstani + geom_bar(stat = "identity") +
  labs(title = "poverty dist. based on supervisers birth year", y = "Number of families")
dev.off()

png("daramadSeniBox.png", width = 1200, height = 800)
daramadSeniBox <- ggplot(trainedData, aes(x = factor(BIRTH_YEAR), y = DARAMADAZHOGHOGH9504_SARPARAST, fill = poor))
daramadSeniBox +geom_boxplot()
dev.off()

png("daramadOstaniBox.png", width = 1200, height = 800)
daramadOstaniBox <- ggplot(trainedData, aes(x = OSTAN_NAME, y = DARAMADAZHOGHOGH9504_SARPARAST))
daramadOstaniBox + geom_boxplot() + ylim(0, 2e+07)
dev.off()


