# Reading data

credits <- read.table("data/credit.txt")
names(credits) <- c("Comptes", "Duree_credit", "Historique_credit", "Objet_credit", "Montant_credit", "Epargne", "Anciennete_emploi", "taux_effort", "Situation_familiale", "Garanties", "Anciennete_domicile", "Biens", "Age", "Autres_credits", "Statut_domicile", "Nb_credits", "Type_emploi", "Nb_pers_charge", "Telephone", "Etranger", "Cible")
credits$Cible<-credits$Cible-1


# fixing ordinal categories
library(plyr)
credits$Comptes <- mapvalues(credits$Comptes,
                             from=c("A11","A12","A13", "A14")
                             ,to=c(0,1,2,3))
credits$Comptes <- as.numeric(as.character(credits$Comptes)) 

credits$Historique_credit <- mapvalues(credits$Historique_credit,
                                       from=c("A30", "A31", "A32", "A33", "A34")
                                       ,to=c(0,1,2,3,4))
credits$Historique_credit <- as.numeric(as.character(credits$Historique_credit)) 


credits$Epargne <- mapvalues(credits$Epargne,
                             from=c("A61", "A62", "A63", "A64", "A65")
                             ,to=c(0,1,2,3,0 ))
credits$Epargne <- as.numeric(as.character(credits$Epargne)) 

credits$Anciennete_emploi <- mapvalues(credits$Anciennete_emploi,
                                       from=c("A71", "A72", "A73", "A74", "A75")
                                       ,to=c(0,1,2,3, 4))
credits$Anciennete_emploi <- as.numeric(as.character(credits$Anciennete_emploi)) 

credits$Type_emploi <- mapvalues(credits$Type_emploi,
                                       from=c("A171", "A172", "A173", "A174")
                                       ,to=c(0,1,2,3))
credits$Type_emploi <- as.numeric(as.character(credits$Type_emploi)) 

credits$Garanties <- mapvalues(credits$Garanties,
                               from=c("A101", "A102", "A103")
                               ,to=c(0,1,2))
credits$Garanties <- as.numeric(as.character(credits$Garanties)) 

credits$Statut_domicile <- as.numeric(credits$Statut_domicile=="A152")

# one hot encoding
oneHotColumns = c("Objet_credit", "Situation_familiale", "Biens", "Autres_credits", "Statut_domicile", "Telephone", "Etranger")

# create the One Hot filter
OHcredits <- onehot::onehot(credits[oneHotColumns])
#Apply it on the columns, to create a one-hot encoded dataframe for these columns
oneHotcredits <- predict(OHcredits, credits[oneHotColumns])
oneHotcredits <- data.frame(oneHotcredits)

#remove the original columns
credits[oneHotColumns] <- NULL
#append the new dataframe to the old one
credits<-cbind(credits, oneHotcredits)

# library(caret)
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# credits$Cible <- as.factor(credits$Cible)
# model <- train(Cible~., data=credits, method="lvq", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)



# splitting data
ratio = 0.8
obsNb <- round(nrow(credits)*ratio)
train <- credits[seq(1,obsNb),]
test <- credits[seq(obsNb+1,nrow(credits)),]

# creating and fitting model
model <- glm(Cible ~ Comptes+Historique_credit+taux_effort, data = train, family=binomial(link='logit'))
summary(model)

results <- predict(model,newdata=test,type='response')
results <- ifelse(results > 0.5,1,0)
misClasificError <- mean(results != test$Cible)
print(paste('Accuracy',1-misClasificError))

