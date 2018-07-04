# Reading data

credits <- read.table("data/credit.txt")
names(credits) <- c("Comptes", "Duree_credit", "Historique_credit", "Objet_credit", "Montant_credit", "Epargne", "Anciennete_emploi", "taux_effort", "Situation_familiale", "Garanties", "Anciennete_domicile", "Biens", "Age", "Autres_credits", "Statut_domicile", "Nb_credits", "Type_emploi", "Nb_pers_charge", "Telephone", "Etranger", "Cible")
credits$Cible<-credits$Cible-1


# fixing ordinal categories
credits$Comptes <- mapvalues(credits$Comptes,
                             from=c("A11","A12","A13", "A14")
                             ,to=c(0,1,2,3))
credits$Historique_credit <- mapvalues(credits$Historique_credit,
                                       from=c("A30", "A31", "A32", "A33", "A34")
                                       ,to=c(0,1,2,3,4))
credits$Epargne <- mapvalues(credits$Epargne,
                             from=c("A61", "A62", "A63", "A64", "A65")
                             ,to=c(0,1,2,3,0 ))
credits$Anciennete_emploi <- mapvalues(credits$Anciennete_emploi,
                                       from=c("A71", "A72", "A73", "A74", "A75")
                                       ,to=c(0,1,2,3, 4))
credits$Garanties <- mapvalues(credits$Garanties,
                               from=c("A101", "A102", "A103")
                               ,to=c(0,1,2))




# splitting data
obsNb <- round(nrow(credits)*0.8)
train <- credits[seq(1,obsNb),]
test <- credits[seq(obsNb+1,nrow(credits)),]

# creating and fitting model
glm.fit <- glm(Cible ~ ., data = train, family=binomial(link='logit'))

fitted.results <- predict(glm.fit,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Cible)
print(paste('Accuracy',1-misClasificError))
