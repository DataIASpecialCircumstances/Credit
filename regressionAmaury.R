credits <- read.table("data/credit.txt")
names(credits) <- c("Comptes", "Duree_credit", "Historique_credit", "Objet_credit", "Montant_credit", "Epargne", "Anciennete_emploi", "taux_effort", "Situation_familiale", "Garanties", "Anciennete_domicile", "Biens", "Age", "Autres_credits", "Statut_domicile", "Nb_credits", "Type_emploi", "Nb_pers_charge", "Telephone", "Etranger", "Cible")
credits$Cible<-credits$Cible-1

obsNb <- round(nrow(credits)*0.8)
train <- credits[seq(1,obsNb),]
test <- credits[seq(obsNb+1,nrow(credits)),]

glm.fit <- glm(Cible ~ ., data = train, family=binomial(link='logit'))

fitted.results <- predict(glm.fit,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Cible)
print(paste('Accuracy',1-misClasificError))
