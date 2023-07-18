setwd("~/Studium/10. Semester/wima-praktikum")

packages = c("tidyverse", "ggplot2", "dplyr", "glmnet", "patchwork")
lapply(packages, library, character.only = TRUE)

#Überblick über Datensatz
daten = readRDS('datensatz.rds')
head(daten)
summary(daten)

#interessiert an improvement
daten = daten%>%
  mutate(improvebody = postbody - prebody,
         improvelet = postlet - prelet,
         improveform = postform - preform,
         improvenumb = postnumb - prenumb,
         improverelat = postrelat - prerelat,
         improveclasf = postclasf - preclasf
  )

#### boxplot zur identifikation von outliern
ggplot(daten, aes(,improvebody))+geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE) +ggplot(daten, aes(,improvelet))+geom_boxplot(outlier.colour="black", outlier.shape=16,
                                                                                        outlier.size=2, notch=FALSE) 

### -- 2-Stichproben t-Test --
## -- Encouragement --
#Aufteilung in Test- und Kontroll-Gruppe
daten.encour.1 = filter(daten, encour == 1)
daten.encour.0 = filter(daten, encour == 0)

summary(daten.encour.1)
summary(daten.encour.0)


###
qq.postbody.enc0 = ggplot(daten.encour.0,  aes(sample=postbody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postbody','für encour=0')
qq.postbody.enc1 = ggplot(daten.encour.1,  aes(sample=postbody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postbody','für encour=1')
qq.postlet.enc0 = ggplot(daten.encour.0,  aes(sample=postlet)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postlet','für encour=0')
qq.postlet.enc1 = ggplot(daten.encour.1,  aes(sample=postlet)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postbody','für encour=1')
qq.postform.enc0 = ggplot(daten.encour.0,  aes(sample=postform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postform','für encour=0')
qq.postform.enc1 = ggplot(daten.encour.1,  aes(sample=postform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postform','für encour=1')
qq.postnumb.enc0 = ggplot(daten.encour.0,  aes(sample=postnumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postnumb','für encour=0')
qq.postnumb.enc1 = ggplot(daten.encour.1,  aes(sample=postnumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postnumb','für encour=1')
qq.postrelat.enc0 = ggplot(daten.encour.0,  aes(sample=postrelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postrelat','für encour=0')
qq.postrelat.enc1 = ggplot(daten.encour.1,  aes(sample=postrelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postrelat','für encour=1')
qq.postclasf.enc0 = ggplot(daten.encour.0,  aes(sample=postclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postclasf','für encour=0')
qq.postclasf.enc1 = ggplot(daten.encour.1,  aes(sample=postclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot postclasf','für encour=1')
(qq.postbody.enc1 + qq.postlet.enc1 + qq.postform.enc1)/( qq.postnumb.enc1 + qq.postrelat.enc1 + qq.postclasf.enc1)
  (qq.postbody.enc0 + qq.postlet.enc0 + qq.postform.enc0)/( qq.postnumb.enc0 + qq.postrelat.enc0 + qq.postclasf.enc0)

##improve
qq.improvebody.enc0 = ggplot(daten.encour.0,  aes(sample=improvebody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','für encour=0')
qq.improvebody.enc1 = ggplot(daten.encour.1,  aes(sample=improvebody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','für encour=1')
qq.improvelet.enc0 = ggplot(daten.encour.0,  aes(sample=improvelet)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvelet','für encour=0')
qq.improvelet.enc1 = ggplot(daten.encour.1,  aes(sample=improvelet)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','für encour=1')
qq.improveform.enc0 = ggplot(daten.encour.0,  aes(sample=improveform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveform','für encour=0')
qq.improveform.enc1 = ggplot(daten.encour.1,  aes(sample=improveform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveform','für encour=1')
qq.improvenumb.enc0 = ggplot(daten.encour.0,  aes(sample=improvenumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvenumb','für encour=0')
qq.improvenumb.enc1 = ggplot(daten.encour.1,  aes(sample=improvenumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvenumb','für encour=1')
qq.improverelat.enc0 = ggplot(daten.encour.0,  aes(sample=improverelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improverelat','für encour=0')
qq.improverelat.enc1 = ggplot(daten.encour.1,  aes(sample=improverelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improverelat','für encour=1')
qq.improveclasf.enc0 = ggplot(daten.encour.0,  aes(sample=improveclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveclasf','für encour=0')
qq.improveclasf.enc1 = ggplot(daten.encour.1,  aes(sample=improveclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveclasf','für encour=1')
(qq.improvebody.enc1 + qq.improvelet.enc1 + qq.improveform.enc1)/( qq.improvenumb.enc1 + qq.improverelat.enc1 + qq.improveclasf.enc1)
(qq.improvebody.enc0 + qq.improvelet.enc0 + qq.improveform.enc0)/( qq.improvenumb.enc0 + qq.improverelat.enc0 + qq.improveclasf.enc0)

#body
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.encour.0$improvebody) #20.28004
var(daten.encour.1$improvebody) #28.81335
#Normalverteilung in beiden Gruppen
hist.body.enc0 = ggplot(daten.encour.0, aes(improvebody)) +
  geom_histogram() + ggtitle('Histogramm improvebody','in der Kontrollgruppe')
qq.body.enc0 = ggplot(daten.encour.0,  aes(sample=improvebody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','in der Kontrollgruppe')
hist.body.enc1 = ggplot(daten.encour.1, aes(improvebody)) +
  geom_histogram() + ggtitle('Histogramm improvebody','in der Testgruppe')
qq.body.enc1 = ggplot(daten.encour.1,  aes(sample=improvebody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','in der Testgruppe')
(hist.body.enc1 + hist.body.enc0)/(qq.body.enc1 + qq.body.enc0)

# -- 2-Gruppen t-test --
t.test(daten.encour.1$improvebody, daten.encour.0$improvebody, var.equal = TRUE)
t.test(daten.encour.1$improvebody, daten.encour.0$improvebody, var.equal = TRUE, conf.level=0.74)


#let
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.encour.0$improvelet) #128.8922
var(daten.encour.1$improvelet) #115.2583

#Normalverteilung in beiden Gruppen
hist.let.enc0 = ggplot(daten.encour.0, aes(improvelet)) +
  geom_histogram() + ggtitle('Histogramm improvelet','in der Kontrollgruppe')
qq.let.enc0 = ggplot(daten.encour.0,  aes(sample=improvelet)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvelet','in der Kontrollgruppe')
hist.let.enc1 = ggplot(daten.encour.1, aes(improvelet)) +
  geom_histogram() + ggtitle('Histogramm improvelet','in der Testgruppe')
qq.let.enc1 = ggplot(daten.encour.1,  aes(sample=improvelet)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvelet','in der Testgruppe')
(hist.let.enc1 + hist.let.enc0)/(qq.let.enc1 + qq.let.enc0)

# -- 2-Gruppen t-test --
t.test(daten.encour.1$improvelet, daten.encour.0$improvelet, var.equal = TRUE)
t.test(daten.encour.1$improvelet, daten.encour.0$improvelet, var.equal = TRUE, conf.level = 0.9981)

#form
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.encour.0$improveform) #12.46395
var(daten.encour.1$improveform) #14.84903
#Normalverteilung in beiden Gruppen
hist.form.enc0 = ggplot(daten.encour.0, aes(improveform)) +
  geom_histogram() + ggtitle('Histogramm improveform','in der Kontrollgruppe')
qq.form.enc0 = ggplot(daten.encour.0,  aes(sample=improveform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveform','in der Kontrollgruppe')
hist.form.enc1 = ggplot(daten.encour.1, aes(improveform)) +
  geom_histogram() + ggtitle('Histogramm improveform','in der Testgruppe')
qq.form.enc1 = ggplot(daten.encour.1,  aes(sample=improveform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveform','in der Testgruppe')
(hist.form.enc1 + hist.form.enc0)/(qq.form.enc1 + qq.form.enc0)

# -- 2-Gruppen t-test --
t.test(daten.encour.1$improveform, daten.encour.0$improveform, var.equal = TRUE)
t.test(daten.encour.1$improveform, daten.encour.0$improveform, var.equal = TRUE, conf.level = 0.84)

#numb
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.encour.0$improvenumb) #100.1942
var(daten.encour.1$improvenumb) #89.74974
#Normalverteilung in beiden Gruppen
hist.numb.enc0 = ggplot(daten.encour.0, aes(improvenumb)) +
  geom_histogram() + ggtitle('Histogramm improvenumb','in der Kontrollgruppe')
qq.numb.enc0 = ggplot(daten.encour.0,  aes(sample=improvenumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvenumb','in der Kontrollgruppe')
hist.numb.enc1 = ggplot(daten.encour.1, aes(improvenumb)) +
  geom_histogram() + ggtitle('Histogramm improvenumb','in der Testgruppe')
qq.numb.enc1 = ggplot(daten.encour.1,  aes(sample=improvenumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvenumb','in der Testgruppe')
(hist.numb.enc1 + hist.numb.enc0)/(qq.numb.enc1 + qq.numb.enc0)

# -- 2-Gruppen t-test --
t.test(daten.encour.1$improvenumb, daten.encour.0$improvenumb, var.equal = TRUE)
t.test(daten.encour.1$improvenumb, daten.encour.0$improvenumb, var.equal = TRUE, conf.level = 0.85)

#relat
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.encour.0$improverelat) #12.05852
var(daten.encour.1$improverelat) #12.01307
#Normalverteilung in beiden Gruppen
hist.relat.enc0 = ggplot(daten.encour.0, aes(improverelat)) +
  geom_histogram() + ggtitle('Histogramm improverelat','in der Kontrollgruppe')
qq.relat.enc0 = ggplot(daten.encour.0,  aes(sample=improverelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improverelat','in der Kontrollgruppe')
hist.relat.enc1 = ggplot(daten.encour.1, aes(improverelat)) +
  geom_histogram() + ggtitle('Histogramm improverelat','in der Testgruppe')
qq.relat.enc1 = ggplot(daten.encour.1,  aes(sample=improverelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improverelat','in der Testgruppe')
(hist.relat.enc1 + hist.relat.enc0)/(qq.relat.enc1 + qq.relat.enc0)

# -- 2-Gruppen t-test --
t.test(daten.encour.1$improverelat, daten.encour.0$improverelat, var.equal = TRUE)
t.test(daten.encour.1$improverelat, daten.encour.0$improverelat, var.equal = TRUE, conf.level = 0.84)

#clasf
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.encour.0$improveclasf) #17.91379
var(daten.encour.1$improveclasf) #20.90376
#Normalverteilung in beiden Gruppen
hist.clasf.enc0 = ggplot(daten.encour.0, aes(improveclasf)) +
  geom_histogram() + ggtitle('Histogramm improveclasf','in der Kontrollgruppe')
qq.clasf.enc0 = ggplot(daten.encour.0,  aes(sample=improveclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveclasf','in der Kontrollgruppe')
hist.clasf.enc1 = ggplot(daten.encour.1, aes(improveclasf)) +
  geom_histogram() + ggtitle('Histogramm improveclasf','in der Testgruppe')
qq.clasf.enc1 = ggplot(daten.encour.1,  aes(sample=improveclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveclasf','in der Testgruppe')
(hist.clasf.enc1 + hist.clasf.enc0)/(qq.clasf.enc1 + qq.clasf.enc0)

# -- 2-Gruppen t-test --
t.test(daten.encour.1$improveclasf, daten.encour.0$improveclasf, var.equal = TRUE)
t.test(daten.encour.1$improveclasf, daten.encour.0$improveclasf, var.equal = TRUE, conf.level = 0.84)


### -- 2-Stichproben t-Test --
## -- Regular --
#Aufteilung in Test- und Kontroll-Gruppe
daten.regular.1 = filter(daten, regular == 1)
daten.regular.0 = filter(daten, regular == 0)

#body
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.regular.0$improvebody) #24.66981
var(daten.regular.1$improvebody) #25.99108
#Normalverteilung in beiden Gruppen
hist.body.reg0 = ggplot(daten.regular.0, aes(improvebody)) +
  geom_histogram() + ggtitle('Histogramm improvebody','in der Kontrollgruppe')
qq.body.reg0 = ggplot(daten.regular.0,  aes(sample=improvebody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','in der Kontrollgruppe')
hist.body.reg1 = ggplot(daten.regular.1, aes(improvebody)) +
  geom_histogram() + ggtitle('Histogramm improvebody','in der Testgruppe')
qq.body.reg1 = ggplot(daten.regular.1,  aes(sample=improvebody)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvebody','in der Testgruppe')
(hist.body.reg1 + hist.body.reg0)/(qq.body.reg1 + qq.body.reg0)

# -- 2-Gruppen t-test --
t.test(daten.regular.1$improvebody, daten.regular.0$improvebody, var.equal = TRUE)

#let
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.regular.0$improvelet) #45.49965
var(daten.regular.1$improvelet) #122.0106

#Normalverteilung in beiden Gruppen

# -- 2-Gruppen t-test --
t.test(daten.regular.1$improvelet, daten.regular.0$improvelet, var.equal = FALSE)
t.test(daten.regular.1$improvelet, daten.regular.0$improvelet, var.equal = TRUE)

#form
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.regular.0$improveform) #14.93082
var(daten.regular.1$improveform) #13.45205
#Normalverteilung in beiden Gruppen
hist.form.reg0 = ggplot(daten.regular.0, aes(improveform)) +
  geom_histogram() + ggtitle('Histogramm improveform','in der Kontrollgruppe')
qq.form.reg0 = ggplot(daten.regular.0,  aes(sample=improveform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveform','in der Kontrollgruppe')
hist.form.reg1 = ggplot(daten.regular.1, aes(improveform)) +
  geom_histogram() + ggtitle('Histogramm improveform','in der Testgruppe')
qq.form.reg1 = ggplot(daten.regular.1,  aes(sample=improveform)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveform','in der Testgruppe')
(hist.form.reg1 + hist.form.reg0)/(qq.form.reg1 + qq.form.reg0)

# -- 2-Gruppen t-test --
t.test(daten.regular.1$improveform, daten.regular.0$improveform, var.equal = TRUE)
t.test(daten.regular.1$improveform, daten.regular.0$improveform, var.equal = TRUE, conf.level = 0.84)

#numb
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.regular.0$improvenumb) #42.35919
var(daten.regular.1$improvenumb) #101.4617
#Normalverteilung in beiden Gruppen
hist.numb.reg0 = ggplot(daten.regular.0, aes(improvenumb)) +
  geom_histogram() + ggtitle('Histogramm improvenumb','in der Kontrollgruppe')
qq.numb.reg0 = ggplot(daten.regular.0,  aes(sample=improvenumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvenumb','in der Kontrollgruppe')
hist.numb.reg1 = ggplot(daten.regular.1, aes(improvenumb)) +
  geom_histogram() + ggtitle('Histogramm improvenumb','in der Testgruppe')
qq.numb.reg1 = ggplot(daten.regular.1,  aes(sample=improvenumb)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improvenumb','in der Testgruppe')
(hist.numb.reg1 + hist.numb.reg0)/(qq.numb.reg1 + qq.numb.reg0)

# -- 2-Gruppen t-test --
t.test(daten.regular.1$improvenumb, daten.regular.0$improvenumb, var.equal = FALSE)
t.test(daten.regular.1$improvenumb, daten.regular.0$improvenumb, var.equal = TRUE)

#relat
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.regular.0$improverelat) #15.04717
var(daten.regular.1$improverelat) #11.17539
#Normalverteilung in beiden Gruppen
hist.relat.reg0 = ggplot(daten.regular.0, aes(improverelat)) +
  geom_histogram() + ggtitle('Histogramm improverelat','in der Kontrollgruppe')
qq.relat.reg0 = ggplot(daten.regular.0,  aes(sample=improverelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improverelat','in der Kontrollgruppe')
hist.relat.reg1 = ggplot(daten.regular.1, aes(improverelat)) +
  geom_histogram() + ggtitle('Histogramm improverelat','in der Testgruppe')
qq.relat.reg1 = ggplot(daten.regular.1,  aes(sample=improverelat)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improverelat','in der Testgruppe')
(hist.relat.reg1 + hist.relat.reg0)/(qq.relat.reg1 + qq.relat.reg0)

# -- 2-Gruppen t-test --
t.test(daten.regular.1$improverelat, daten.regular.0$improverelat, var.equal = TRUE)
t.test(daten.regular.1$improverelat, daten.regular.0$improverelat, var.equal = TRUE, conf.level = 0.84)

#clasf
# -- Überprüfungvorraussetzungen --

#Varianz
var(daten.regular.0$improveclasf) #17.029
var(daten.regular.1$improveclasf) #20.28445
#Normalverteilung in beiden Gruppen
hist.clasf.reg0 = ggplot(daten.regular.0, aes(improveclasf)) +
  geom_histogram() + ggtitle('Histogramm improveclasf','in der Kontrollgruppe')
qq.clasf.reg0 = ggplot(daten.regular.0,  aes(sample=improveclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveclasf','in der Kontrollgruppe')
hist.clasf.reg1 = ggplot(daten.regular.1, aes(improveclasf)) +
  geom_histogram() + ggtitle('Histogramm improveclasf','in der Testgruppe')
qq.clasf.reg1 = ggplot(daten.regular.1,  aes(sample=improveclasf)) +
  stat_qq() + stat_qq_line() + ggtitle('QQ-Plot improveclasf','in der Testgruppe')
(hist.clasf.reg1 + hist.clasf.reg0)/(qq.clasf.reg1 + qq.clasf.reg0)

# -- 2-Gruppen t-test --
t.test(daten.regular.1$improveclasf, daten.regular.0$improveclasf, var.equal = TRUE)

#-----
summary(daten.regular.1)
summary(daten.regular.0)

#### Weitere Gruppenaufteilung
daten.encour.0.regular.0 = filter(daten.encour.0, regular ==0)
daten.encour.0.regular.1 = filter(daten.encour.0, regular ==1)
daten.encour.1.regular.0 = filter(daten.encour.1, regular ==0)
daten.encour.1.regular.1 = filter(daten.encour.1, regular ==1)

mean(daten.encour.0.regular.0$improvelet)
mean(daten.encour.0.regular.1$improvelet)
mean(daten.encour.1.regular.0$improvelet)
mean(daten.encour.1.regular.1$improvelet)

#----Prädiktionsintervalle -encour
# --improvebody
lm.imprbody.encour = lm(improvebody ~ encour, daten)
confint(lm.imprbody.encour)
#use model to create prediction intervals
pred.imprbody.encour <- predict(lm.imprbody.encour, interval = "predict")
pred.imprbody.encour
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprbody.encour)
#create plot
pred.imprbody = ggplot(all_data, aes(x =encour, y = improvebody)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improveform
lm.imprform.encour = lm(improveform ~ encour, daten)
confint(lm.imprform.encour)
#use model to create prediction intervals
pred.imprform.encour <- predict(lm.imprform.encour, interval = "predict")
pred.imprform.encour
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprform.encour)
#create plot
pred.imprform = ggplot(all_data, aes(x =encour, y = improveform)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improvelet
lm.imprlet.encour = lm(improvelet ~ encour, daten)
confint(lm.imprlet.encour)
#use model to create prediction intervals
pred.imprlet.encour <- predict(lm.imprlet.encour, interval = "predict")
pred.imprlet.encour
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprlet.encour)
#create plot
pred.imprlet = ggplot(all_data, aes(x =encour, y = improvelet)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval


# --improvenumb
lm.imprnumb.encour = lm(improvenumb ~ encour, daten)
confint(lm.imprnumb.encour)
#use model to create prediction intervals
pred.imprnumb.encour <- predict(lm.imprnumb.encour, interval = "predict")
pred.imprnumb.encour
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprnumb.encour)
#create plot
pred.imprnumb = ggplot(all_data, aes(x =encour, y = improvenumb)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improverelat
lm.imprrelat.encour = lm(improverelat ~ encour, daten)
confint(lm.imprrelat.encour)
#use model to create prediction intervals
pred.imprrelat.encour <- predict(lm.imprrelat.encour, interval = "predict")
pred.imprrelat.encour
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprrelat.encour)
#create plot
pred.imprrelat = ggplot(all_data, aes(x =encour, y = improverelat)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improveclasf
lm.imprclasf.encour = lm(improveclasf ~ encour, daten)
confint(lm.imprclasf.encour)
#use model to create prediction intervals
pred.imprclasf.encour <- predict(lm.imprclasf.encour, interval = "predict")
pred.imprclasf.encour
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprclasf.encour)
#create plot
pred.imprclasf = ggplot(all_data, aes(x =encour, y = improveclasf)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

(pred.imprform +pred.imprlet)
(pred.imprnumb + pred.imprclasf)
(pred.imprrelat)

##Prädiktionsintervalle regular
# --improvebody
lm.imprbody.regular = lm(improvebody ~ regular, daten)
confint(lm.imprbody.regular)
#use model to create prediction intervals
pred.imprbody.regular <- predict(lm.imprbody.regular, interval = "predict")
pred.imprbody.regular
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprbody.regular)
#create plot
pred.imprbody = ggplot(all_data, aes(x =regular, y = improvebody)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improveform
lm.imprform.regular = lm(improveform ~ regular, daten)
confint(lm.imprform.regular)
#use model to create prediction intervals
pred.imprform.regular <- predict(lm.imprform.regular, interval = "predict")
pred.imprform.regular
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprform.regular)
#create plot
pred.imprform = ggplot(all_data, aes(x =regular, y = improveform)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improvelet
lm.imprlet.regular = lm(improvelet ~ regular, daten)
confint(lm.imprlet.regular)
#use model to create prediction intervals
pred.imprlet.regular <- predict(lm.imprlet.regular, interval = "predict")
pred.imprlet.regular
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprlet.regular)
#create plot
pred.imprlet = ggplot(all_data, aes(x =regular, y = improvelet)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval


# --improvenumb
lm.imprnumb.regular = lm(improvenumb ~ regular, daten)
confint(lm.imprnumb.regular)
#use model to create prediction intervals
pred.imprnumb.regular <- predict(lm.imprnumb.regular, interval = "predict")
pred.imprnumb.regular
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprnumb.regular)
#create plot
pred.imprnumb = ggplot(all_data, aes(x =regular, y = improvenumb)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improverelat
lm.imprrelat.regular = lm(improverelat ~ regular, daten)
confint(lm.imprrelat.regular)
#use model to create prediction intervals
pred.imprrelat.regular <- predict(lm.imprrelat.regular, interval = "predict")
pred.imprrelat.regular
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprrelat.regular)
#create plot
pred.imprrelat = ggplot(all_data, aes(x =regular, y = improverelat)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

# --improveclasf
lm.imprclasf.regular = lm(improveclasf ~ regular, daten)
confint(lm.imprclasf.regular)
#use model to create prediction intervals
pred.imprclasf.regular <- predict(lm.imprclasf.regular, interval = "predict")
pred.imprclasf.regular
#create dataset that contains original data along with prediction intervals
all_data <- cbind(daten, pred.imprclasf.regular)
#create plot
pred.imprclasf = ggplot(all_data, aes(x =regular, y = improveclasf)) + #define x and y axis variables
  geom_point() + #add scatterplot points
  stat_smooth(method = lm, col = "darkgreen", size = 0.5) + #confidence bands
  geom_line(aes(y = lwr), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(y = upr), col = "coral2", linetype = "dashed") #upr pred interval

(pred.imprbody + pred.imprform)/( pred.imprlet + pred.imprnumb)/( pred.imprclasf +pred.imprrelat)

#----Multivariate lineare Regression/ Lasso
#--body
#-volles regressionsmodell
lm_all_postbody = lm(postbody~ sex + age + viewcat + setting+ as.factor(site) + encour + regular + prebody + preform + prelet + prenumb + prerelat + preclasf + peabody, daten)
summary(lm_all_postbody)
#Preparing Training & Test Data Set
set.seed(1)
train = sample(nrow(daten),nrow(daten)/2)
test = -train
x = model.matrix(postbody~as.factor(site)+sex+age+setting+prebody+prelet+preform+prenumb+prerelat+preclasf+peabody+encour+regular, daten) [,-1] # matrix that contains all possible 
y = daten$postbody # y to be predicted
y.test = y[test]
#Find Lambda that minimizes testsample error
lasso.mod = glmnet(x[train,],y[train], alpha =1) # fit lasso model on trainig data 
lasso.pred = predict(lasso.mod,s=1,newx=x[test,]) #evaluate with test data set
mean((lasso.pred-y.test)^2) # calculate test sample error
#find optimal lambda using cv
cv.out1=cv.glmnet(x[train,],y[train],alpha=1) #10 fold crossvalidation 
plot(cv.out1)
bestlam1=cv.out1$lambda.min
bestlam1

lasso.pred = predict(lasso.mod,s=bestlam,newx=x[test,]) #evaluate with test data set
mean((lasso.pred-y.test)^2) # calculate test sample error

#refit model on full data set
out=glmnet(x,y,alpha=1)
lasso.coef = predict(out,type="coefficients",s=bestlam2)[1:17,]
lasso.coef
