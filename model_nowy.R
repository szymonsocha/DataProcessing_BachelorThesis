library("lmtest")
library("sandwich")
library("stargazer")
library(rcompanion)
library("car")
options(scipen=999)

setwd("C:\\Users\\szymo\\Desktop\\Nauka\\Licencjat")

dane = read.csv(file = "Dane\\licencjat dane\\merged.csv", sep=",", header = TRUE)

# Usuwam te kraje, w których byly braki danych
unique(dane$country_region)
dane = dane[!dane$country_region == 'Cyprus',]
dane = dane[!dane$country_region == 'Luxembourg',]
dane = dane[!dane$country_region == 'Russia',]
dane = dane[!dane$country_region == 'Ukraine',]
dane = dane[!dane$country_region == 'South Korea',]

# Sprawdzenie w ktorych krajach brakuje obserwacji
for (val in unique(dane$country_region)) {
  x = dane[dane$country_region == val,]
  print(length(x$data))
  print(unique(x$country_region))
}
remove(x)

View(dane[dane$country_region == 'Bulgaria',])


# Przesuniecie danych w celu zlogarytmowania zmiennej
dane$zmiana_log <- dane$zmiana + 46

# Zmiana poziomu bazowego zmiennnej weekday i month
dane$weekday = relevel(factor(dane$weekday), "monday")
dane$month = relevel(factor(dane$month), "April")   
dane$country_region = relevel(factor(dane$country_region), "Poland")


#############################
# MODELE
#############################
model_dumm = lm(zmiana~as.factor(dane$country_region)+workplaces_percent_change_from_baseline+as.factor(dane$weekday)+as.factor(dane$month), 
           data=dane)
summary(model_dumm)

model_dumm_log = lm(log(zmiana_log)~as.factor(dane$country_region)+workplaces_percent_change_from_baseline+as.factor(dane$weekday)+as.factor(dane$month), 
           data=dane)
summary(model_dumm_log)

######
# glowny
model = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane$weekday)+as.factor(dane$month), 
                data=dane)
summary(model)
######

model_log = lm(log(zmiana_log)~I(workplaces_percent_change_from_baseline^2)+as.factor(dane$weekday)+as.factor(dane$month),
               data=dane)
summary(model_log)

#############################
# TESTY
#############################
resettest(model, power=2:3, type="fitted")
# p-value = 0.06155 
# tylko ten model spelnia RESET, dodanie kwadratow, logarytmu zle wplywa na model

car::vif(model) # dobrze

plot(model) # tez chyba dobrze

bptest(model)
# p-value = 0, niestety heteroskedastycznosc

# Stosuje zatem macierz odporna White'a
coeftest(model, vcov.=vcovHC(model, type = "HC0"))
model_robust = (coeftest(model, vcov.=vcovHC(model, type = "HC0")))

#stargazer(model_dumm, model, model_robust, type="html", df=FALSE, out="C:\\Users\\szymo\\Desktop\\star_dumm2.doc")
stargazer(model, model_dumm, model_robust, type="text", df=FALSE)

# Efekty stale dla krajow nie spelniaja testu RESET dlatego porownuje dwa modele (fixed i zwykly)
# i patrze czy oszacowanie parametru sie zmienia (nie zmienia sie)

###############################################
model1 = lm(zmiana~workplaces_percent_change_from_baseline, 
           data=dane)
coeftest(model1, vcov.=vcovHC(model1, type = "HC0"))
model_robust1 = (coeftest(model1, vcov.=vcovHC(model1, type = "HC0")))

model2 = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane$month), 
            data=dane)
coeftest(model2, vcov.=vcovHC(model2, type = "HC0"))
model_robust2 = (coeftest(model2, vcov.=vcovHC(model2, type = "HC0")))

model3 = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane$month)+as.factor(dane$weekday), 
            data=dane)
coeftest(model3, vcov.=vcovHC(model3, type = "HC0"))
model_robust3 = (coeftest(model3, vcov.=vcovHC(model3, type = "HC0")))

model4 = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane$month)+as.factor(dane$weekday)+as.factor(dane$country_region), 
            data=dane)
coeftest(model4, vcov.=vcovHC(model4, type = "HC0"))
model_robust4 = (coeftest(model4, vcov.=vcovHC(model4, type = "HC0")))

stargazer(model_robust1, model_robust2, model_robust3, model_robust4, type="text", df=FALSE)
#stargazer(model_robust1, model_robust2, model_robust3, model_robust4, type="html", df=FALSE, out="C:\\Users\\szymo\\Desktop\\star_fix.doc")

########
# TESTY
########
plot(log(dane$zmiana), dane$workplaces_percent_change_from_baseline)
model1 = lm(zmiana~workplaces_percent_change_from_baseline, 
            data=dane)
plot(model1)
min(dane$workplaces_percent_change_from_baseline)

resettest(model1, power=2:3, type="fitted")
resettest(model2, power=2:3, type="fitted")
resettest(model3, power=2:3, type="fitted")
resettest(model4, power=2:3, type="fitted")

bptest(model1)
bptest(model2)
bptest(model3)
bptest(model4)

###
dane_high = dane[dane$income == 'high',]
dane_mid_high = dane[dane$income == 'upper_middle',]
dane_mid_low = dane[dane$income == 'lower_middle',]
dane_high$country_region = relevel(factor(dane_high$country_region), "Poland")
dane_mid_high$country_region = relevel(factor(dane_mid_high$country_region), "Turkey")
dane_mid_low$country_region = relevel(factor(dane_mid_low$country_region), "Egypt")

model_high = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane_high$month)+as.factor(dane_high$weekday)+as.factor(dane_high$country_region), 
            data=dane_high)
coeftest(model_high, vcov.=vcovHC(model_high, type = "HC0"))
model_robust_high = (coeftest(model_high, vcov.=vcovHC(model_high, type = "HC0")))

model_mid_high = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane_mid_high$month)+as.factor(dane_mid_high$weekday)+as.factor(dane_mid_high$country_region), 
            data=dane_mid_high)
coeftest(model4, vcov.=vcovHC(model_mid_high, type = "HC0"))
model_robust_mid_high = (coeftest(model_mid_high, vcov.=vcovHC(model_mid_high, type = "HC0")))

model_mid_low = lm(zmiana~workplaces_percent_change_from_baseline+as.factor(dane_mid_low$month)+as.factor(dane_mid_low$weekday)+as.factor(dane_mid_low$country_region), 
            data=dane_mid_low)
coeftest(model_mid_low, vcov.=vcovHC(model_mid_low, type = "HC0"))
model_robust_mid_low = (coeftest(model_mid_low, vcov.=vcovHC(model_mid_low, type = "HC0")))

stargazer(model_robust_high, model_robust_mid_high, model_robust_mid_low, type="text", df=FALSE)
#stargazer(model_robust_high, model_robust_mid_high, model_robust_mid_low, type="html", df=FALSE, out="C:\\Users\\szymo\\Desktop\\star_income.doc")

###
# Testy

resettest(model_high, power=2:3, type="fitted")
resettest(model_mid_high, power=2:3, type="fitted")
resettest(model_mid_low, power=2:3, type="fitted")

bptest(model_high)
bptest(model_mid_high)
bptest(model_mid_low)

library("tseries")
jarque.bera.test(model1$residuals)
jarque.bera.test(model2$residuals)
jarque.bera.test(model3$residuals)
jarque.bera.test(model4$residuals)
jarque.bera.test(model_high$residuals)
jarque.bera.test(model_mid_high$residuals)
jarque.bera.test(model_mid_low$residuals)

library(rcompanion)
plotNormalHistogram(model_mid_low$residuals) # te wykresy musza sie znalezc w modelu

boxplot(model1$residuals)


plot(model1, which = 5)


###


unique(dane_mid_low$country_region)

###
mean(dane_high$srednia)
mean(dane_mid_high$srednia)
mean(dane_mid_low$srednia)

min(dane_high$srednia)
min(dane_mid_high$srednia)
min(dane_mid_low$srednia)

max(dane_high$srednia)
max(dane_mid_high$srednia)
max(dane_mid_low$srednia)