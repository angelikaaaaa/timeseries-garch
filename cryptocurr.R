#Instalacja bibliotek i pakietów
install.packages('rvest')
install.packages("car")
install.packages("FinTS")
install.packages("fGarch")
install.packages("rugarch")
install.packages("tidyverse")
install.packages("ggthemes")
install.packages('tidyverse')
install.packages('tseries')
library(tseries)
library(tidyverse)
library(ggplot2)
library(rvest)
library(xts)
library(fBasics) 
library(tseries)
library(car) 
library(FinTS)
library(fGarch) 
library(rugarch) 
library(ggthemes)

#Kod tworz¹cy funkcjê do pobierania danych (zaczerpniêty z internetu, lekko zmodyfikowany)
getdata = function(crypto='bitcoin', start_date='2010-01-01', end_date='2021-06-08'){
  webpage = read_html(paste0("https://www.coingecko.com/en/coins/",crypto,
                             "/historical_data/usd?start_date=", start_date,"&end_date=",end_date,"#panel"))
  tbls = html_nodes(webpage, "tr")
  table = tbls %>% html_text()
  rows = gsub(',,',',',gsub(' ','',gsub('.{1}$','\n',gsub('\n',',',gsub('\n\n',',',gsub('\n\n\n',',',
                                                                                        gsub('\\$','',gsub(',', '' ,table))))))))
  to_df = paste(rows, collapse = '')
  df = read.csv(text=to_df)
  df = df[-1,]
  df$Close = as.numeric(df$Close)
  return (df) 
}

#Wczytywanie danych (wybranych kryptowalut)
algorand = getdata('algorand')
stellar = getdata('stellar')
ontology = getdata('ontology')
kadena = getdata('kadena')

#Wyœwietlanie danych, ¿eby zobaczyæ od kiedy s¹ dostêpne
View(algorand) #dane dostêpne od 2019-06-21
View(stellar) #dane dostêpne od 2014-08-06
View(ontology) #dane dostêpne od 2018-03-23
View(kadena) #pe³ne dane (marketcap) od 22.04.2020

#Redukujcja danych do tego samego okresu wzgledem kryptowaluty kadena
algorand = algorand[1:412,]
stellar = stellar[1:412,]
ontology = ontology[1:412,]
kadena = kadena[1:412,]
#Wysz³o nam po 412 obserwacji, okres od 22.04.2020 do 07.06.2021

#Histogramy cen zamkniêcia w analizowanym okresie
hist(algorand$Close, breaks = 25, col = 'pink', main = 'Algorand: Histogram ceny zamkniêcia', xlab = 'Cena zamkniêcia')
hist(stellar$Close, breaks = 25, col = 'pink', main = 'Stellar: Histogram ceny zamkniêcia', xlab = 'Cena zamkniêcia')
hist(kadena$Close, breaks = 25, col = 'pink', main = 'Kadena: Histogram ceny zamkniêcia', xlab = 'Cena zamkniêcia')
hist(ontology$Close, breaks = 25, col = 'pink', main = 'Ontology: Histogram ceny zamkniêcia', xlab = 'Cena zamkniêcia')

#Wykresy cen zamkniêcia wzglêdem daty w badanym okresie
ggplot(algorand, aes(Date, Close)) +
  geom_point(color="pink", size=3) + labs(title = "Algorand: Ceny zamkniêcia")

ggplot(stellar, aes(Date, Close)) +
  geom_point(color="pink", size=3) + labs(title = "Stellar: Ceny zamkniêcia")

ggplot(kadena, aes(Date, Close)) +
  geom_point(color="pink", size=3) + labs(title = "Kadena: Ceny zamkniêcia")

ggplot(ontology, aes(Date, Close)) +
  geom_point(color="pink", size=3) + labs(title = "Ontology: Ceny zamkniêcia")

#################################################################################################
#Proces generowania zwrotów
#laczenie danych
portfel = merge.data.frame(algorand, stellar, by='Date')
colnames(portfel)[2:9]=c("MarketCap_algorand", "Volume_algorand", "Open_algorand", "Close_algorand","MarketCap_stellar", "Volume_stellar", "Open_stellar", "Close_stellar")
portfel = merge.data.frame(portfel, kadena, by='Date')
colnames(portfel)[10:13]=c("MarketCap_kadena", "Volume_kadena", "Open_kadena", "Close_kadena")
portfel = merge.data.frame(portfel, ontology, by='Date')
colnames(portfel)[14:17]=c("MarketCap_ontology", "Volume_ontology", "Open_ontology", "Close_ontology")
View(portfel)


#4 waluty na 1 wykresie 
ggplot(portfel, aes(Date))  +
  geom_point(aes(y = Close_algorand), color='red') +
  geom_point(aes(y = Close_stellar), color='blue') +
  geom_point(aes(y = Close_kadena), color='darkgreen') +
  geom_point(aes(y = Close_ontology)) 

#TWORZENIE PORTFELA

#1. Sumujê MarketCap poszczególnych walut, bo potrzebne mi to do policzenia udzia³ow (i dodajê do bazy danych)
portfel$sumaMarketCap = (portfel$MarketCap_algorand+portfel$MarketCap_stellar+portfel$MarketCap_kadena+portfel$MarketCap_ontology)

#2. Liczê udzia³
portfel$udzial_algorand = portfel$MarketCap_algorand/portfel$sumaMarketCap
portfel$udzial_stellar = portfel$MarketCap_stellar/portfel$sumaMarketCap
portfel$udzial_kadena = portfel$MarketCap_kadena/portfel$sumaMarketCap
portfel$udzial_ontology = portfel$MarketCap_ontology/portfel$sumaMarketCap

#3. Licze zwroty poszczegolnych krypto
portfel$x_algorand = diff.xts(log(portfel$Close_algorand))
portfel$x_stellar = diff.xts(log(portfel$Close_stellar))
portfel$x_kadena = diff.xts(log(portfel$Close_kadena))
portfel$x_ontology = diff.xts(log(portfel$Close_ontology))

#4. Waze zwroty udzialem i sumuje, z czego powstaje portfel
portfel$portfelkryptowalut = portfel$udzial_algorand*portfel$x_algorand +
  portfel$udzial_stellar*portfel$x_stellar+
  portfel$udzial_kadena*portfel$x_kadena+
  portfel$udzial_ontology*portfel$x_ontology

zwroty = portfel$portfelkryptowalut #zwroty z portfela kryptowalut
czas = portfel$Date #data
dane = data.frame(czas,zwroty)
view(dane)


#WYKRES ZWROTÓW
ggplot(dane, aes(czas, zwroty)) +
  geom_point(color="black", size=1) + labs(title = "Zwroty (USD) z portfela")


#KOD Z ZAJÊÆ 7
# wykres wartoÅ›ci ACF dla zwrotÃ³w
acf(zwroty, lag.max = 36, na.action = na.pass,
    ylim = c(-0.05, 0.2),
    col = "black", lwd = 7,
    main = "ACF zwrotów z portfela kryptowalut")

# wykres wartoÅ›ci ACF dla kwadratÃ³w zwrotÃ³w
acf(zwroty^2, lag.max = 36, na.action = na.pass,
    ylim = c(-0.05, 0.7), 
    col = "black", lwd = 7,
    main = "ACF kwadratóww zwrotów z portfela kryptowalut")

#STATYSTYKI OPISOWE
basicStats(zwroty)

#histogram
hist(zwroty, prob = T, breaks = 100)
curve(dnorm(x,
            mean = mean(zwroty, na.rm = T),
            sd = sd(zwroty, na.rm = T)),
      col = "black", lwd = 2, add = TRUE)

# wykres quantile-quantile
qqnorm(zwroty)
qqline(zwroty, col = 2)

#STATYSTYKI TESTOWE
#jarque bera
jarque.bera.test(na.omit(zwroty))

#  statystykÄ™ Durbina-Watsona
durbinWatsonTest(lm(formula = zwroty ~ 1),
                 max.lag = 8)
# arch
ArchTest(zwroty, lags = 8)

# statystyka durbina dla kwadratow zwrotow
durbinWatsonTest(lm(formula = zwroty^ 2 ~ 1),
                 max.lag = 8)
#Ljunga boxa
Box.test(zwroty, lag = 8, type = 'Ljung-Box')
#Ljunga boxa dla kwadratow zwrotow
Box.test(zwroty^2, lag = 8, type = 'Ljung-Box')

#Test Dickey Fullera
adf.test(na.omit(zwroty))

###############################################
#Podzial próby na in-sample i out-of-sample
IN = dane[1:340,] #in-sample
OUT = dane[341:412,] #out-of-sample
view(IN$zwroty)

###############################################
#GARCH
#USTALAM PARAMETRY MODELU GARCH

#(1,1)
summary(garchFit(formula = ~garch(1, 1), 
                 data = na.omit(IN$zwroty), 
                 cond.dist = "norm", 
                 include.mean = F, 
                 trace = FALSE))

#(1,2)
summary(garchFit(formula = ~ garch(1, 2),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "norm", 
                 trace = FALSE))
#(2,1)
summary(garchFit(formula = ~ garch(2, 1),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "norm", 
                 trace = FALSE))

#(1,3)

summary(garchFit(formula = ~ garch(1, 3),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "norm", 
                 trace = FALSE))

#(1,4)
summary(garchFit(formula = ~ garch(1, 4),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "norm", 
                 trace = FALSE))

#(1,1)
summary(garchFit(formula = ~garch(1, 1), 
                 data = na.omit(IN$zwroty), 
                 cond.dist = "std", 
                 include.mean = F, 
                 trace = FALSE))

#(1,2)
summary(garchFit(formula = ~ garch(1, 2),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "std", 
                 trace = FALSE))
#(2,1)
summary(garchFit(formula = ~ garch(2, 1),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "std", 
                 trace = FALSE))

#(1,3)

summary(garchFit(formula = ~ garch(1, 3),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "std", 
                 trace = FALSE))

#(1,4)
summary(garchFit(formula = ~ garch(1, 4),
                 data = na.omit(IN$zwroty),
                 include.mean = F,
                 cond.dist = "std", 
                 trace = FALSE))

#wybrano garch(1,1)


# specyfikacja modelu
spec <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = F),
  distribution.model = "norm") 

#wywo³anie modelu garch(1,1)
garch11 <- ugarchfit(spec=spec, data=na.omit(IN$zwroty))
garch11

plot(garch11, which=12) #news impact curve

#funkcja warunkowej wariancji
plot(garch11@fit$sigma, type="l", xlab="Czas", ylab="Wartoœæ", main="GARCH (1,1): Wariancja warunkowa in-sample")

#Statystyki odnoœnie reszt modelu
jarque.bera.test(na.omit(garch11@fit$residuals))
durbinWatsonTest(lm(formula = (garch11@fit$residuals)^2 ~ 1),
                 max.lag = 8)

##############################################
# Model TGARCH 
###############################################

#Mniej wiêcej podobnie jak w przypadku poprzedniego modelu GARCH przebiegal proces wyboru parametrow w t-garch
#testowano rozklad t-studenta, wlaczenie stalej, jednak nie przynioslo to pozadanego efektu

# specyfikacja modelu TGARCH
spec <- ugarchspec(
  variance.model = list(model = "fGARCH",
                        garchOrder = c(1, 1),
                        submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = F),
  distribution.model = "norm")


tgarch11 <- ugarchfit(spec = spec, data = na.omit(IN$zwroty))
tgarch11

#news impact curve
plot(tgarch11, which=12) 

#Warunkowej wariancji funckja
plot(tgarch11@fit$sigma, type="l", xlab="Czas", ylab="Wartoœæ", main="TGARCH (1,1): Wariancja warunkowa in-sample")


#Statystyki odnoœnie reszt modelu
jarque.bera.test(na.omit(tgarch11@fit$residuals))
durbinWatsonTest(lm(formula = (tgarch11@fit$residuals)^2 ~ 1),
                 max.lag = 8)

#szacowanie wartosci nara¿onej na ryzyko w 3 modelach (garch, tgarch 1,0 i tgarch 1,1) 
IN$d = (IN$zwroty-mean(IN$zwroty, na.rm=T))/sd(IN$zwroty, na.rm = T)
q01 = quantile(IN$d, 0.01, na.rm = T)

f = ugarchforecast(garch11, n.ahead = 1)
f1 = f@forecast$sigmaFor[1, 1]
IN$v1 = q01*f1
#Szacowanie wartoœci nara¿onej na ryzyko w modelu GARCH(1,1) IN SAMPLE
v1

g = ugarchforecast(tgarch10, n.ahead = 1)
g1 = g@forecast$sigmaFor[1, 1]
IN$v2 = q01*g1
#Szacowanie wartoœci nara¿onej na ryzyko w modelu TGARCH(1,0) IN SAMPLE
v2

u = ugarchforecast(tgarch11, n.ahead = 1)
u1 = u@forecast$sigmaFor[1, 1]
IN$v2 = q01*u1
#Szacowanie wartoœci nara¿onej na ryzyko w modelu TGARCH(1,1) IN SAMPLE
v2


###########################################

# Prognozowanie warunkowej wariancji
####################################

#GARCH(1,1)
# szacowanie modelu
k.garch11 <- garchFit(formula = ~ garch(1, 1),
                      data = na.omit(IN$zwroty),
                      include.mean = F,
                      cond.dist = "norm",
                      trace = F)

# wyniki
summary(k.garch11)

# struktura wynikowego obiektu
str(k.garch11)

#####################
#Ponizszy kod (czesciowo z zajec)

###############
uncond_variance1 <- k.garch11@fit$matcoef[1] / (1 - k.garch11@fit$matcoef[2]
                                      - k.garch11@fit$matcoef[3])
names(uncond_variance1) <- "unconditional variance"
uncond_variance1

# prognozÄ™ warunkowej wariancji na nastÄ™pne 72 okresy
k.fore72 <- predict(k.garch11, n.ahead = 72)
head(k.fore72)

plot(k.fore72[, 3] ^ 2, type = "l")
abline(h = uncond_variance1, col = "red", lty = 2)
title(main = "GARCH(11): Warunkowa i bezwarunkowa wariancja zwrotów out-of-sample")


#TGARCH(1,0)

spec <- ugarchspec(
  variance.model = list(model = "fGARCH",
                        garchOrder = c(1, 0),
                        submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = F),
  distribution.model = "norm")

k.tgarch10 <- ugarchfit(spec = spec, data = na.omit(IN$zwroty))


uncond_variance2 <- k.tgarch10@fit$matcoef[1] / (1 - k.tgarch10@fit$matcoef[2]
                                           - k.tgarch10@fit$matcoef[3])
names(uncond_variance2) <- "wariancja bezwarunkowa dla tgarch(1,0) out of sample"
uncond_variance2

k.fore72.2 <- ugarchforecast(k.tgarch10, n.ahead = 72)
head(k.fore72.2@forecast$sigmaFor)


# wykres oszacowañ i prognoz warunkowej wariancji w d³ugim okresie
plot(k.fore72.2@forecast$sigmaFor ^ 2, type = "l")
abline(h = uncond_variance2, col = "red", lty = 2) 
title(main = "TGARCH(1,0): Warunkowa wariancja zwrotów out-of-sample")

#TGARCH(1,1)

spec <- ugarchspec(
  variance.model = list(model = "fGARCH",
                        garchOrder = c(1, 1),
                        submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0, 0),
                    include.mean = F),
  distribution.model = "norm")

k.tgarch11 <- ugarchfit(spec = spec, data = na.omit(IN$zwroty))

uncond_variance3 <- k.tgarch11@fit$matcoef[1] / (1 - k.tgarch11@fit$matcoef[2]
                                            - k.tgarch11@fit$matcoef[3])
names(uncond_variance3) <- "wariancja bezwarunkowa dla tgarch(1,1) out of sample"
uncond_variance3


k.fore72.3 <- ugarchforecast(k.tgarch11, n.ahead = 72)
head(k.fore72.3@forecast$sigmaFor)

plot(k.fore72.3@forecast$sigmaFor ^ 2, type = "l")

abline(h = uncond_variance3, col = "green", lty = 2)
title(main = "TGARCH(1,1): Warunkowa wariancja zwrotów out-of-sample")
