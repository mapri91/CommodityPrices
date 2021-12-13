### Seminararbeit SS19
### Bearbeiter: Matthias Priehs (ID: 454225)
### Thema: Spekulanten und ihr Einfluss auf Rohstoffpreise

## 1. Datensatz laden/anpassen und benötigte Pakete beziehen
# Pakete laden
library(tseries) # FÜr ADF Test
library(lmtest) # FÜr Test auf Autokorrelation
library(sjPlot) # Für HTML Outputs der Regression
library(papeR) # FÜr deskriptive Statistiken
library(ggplot2)

# Verzeichnis wechseln
setwd("H:/Google Drive/Uni/Uni Münster/Module/Seminar Empirische Aspekte von Rohstoffmärkten/R")

# Datensatz laden
x <- as.data.frame(read.csv2("data_new.csv"))

# Anpassungen
x$date <- as.character(x$date)
x$date <- as.Date(x$date, format = "%d.%m.%Y")
attach(x)

## 2. Spekulationsindizes bilden
# Working's T
# Weizen
work_t_w <- 1+ifelse(((coc_com_long+0.5*coc_nrep_long) > (coc_com_short+0.5*coc_nrep_short)),
                     (coc_ncom_long+0.5*coc_nrep_long)/(coc_com_long+coc_com_short+0.5*(coc_nrep_long+coc_nrep_short)),
                     (coc_ncom_short+0.5*coc_nrep_short)/(coc_com_long+coc_com_short+0.5*(coc_nrep_long+coc_nrep_short)))
share_spec_w <- (wheat_ncom_long+wheat_ncom_short+0.5*(wheat_nrep_long+wheat_nrep_short))/(2*(wheat_com_long+wheat_ncom_long+wheat_nrep_long))
  
# Mais
work_t_corn <- 1+ifelse(((corn_com_long+0.5*corn_nrep_long) > (corn_com_short+0.5*corn_nrep_short)),
                     (corn_ncom_long+0.5*corn_nrep_long)/(corn_com_long+corn_com_short+0.5*(corn_nrep_long+corn_nrep_short)),
                     (corn_ncom_short+0.5*corn_nrep_short)/(corn_com_long+corn_com_short+0.5*(corn_nrep_long+corn_nrep_short)))
share_spec_corn <- (corn_ncom_long+corn_ncom_short+0.5*(corn_nrep_long+corn_nrep_short))/(2*(corn_com_long+corn_ncom_long+corn_nrep_long))

# Kaffee
work_t_coff <- 1+ifelse(((coff_com_long+0.5*coff_nrep_long) > (coff_com_short+0.5*coff_nrep_short)),
                        (coff_ncom_long+0.5*coff_nrep_long)/(coff_com_long+coff_com_short+0.5*(coff_nrep_long+coff_nrep_short)),
                        (coff_ncom_short+0.5*coff_nrep_short)/(coff_com_long+coff_com_short+0.5*(coff_nrep_long+coff_nrep_short)))
share_spec_coff <- (coff_ncom_long+coff_ncom_short+0.5*(coff_nrep_long+coff_nrep_short))/(2*(coff_com_long+coff_ncom_long+coff_nrep_long))

# Kakao
work_t_coc <- 1+ifelse(((coc_com_long+0.5*coc_nrep_long) > (coc_com_short+0.5*coc_nrep_short)),
                        (coc_ncom_long+0.5*coc_nrep_long)/(coc_com_long+coc_com_short+0.5*(coc_nrep_long+coc_nrep_short)),
                        (coc_ncom_short+0.5*coc_nrep_short)/(coc_com_long+coc_com_short+0.5*(coc_nrep_long+coc_nrep_short)))
share_spec_coc <- (coc_ncom_long+coc_ncom_short+0.5*(coc_nrep_long+coc_nrep_short))/(2*(coc_com_long+coc_ncom_long+coc_nrep_long))

## 3. Log-returns bilden
log_r_wheat <- diff(log(x$wheat_fut))
log_r_corn <- diff(log(x$corn_fut))
log_r_coff <- diff(log(x$coffee_fut))
log_r_coc <- diff(log(x$cocoa_fut))
log_r_sp500 <- diff(log(x$s.p_500_index))
log_r_oil <- diff(log(x$light_crude_oil_fut))
log_r_usd <- diff(log(x$usd_index))

## 4. Finalen Datensatz erstellen
# Auffüllen der Log-Reihen mit 0 als erster Wert, da sonst unterschiedliche Längen der Vektoren
log_r_wheat <- append(log_r_wheat,0,0)
log_r_corn <- append(log_r_corn,0,0)
log_r_coff <- append(log_r_coff,0,0)
log_r_coc <- append(log_r_coc,0,0)
log_r_sp500 <- append(log_r_sp500,0,0)
log_r_oil <- append(log_r_oil,0,0)
log_r_usd <- append(log_r_usd,0,0)

# Hinzufügen der erstellten Variablen zu Anfangsdatensatz
x <- cbind(x,work_t_w,work_t_corn,work_t_coff,work_t_coc,share_spec_w,share_spec_corn,
           share_spec_coff,share_spec_coc,log_r_wheat,log_r_corn,log_r_coff,log_r_coc,
           log_r_sp500,log_r_oil,log_r_usd)

## 5. Deskriptive Statistiken
sum_indices <- summarize(x, type = "numeric", variables = c("work_t_w","work_t_corn","work_t_coff","work_t_coc",
                                                            "share_spec_w","share_spec_corn","share_spec_coff","share_spec_coc"),
                         variable.labels = c("T Weizen","T Mais","T Kaffee","T Kakao","Weizen","Mais","Kaffee","Kakao"))

cor_indices <- cor(x[,c(33:40)])

# Übertragung zu Word
write.table(sum_indices, file = "indices.txt")
write.table(cor_indices, file = "cor_indices.txt")

## 6. Stationaritätstest
# Test aller Zeitreihen auf Stationarität
adf.test(wheat_fut) # nicht-stationär
adf.test(corn_fut) # nicht-stationär
adf.test(coffee_fut) # nicht-stationär
adf.test(cocoa_fut) # nicht-stationär
adf.test(s.p_500_index) # nicht-stationär
adf.test(light_crude_oil_fut) # nicht-stationär
adf.test(usd_index) # nicht stationär

adf.test(log_r_wheat, k=0) # stationär
adf.test(log_r_corn, k=0) # stationär
adf.test(log_r_coff, k=0) # stationär
adf.test(log_r_coc, k=0) # statonär
adf.test(log_r_sp500, k=0) # stationär
adf.test(log_r_oil, k=0) # stationär
adf.test(log_r_usd, k=0) # stationär

adf.test(work_t_w, k=0) # stationär
adf.test(work_t_corn, k=0) # stationär
adf.test(work_t_coff, k=0) # stationär
adf.test(work_t_coc, k=0) # stationär

adf.test(share_spec_w, k=0) # stationär
adf.test(share_spec_corn, k=0) # stationär
adf.test(share_spec_coff, k=0) # stationär
adf.test(share_spec_coc, k=0) # stationär

## 7. Modellschätzung
# Schätzung für Anteil Spekulanten
ols1 <- lm(log_r_wheat ~ log_r_usd + log_r_sp500 + log_r_oil + share_spec_w)
ols2 <- lm(log_r_corn ~ log_r_usd + log_r_sp500 + log_r_oil + share_spec_corn)
ols3 <- lm(log_r_coff ~  + log_r_usd + log_r_sp500 + log_r_oil + share_spec_coff)
ols4 <- lm(log_r_coc ~ log_r_usd + log_r_sp500 + log_r_oil + share_spec_coc)

# Output mit Marktanteil Spekulanten
tab_model(ols1,ols2,ols3,ols4, show.ci = FALSE, p.style = "asterisk",
          dv.labels = c("Weizen","Mais","Kaffee","Kakao"),
          string.pred = "Regressoren",
          string.est = "Schaetzer",
          pred.labels = c("(Konstante)","usd_index","s$p500","rohoel","Anteil_Spek_Weizen","Anteil_Spek_Mais","Anteil_Spek_Kaffee","Anteil_Spek_Kakao"))

# Schätzung für Working's T
ols5 <- lm(log_r_wheat ~ log_r_usd + log_r_sp500 + log_r_oil + work_t_w)
ols6 <- lm(log_r_corn ~ log_r_usd + log_r_sp500 + log_r_oil + work_t_corn)
ols7 <- lm(log_r_coff ~  + log_r_usd + log_r_sp500 + log_r_oil + work_t_coff)
ols8 <- lm(log_r_coc ~ log_r_usd + log_r_sp500 + log_r_oil + work_t_coc)

# Output mit Working's T
tab_model(ols5,ols6,ols7,ols8, show.ci = FALSE, p.style = "asterisk",
          dv.labels = c("Weizen","Mais","Kaffee","Kakao"),
          string.pred = "Regressoren",
          string.est = "Schaetzer",
          pred.labels = c("(Konstante)","usd_index","s$p500","rohoel","T_Index_Weizen","T_Index_Mais","T_Index_Kaffee","T_Index_Kakao"))

### Grafiken
# OI und Preise, indexiert
avg_price <- (wheat_fut+corn_fut+coffee_fut+cocoa_fut)/4
avg_oi_total <- (wheat_com_long+wheat_ncom_long+wheat_nrep_long+corn_com_long+corn_ncom_long+corn_nrep_long+
  coff_com_long+coff_ncom_long+coff_nrep_long+coc_com_long+coc_ncom_long+coc_nrep_long)/4
avg_price_ind <- avg_price[2:length(avg_price)]/avg_price[1]*100
avg_oi_total_ind <- avg_oi_total[2:length(avg_oi_total)]/avg_oi_total[1]*100
date_ind <- date[2:length(date)]

plot(date_ind,avg_oi_total_ind,type="l",col="red",xlab="Jahr",ylab="Index")
lines(date_ind,avg_price_ind,col="blue")
legend("bottomright")
