
#Libraries
library(tidyverse)
library(tidyselect)
library(ggplot2)
library(soundecology)
library(tuneR)
library(seewave)
library(nlme)
library(dplyr)
library(mefa)
library(plotly)




setwd("D:/RA-withSangermano/Miles_and_Shreena/CSV")
# DF<- read.csv("ALL_INDICES.csv")

#Your Choice whether to read in the data from a csv or use the made datafram
DF <- DF_all_cbind
DF <- read.csv("DF_Fulltable_CBIND.csv")


#In the following code we use lmlist to run the regressions given we have data points taken at different buffers.
#In order to prevent grouping of the data for the calculation of the standard error, deviation, t statistics, and pvalues,
#we use pool: an optional logical value indicating whether a pooled estimate of the residual standard error should be used
#in calculations of standard deviations or standard errors for summaries.

#ACI
lm_ACI_NDVI<- lmList(ACI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
ACI_NDVI_lm <- summary(lm_ACI_NDVI)$r.squared

lm_ACI_NDVI



lm_ACI_NDBI<- lmList(ACI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
ACI_NDBI_lm <- summary(lm_ACI_NDBI)$r.squared

ACI_NDVI_lm

#ADI
lm_ADI_NDVI<- lmList(ADI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
ADI_NDVI_lm <- summary(lm_ADI_NDVI)$r.squared

lm_ADI_NDVI


lm_ADI_NDBI<- lmList(ADI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
ADI_NDBI_lm <- summary(lm_ADI_NDBI)$r.squared

ADI_NDVI_lm


#AEI

lm_AEI_NDVI<- lmList(AEI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
summary(lm_AEI_NDVI)

AEI_NDVI_lm <- summary(lm_AEI_NDVI)$r.squared

lm_AEI_NDVI


lm_AEI_NDBI<- lmList(AEI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
AEI_NDBI_lm <- summary(lm_AEI_NDBI)$r.squared

AEI_NDBI_lm

plot(lm_AEI_NDVI, resid(., type = "pool") ~ fitted(.) | Buffer, abline = 0, id = 0.05)





#BI

lm_BI_NDVI<- lmList(BI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
BI_NDVI_lm <- summary(lm_BI_NDVI)$r.squared

lm_BI_NDVI


lm_BI_NDBI<- lmList(BI_select ~ MEAN_NDBI | Buffer, data = DF, pool = FALSE)
BI_NDBI_lm <- summary(lm_BI_NDBI)$r.squared

BI_NDBI_lm


#plot(m1$residuals ~ mlb11$at_bats)



#----------------------------------------------------------------------------------------------------------
#NDSI
lm_NDSI_NDVI <- lmList(NDSI_select ~ MEAN_NDVI | Buffer, data = DF, pool = FALSE)
NDSI_NDVI_lm <- summary(lm_NDSI_NDVI)$r.squared

lm_NDSI_NDVI
NDSI_NDVI_lm

cor(x = DF$NDSI_select, y = DF$MEAN_NDVI)

#Obtain numbers for NDVI at Buffer 2000
cor_2000_NDVI <- lm_NDSI_NDVI[["2000"]][["model"]][["MEAN_NDVI"]]
cor_2000_NDSI <- lm_NDSI_NDVI[["2000"]][["model"]][["NDSI_select"]]

cor(x = cor_2000_NDSI, y = cor_2000_NDVI)

lm_NDSI_NDBI<- lmList(NDSI_select ~ MEAN_NDBI | Buffer, data= DF, pool = FALSE)
NDSI_NDBI_lm<-summary(lm_NDSI_NDBI)$r.squared

cor(x = DF$NDSI_select, y = DF$MEAN_NDBI)


#Obtain numbers for NDBI at Buffer 2000
cor_2000_NDBI <- lm_NDSI_NDBI[["2000"]][["model"]][["MEAN_NDBI"]]
cor(x = cor_2000_NDSI, y = cor_2000_NDBI)


#---------------------------------------------------------------------------------------------------------
#Biophony

lm_NDSI_NDBIO_NDVI<- lmList(BIO_select ~ MEAN_NDVI | Buffer, data= DF, pool = FALSE)
BIO_NDVI_lm<-summary(lm_NDSI_NDBIO_NDVI)$r.squared

lm_NDSI_NDBIO_NDVI


cor(x = DF$BIO_select, y = DF$MEAN_NDVI)


#Obtain data for NDBIO at Buffer 2000
cor_2000_NDBIO <- lm_NDSI_NDBIO_NDVI[["2000"]][["model"]][["BIO_select"]]

cor(x = cor_2000_NDBIO, y = cor_2000_NDVI)


lm_NDSI_NDBIO_NDBI <- lmList(BIO_select ~ MEAN_NDBI | Buffer, data= DF, pool = FALSE)
BIO_NDBI_lm<-summary(lm_NDSI_NDBIO_NDBI)$r.squared

lm_NDSI_NDBIO_NDBI

cor(x = DF$BIO_select, y = DF$MEAN_NDBI)

#Correlation between NDBIO and NDBI at 2000 BUffer

cor(x = cor_2000_NDBIO, y = cor_2000_NDBI)

#-----------------------------------------------------------------------------------------------------------------
#Anthrophony

lm_ANT_NDVI<- lmList(ANT_select ~ MEAN_NDVI | Buffer, data= DF, pool = FALSE)
ANT_NDVI_lm <- summary(lm_ANT_NDVI)$r.squared

lm_ANT_NDVI

cor(x = DF$ANT_select, y = DF$MEAN_NDVI)


#Obtain data for ANT at Buffer 2000
cor_2000_ANT <- lm_ANT_NDVI[["2000"]][["model"]][["ANT_select"]]
cor(x = cor_2000_ANT, y = cor_2000_NDVI)



lm_ANT_NDBI<- lmList(ANT_select ~ MEAN_NDBI | Buffer, data= DF, pool = FALSE)
ANT_NDBI_lm<-summary(lm_ANT_NDBI)$r.squared

lm_ANT_NDBI

cor(x = DF$ANT_select, y = DF$MEAN_NDBI)

#Correlation between ANT and NDBI at 2500 BUffer

cor(x = cor_2000_ANT, y = cor_2000_NDBI)



buffername <- c(500, 1000,1500,2000,2500,3000)

linear_rsquared <- data.frame(buffername, ACI_NDVI_lm, ACI_NDBI_lm, ADI_NDVI_lm, ADI_NDBI_lm, AEI_NDVI_lm, AEI_NDBI_lm, BI_NDVI_lm, BI_NDBI_lm, NDSI_NDVI_lm, NDSI_NDBI_lm, BIO_NDVI_lm, BIO_NDBI_lm, ANT_NDVI_lm, ANT_NDBI_lm)


#-------------------------------------------------------------------------------------------------------------------------
#Graphs


#graph of R^2 for NDVI


RSquaredPlot_NDVI <- ggplot(data = linear_rsquared) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$ACI_NDVI_lm, color = 'ACI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$ADI_NDVI_lm, color = 'ADI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$AEI_NDVI_lm, color = 'AEI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$BI_NDVI_lm, color = 'BI')) +
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$NDSI_NDVI_lm, color = 'NDSI'))+
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$BIO_NDVI_lm, color = 'BIOPHONY'))+
  geom_line(aes(x=linear_rsquared$buffername, y=linear_rsquared$ANT_NDVI_lm, color = 'ANTHRO')) +
  labs(title = "Mean NDVI ~ R squared of metrics",x ="Focal Distance (meters)", y = "R squared of metrics")

ggplotly(RSquaredPlot_NDVI)



#graph of R^2 for NDBI

RSquaredPlot_NDBI <- ggplot(data = linear_rsquared) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ACI_NDBI_lm, color = 'ACI'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ADI_NDBI_lm, color = 'ADI')) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$AEI_NDBI_lm, color = 'AEI')) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$BI_NDBI_lm, color = 'BI')) +
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$NDSI_NDBI_lm, color = 'NDSI'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$BIO_NDBI_lm, color = 'BIOPHONY'))+
  geom_line (aes(x=linear_rsquared$buffername, y=linear_rsquared$ANT_NDBI_lm, color = 'ANT')) + scale_x_log10() +
  labs(title = "Mean NDBI ~ R squared of metrics",x ="Focal Distance (meters)", y = "R squared of metrics")


#interactive GGPlot NDBI
ggplotly(RSquaredPlot_NDBI)


#graph of R^2 using facet wrap by buffer


#-------------------------------------------------------------------------------------------------------------------------
#Graphs for NDVI vs Indices


#Graphs of metric vs NDVI/AEI

ggplot(data = DF, aes (x = MEAN_NDVI, y = AEI_select, group = Buffer, color = factor(Buffer))) +
  # geom_line(data = fortify(fit), aes(x = MEAN_NDVI, y = .fitted)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="AEI", x = "NDVI")



ggplot(data = DF, aes(x = MEAN_NDVI, y = AEI_select, group=Buffer, color = factor(Buffer))) +
  # geom_line(data = fortify(fit), aes(x = MEAN_NDVI, y = .fitted)) +
  geom_smooth(method = "lm") +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  geom_line(data = fortify(fit), aes(x = MEAN_NDVI, y = .fitted)) +
  theme(legend.position = "right") +
  labs(y="AEI", x = "NDVI") + geom_abline()


#Graphs of metric vs NDVI/ACI

ggplot(data = DF, aes(x = MEAN_NDVI, y = ACI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ACI", x = "NDVI") + geom_abline()

#NDVI vs Biophony (BIO_SELECT)
ggplot(data = DF, aes (x = MEAN_NDVI, y = BIO_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="BIO_select", x = "NDVI")

#NDVI vs Anthrophony (ANT_SELECT)
ggplot(data = DF, aes (x = MEAN_NDVI, y = ANT_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ANT_select", x = "NDVI")


#NDVI vs NDSI Facets wrap
corr_2000_NDSI <- ggplot(data = DF, aes (x = MEAN_NDVI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDVI") +
  facet_wrap("Buffer")



lm_NDSI_NDBIO_NDVI[["2000"]][["model"]]


ggscatter(my_data, x = "mpg", y = "wt",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")


#NDVI vs NDSI Non-facets warp

ggplot(data = DF, aes (x = MEAN_NDVI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDVI")


#-------------------------------------------------------------------------------------------------------------------------
#Graphs for NDBI vs Indices


#graph of metric vs NDBI/ACI
#NDVI
ggplot(data = DF, aes (x = MEAN_NDBI, y = ACI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ACI", x = "NDBI")

#Graphs of metric vs NDSI/NDBI

#NDBI vs Biophony (BIO_SELECT)
ggplot(data = DF, aes (x = MEAN_NDBI, y = BIO_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="BIO_select", x = "NDBI")

#NDBI vs Anthrophony (ANT_SELECT)
ggplot(data = DF, aes (x = MEAN_NDBI, y = ANT_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="ANT_select", x = "NDBI")


#NDBI vs NDSI Facets Wrap
ggplot(data = DF, aes (x = MEAN_NDBI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDBI") +
  facet_wrap("Buffer")

#NDBI vs NDSI Non-facets warp

ggplot(data = DF, aes (x = MEAN_NDBI, y = NDSI_select, group=Buffer, color = factor(Buffer))) +
  geom_smooth(method = "lm", se=F) +
  geom_point(alpha=0) +
  geom_point(aes(color = factor(Buffer)))+
  theme(legend.position = "right") +
  labs(y="NDSI", x = "NDBI")
