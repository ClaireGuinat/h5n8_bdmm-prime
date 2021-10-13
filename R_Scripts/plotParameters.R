# Load libraries ---------------------------------------------------------------
library(lubridate)
library(dplyr)
library(tidyverse)
library(circlize)
library(chorddiag)
library(dplyr)
library(ggplot2)
library(Hmisc)


# Source files -----------------------------------------------------------------
setwd("/Users/cguinat/Documents/Postdoc_ETHZurich/Project_O1/Analysis/Europe")
trace <- read.table("Analysis/Demes_WBDPc/BDMMPrime/Test_04052021/GLM_29072021combined/HAeraCombined.log", header = TRUE)


# Plot R0 ----------------------------------------------------------------------
name<-data.frame(name=c(rep("Period 1 (Oct-Nov 2016)", nrow(traceNew)), 
                        rep("Period 2 (Dec 2016)", nrow(traceNew)), 
                        rep("Period 3 (Jan 2017)", nrow(traceNew)),
                        rep("Period 4 (Feb-May 2017)", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
deme<-data.frame(deme=rep("Poultry farms in Czech Republic", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0SVEpi.i0_DPCzech_Republic)
value1<-data.frame(value=traceNew$R0SVEpi.i1_DPCzech_Republic)
value2<-data.frame(value=traceNew$R0SVEpi.i2_DPCzech_Republic)
value3<-data.frame(value=traceNew$R0SVEpi.i3_DPCzech_Republic)
value4<-data.frame(rlnorm(nrow(traceNew),0,1))
colnames(value4)[1] <- "value"
value=rbind(value0, value1, value2, value3, value4)
table_CR<-cbind(name,value,deme)
deme<-data.frame(deme=rep("Poultry farms in Germany", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0SVEpi.i0_DPGermany)
value1<-data.frame(value=traceNew$R0SVEpi.i1_DPGermany)
value2<-data.frame(value=traceNew$R0SVEpi.i2_DPGermany)
value3<-data.frame(value=traceNew$R0SVEpi.i3_DPGermany)
value=rbind(value0, value1, value2, value3, value4)
table_G<-cbind(name,value,deme)
deme<-data.frame(deme=rep("Poultry farms in Hungary", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0SVEpi.i0_DPHungary)
value1<-data.frame(value=traceNew$R0SVEpi.i1_DPHungary)
value2<-data.frame(value=traceNew$R0SVEpi.i2_DPHungary)
value3<-data.frame(value=traceNew$R0SVEpi.i3_DPHungary)
value=rbind(value0, value1, value2, value3, value4)
table_H<-cbind(name,value,deme)
deme<-data.frame(deme=rep("Poultry farms in Poland", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0SVEpi.i0_DPPoland)
value1<-data.frame(value=traceNew$R0SVEpi.i1_DPPoland)
value2<-data.frame(value=traceNew$R0SVEpi.i2_DPPoland)
value3<-data.frame(value=traceNew$R0SVEpi.i3_DPPoland)
value=rbind(value0, value1, value2, value3, value4)
table_P<-cbind(name,value,deme)
deme<-data.frame(deme=rep("Wild birds in the four countries", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0SVEpi.i0_WB)
value1<-data.frame(value=traceNew$R0SVEpi.i1_WB)
value2<-data.frame(value=traceNew$R0SVEpi.i2_WB)
value3<-data.frame(value=traceNew$R0SVEpi.i3_WB)
value=rbind(value0, value1, value2, value3, value4)
table_WB<-cbind(name,value,deme)
table=rbind(table_CR,table_G,table_H,table_P,table_WB)
str(table)
table$name<-as.factor(table$name)
table$deme<-as.factor(table$deme)

Re_Within <- ggplot(table, aes(name, value, fill=deme)) + 
  geom_violin(draw_quantiles=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color = "firebrick") +
  coord_cartesian(ylim=c(0,5)) +
  facet_wrap(~deme) +
  scale_fill_manual(values = c("#0066CC", "#00A087FF", "#99CC33", "#66CCFF", "#993300")) +
  labs(x="",y="Within-deme Re estimates") +
  labs(tag = "A") +
  theme_bw() +
  theme(legend.position="none",
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.tag = element_text(size = 25))

tiff("Results/Figure Re_within.tiff", width = 19.4, height = 18, units = "cm",
     compression = "lzw", res = 600)
Re_Within
dev.off()


# Plot Infectious period -------------------------------------------------------
name<-data.frame(name=c(rep("Poultry farms \nin Czech Republic", nrow(traceNew)), 
                        rep("Poultry farms \nin Germany", nrow(traceNew)), 
                        rep("Poultry farms \nin Hungary", nrow(traceNew)),
                        rep("Poultry farms \nin Poland", nrow(traceNew)),
                        rep("Wild birds \nin the four countries", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
value0<-data.frame(value=365.25/traceNew$becomeUninfectiousRateSVEpi.DPCzech_Republic)
value1<-data.frame(value=365.25/traceNew$becomeUninfectiousRateSVEpi.DPGermany)
value2<-data.frame(value=365.25/traceNew$becomeUninfectiousRateSVEpi.DPHungary)
value3<-data.frame(value=365.25/traceNew$becomeUninfectiousRateSVEpi.DPPoland)
value4<-data.frame(value=365.25/traceNew$becomeUninfectiousRateSVEpi.WB)
value5<-data.frame(365.25/rlnorm(nrow(traceNew), log(52), 0.6))
colnames(value5)[1] <- "value"
value=rbind(value0, value1, value2, value3, value4, value5)
table<-cbind(name,value)
str(table)
table$name<-as.factor(table$name)
table$name<-factor(table$name, levels=c("Poultry farms \nin Czech Republic", 
                                          "Poultry farms \nin Germany",
                                          "Poultry farms \nin Hungary",
                                          "Poultry farms \nin Poland",
                                          "Wild birds \nin the four countries",
                                          "Prior"))

Inf_period <- ggplot(table, aes(name, value, fill=name)) + 
  geom_violin(draw_quantiles=0.5) +
  coord_cartesian(ylim=c(0,50)) +
  scale_fill_manual(values = c("#0066CC", "#00A087FF", "#99CC33", "#66CCFF", "#993300", "grey")) +
  labs(x="",y="Infectious period (days)") +
  theme_bw() +
  theme(legend.position="none",
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1))

tiff("Results/Figure Infectious_period.tiff", width = 19.4, height = 18, units = "cm",
     compression = "lzw", res = 600)
Inf_period
dev.off()


# Plot R0AmongDemes -------------------------------------------------------
name<-data.frame(name=c(rep("From poultry farms \nin Germany", nrow(traceNew)), 
                        rep("From poultry farms \nin Hungary", nrow(traceNew)), 
                        rep("From poultry farms \nin Poland", nrow(traceNew)),
                        rep("From wild birds \nin the four countries", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
deme<-data.frame(deme=rep("To poultry farms in Czech Republic", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPGermany_to_DPCzech_Republic)
value1<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPHungary_to_DPCzech_Republic)
value2<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPPoland_to_DPCzech_Republic)
value3<-data.frame(value=traceNew$R0AmongDemesSMEpi.WB_to_DPCzech_Republic)
value4<-data.frame(rlnorm(nrow(traceNew),0,1))
colnames(value4)[1] <- "value"
value=rbind(value0, value1, value2, value3, value4)
table_CR<-cbind(name,value,deme)
name<-data.frame(name=c(rep("From poultry farms \nin Czech Republic", nrow(traceNew)), 
                        rep("From poultry farms \nin Hungary", nrow(traceNew)), 
                        rep("From poultry farms \nin Poland", nrow(traceNew)),
                        rep("From wild birds \nin the four countries", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
deme<-data.frame(deme=rep("To poultry farms in Germany", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPCzech_Republic_to_DPGermany)
value1<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPHungary_to_DPGermany)
value2<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPPoland_to_DPGermany)
value3<-data.frame(value=traceNew$R0AmongDemesSMEpi.WB_to_DPGermany)
value=rbind(value0, value1, value2, value3, value4)
table_G<-cbind(name,value,deme)
name<-data.frame(name=c(rep("From poultry farms \nin Czech Republic", nrow(traceNew)), 
                        rep("From poultry farms \nin Germany", nrow(traceNew)), 
                        rep("From poultry farms \nin Poland", nrow(traceNew)),
                        rep("From wild birds \nin the four countries", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
deme<-data.frame(deme=rep("To poultry farms in Hungary", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPCzech_Republic_to_DPHungary)
value1<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPGermany_to_DPHungary)
value2<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPPoland_to_DPHungary)
value3<-data.frame(value=traceNew$R0AmongDemesSMEpi.WB_to_DPHungary)
value=rbind(value0, value1, value2, value3, value4)
table_H<-cbind(name,value,deme)
name<-data.frame(name=c(rep("From poultry farms \nin Czech Republic", nrow(traceNew)), 
                        rep("From poultry farms \nin Germany", nrow(traceNew)), 
                        rep("From poultry farms \nin Hungary", nrow(traceNew)),
                        rep("From wild birds \nin the four countries", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
deme<-data.frame(deme=rep("To poultry farms in Poland", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPCzech_Republic_to_DPPoland)
value1<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPGermany_to_DPPoland)
value2<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPHungary_to_DPPoland)
value3<-data.frame(value=traceNew$R0AmongDemesSMEpi.WB_to_DPPoland)
value=rbind(value0, value1, value2, value3, value4)
table_P<-cbind(name,value,deme)
name<-data.frame(name=c(rep("From poultry farms \nin Czech Republic", nrow(traceNew)), 
                        rep("From poultry farms \nin Germany", nrow(traceNew)), 
                        rep("From poultry farms \nin Hungary", nrow(traceNew)),
                        rep("From poultry farms \nin Poland", nrow(traceNew)),
                        rep("Prior", nrow(traceNew))))
deme<-data.frame(deme=rep("To wild birds in the four countries", nrow(traceNew)*5))
value0<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPCzech_Republic_to_WB)
value1<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPGermany_to_WB)
value2<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPHungary_to_WB)
value3<-data.frame(value=traceNew$R0AmongDemesSMEpi.DPPoland_to_WB)
value=rbind(value0, value1, value2, value3, value4)
table_WB<-cbind(name,value,deme)
table=rbind(table_CR,table_G,table_H,table_P,table_WB)
str(table)
table$name<-as.factor(table$name)
table$deme<-as.factor(table$deme)

Re_Between <- ggplot(table, aes(name, value, fill=deme)) + 
  geom_violin(draw_quantiles=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color = "firebrick") +
  coord_cartesian(ylim = c(0, 5)) +
  facet_wrap(~deme, scales = 'free_x') +
  scale_fill_manual(values = c("#0066CC", "#00A087FF", "#99CC33", "#66CCFF", "#993300"), drop=TRUE) +
  labs(x="",y="Between-deme Re estimates") +
  labs(tag = "B") +
  theme_bw() +
  theme(legend.position="none",
        axis.text = element_text(size = 8),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.tag = element_text(size = 25))

tiff("Results/Figure Re_between.tiff", width = 19.4, height = 18, units = "cm",
     compression = "lzw", res = 600)
Re_Between
dev.off()


# Plot predictors -------------------------------------------------------
dat = data.frame(x=c("live poultry trade", 
                     "distance between centroids", 
                     "poultry density in source deme", 
                     "poultry density in destination deme", 
                     "poultry farm density in source deme", 
                     "poultry farm density in destination deme",
                     "farm outbreak density in source deme", 
                     "farm outbreak density in destination deme"),
                 y=c(length(which(traceNew[,"indicatorParamGLM2.1"]!=0))/length(traceNew$indicatorParamGLM2.1),
                       length(which(traceNew[,"indicatorParamGLM2.2"]!=0))/length(traceNew$indicatorParamGLM2.2),
                       length(which(traceNew[,"indicatorParamGLM2.3"]!=0))/length(traceNew$indicatorParamGLM2.3),
                       length(which(traceNew[,"indicatorParamGLM2.4"]!=0))/length(traceNew$indicatorParamGLM2.4),
                       length(which(traceNew[,"indicatorParamGLM2.5"]!=0))/length(traceNew$indicatorParamGLM2.5),
                       length(which(traceNew[,"indicatorParamGLM2.6"]!=0))/length(traceNew$indicatorParamGLM2.6),
                       length(which(traceNew[,"indicatorParamGLM2.7"]!=0))/length(traceNew$indicatorParamGLM2.7),
                       length(which(traceNew[,"indicatorParamGLM2.8"]!=0))/length(traceNew$indicatorParamGLM2.8)),
                 direction =c(
                   median(traceNew$scalerParamGLM2.1)>0,
                   median(traceNew$scalerParamGLM2.2)>0,
                   median(traceNew$scalerParamGLM2.3)>0,
                   median(traceNew$scalerParamGLM2.4)>0,
                   median(traceNew$scalerParamGLM2.5)>0,
                   median(traceNew$scalerParamGLM2.6)>0,
                   median(traceNew$scalerParamGLM2.7)>0,
                   median(traceNew$scalerParamGLM2.8)>0))
 
indicators <- ggplot(dat) + geom_bar(aes(x=x,y=y), fill="gray60", stat="identity", width=0.8, colour="gray40") +
  xlab("") +
  geom_hline(yintercept = 0.8, color="firebrick", linetype="dashed") +
  annotate(geom="text", x = 0.6, y = 0.90, label = "BF=3.2", color="firebrick",) +
  ylab("Inclusion probability") +
  ggtitle("Predictors of Re between farms across borders") +
  scale_x_discrete(limits = c("farm outbreak density in destination deme",
                              "farm outbreak density in source deme",
                              "poultry farm density in destination deme",
                              "poultry farm density in source deme",
                              "poultry density in destination deme",
                              "poultry density in source deme",
                              "distance between centroids",
                              "live poultry trade")) + 
  guides(fill = FALSE) +
  labs(tag = "A") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        plot.tag = element_text(size = 25)) +
  coord_flip(ylim = c(0,1)) 
 
tiff("Results/Figure Indicator.tiff", width = 19.4, height = 18, units = "cm",
     compression = "lzw", res = 600)
indicators
dev.off()

#select only rows with indicator 1 
pred.1 <- traceNew %>% 
  filter(indicatorParamGLM2.1!=0) 
pred.2 <- traceNew %>%
  filter(indicatorParamGLM2.2!=0)
pred.3 <- traceNew %>%
  filter(indicatorParamGLM2.3!=0)
pred.4 <- traceNew %>%
  filter(indicatorParamGLM2.4!=0)
pred.5 <- traceNew %>%
  filter(indicatorParamGLM2.5!=0)
pred.6 <- traceNew %>%
  filter(indicatorParamGLM2.6!=0)
pred.7 <- traceNew %>%
  filter(indicatorParamGLM2.7!=0)
pred.8 <- traceNew %>%
  filter(indicatorParamGLM2.8!=0)

dat = data.frame(name=c(rep("live poultry trade", nrow(pred.1)),
                      rep("distance between centroids", nrow(pred.2)),
                      rep("poultry density in source deme", nrow(pred.3)),
                      rep("poultry density in destination deme", nrow(pred.4)),
                      rep("poultry farm density in source deme", nrow(pred.5)),
                      rep("poultry farm density in destination deme", nrow(pred.6)),
                      rep("farm outbreak density in source deme", nrow(pred.7)),
                      rep("farm outbreak density in destination deme", nrow(pred.8))),
                      value=c(pred.1$scalerParamGLM2.1,
                         pred.2$scalerParamGLM2.2,
                         pred.3$scalerParamGLM2.3,
                         pred.4$scalerParamGLM2.4,
                         pred.5$scalerParamGLM2.5,
                         pred.6$scalerParamGLM2.6,
                         pred.7$scalerParamGLM2.7,
                         pred.8$scalerParamGLM2.8))

coef <- ggplot(dat, aes(name, value, fill=name)) + 
  geom_violin(draw_quantiles=0.5) +
  geom_hline(yintercept = 0, color="black", linetype="dashed") + 
  scale_x_discrete(limits = c("farm outbreak density in destination deme",
                              "farm outbreak density in source deme",
                              "poultry farm density in destination deme",
                              "poultry farm density in source deme",
                              "poultry density in destination deme",
                              "poultry density in source deme",
                              "distance between centroids",
                              "live poultry trade")) + 
  scale_fill_manual(values = c("#66CCCC", "#99CCCC", "#669999", "#003366", "#0066CC", "#336699", "#6666FF", "#3399FF")) +
  coord_flip() +
  ggtitle("Predictors of Re between farms across borders") +
  xlab("") +
  ylab("Log conditional effect size") + 
  labs(tag = "B") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        plot.tag = element_text(size = 25)) +
  guides(fill = FALSE)  

tiff("Results/Figure Coef.tiff", width = 19.4, height = 18, units = "cm",
     compression = "lzw", res = 600)
coef
dev.off()





