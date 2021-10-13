# Load libraries ---------------------------------------------------------------
library(yaml)
library(lubridate)
library(scales)
library(ggpubr)
library(ggsci)
library(ggridges)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag) 
library(ggforce)
library(maps)
library(sf)
library(wpp2019)
library(gridExtra)
library(grid)
library(HDInterval)


# Source files -----------------------------------------------------------------
setwd("/Users/cguinat/Documents/Postdoc_ETHZurich/Project_O1/Analysis/Europe")
source("Script/trajProcessing_EUR.R")

demes <- read.csv("Analysis/Demes_WBDPc/BDMMPrime/Test_04052021_Final/Mapper_GLM_29072021Sub_Final/Mapper_GLM_29072021Sub/demes.csv", stringsAsFactors = FALSE)
traj = "Analysis/Demes_WBDPc/BDMMPrime/Test_04052021_Final/Mapper_GLM_29072021Sub_Final/Mapper_GLM_29072021Sub/HAera.trajectoryMapper_GLM_29072021Sub.TL.traj"
empresi <- read.csv("Data/Metadata/Outbreak_list_deme.csv", sep=",") 


# Load trajectory data ---------------------------------------------------------
empresi$deme <- sub("Czech Republic", "Poultry farms in Czech Republic", empresi$deme)
empresi$deme <- sub("Germany", "Poultry farms in Germany", empresi$deme)
empresi$deme <- sub("Hungary", "Poultry farms in Hungary", empresi$deme)
empresi$deme <- sub("Poland", "Poultry farms in Poland", empresi$deme)
empresi$deme <- sub("Wild birds", "Wild birds in the four countries", empresi$deme)
empresi$deme=as.factor(empresi$deme)

Sys.setlocale("LC_TIME", "C") 
empresi$obsDate=as.Date(empresi$obsDate, format="%d.%m.%y")
summary(empresi$obsDate)

min_date <- min(empresi$obsDate)
max_date <- max(empresi$obsDate) 
empresi["value"]=1 
country_complete <- empresi %>%
  group_by(deme) %>%
  complete(obsDate = seq.Date(min_date, max_date, by = "day")) %>%
  replace_na(list(value = 0)) 

cum_empresi <- country_complete %>%  
  arrange(deme, obsDate) %>%
  mutate(cumvalue = cumsum(value))


# Load trajectory data ---------------------------------------------------------
filename <- traj
burninFrac <- 0
subsample <- 500

df <- loadTrajectories2(filename, burninFrac, subsample)
summary(as.factor(df$event))
head(df)


# Data wrangling ---------------------------------------------------------------
events <- df
demes <- demes
source <- "Wild birds in the four countries"
mrs <- ymd("2017-05-09")
df_traj <- processEvents(events, demes, source = NA, mrs)


## Timing of key first epidemic events------------------------------------------  
dcolors <- c("#003366", "#99CC33", "#66CCFF")
             
df_eventsttiming <- df_traj %>%
  filter(var %in% c("B", "IC", "OC"), value != 0) %>%
  group_by(traj, var, deme) %>% 
  mutate(var = ifelse(var == "O", "B", var)) %>%
  arrange(date) %>%
  slice(1) 

summary_df_events <- df_eventsttiming %>%
  group_by(var, deme) %>% 
  summarise(median_date = median(date),
            l95_date = date_decimal(hdi(decimal_date(round_date(date, unit="day")))[[1]]),
            h95_date = date_decimal(hdi(decimal_date(round_date(date, unit="day")))[[2]]))

first_empresi <- country_complete %>% 
  filter(value != 0) %>% 
  group_by(deme) %>% 
  arrange(obsDate) %>% 
  slice(1) %>%
  ungroup() 

events_timingplot <- ggplot(df_eventsttiming) +
  geom_density_ridges(aes(x = date, y = factor(var, level =  c("OC", "B", "IC")), fill = var), color="transparent", quantile_lines = TRUE, quantiles = c(0.025, 0.975), bandwidth=8, alpha = 0.7, size = 0.2) +  #
  geom_vline(data = first_empresi, aes(xintercept = obsDate, linetype = "Date of the first officially reported outbreak/case in each deme (source: empres-i)"), colour="firebrick") +
  scale_linetype_manual(values = 2, name = "") +
  facet_wrap(~deme, ncol=5) +
  scale_fill_manual(name = "", values = dcolors, guide = FALSE) +
  scale_y_discrete(breaks = c("IC", "B", "OC"), 
                   labels = c("Inferred date of the first \nimported outbreak/case", "Inferred date of the first \nlocal outbreak/case", "Inferred date of the first \nexported outbreak/case")) +
  scale_x_date(limits = c(ymd("2016-05-01"), ymd("2017-02-01")), date_breaks = "2 month", date_labels = "%b, %Y", expand = c(0,0)) +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(strip.text = element_text(size = 11),
         axis.text.y = element_text(size = 11),
         legend.text = element_text(size = 11),
         axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("") +
  xlab("")

tiff("Results/Figure 2.tiff", width = 35, height = 15, units = "cm",
     compression = "lzw", res = 600)
events_timingplot
dev.off()

 

## Cumulative number of active cases/outbreaks--------------------------------------------------------

traj_summary <- df_traj %>%
  group_by(var, deme, partner, date) %>%
  summarise(l95_value = HDInterval::hdi(value)[[1]],
            h95_value = HDInterval::hdi(value)[[2]],
            median_value = median(value),
            mean_value = mean(value),
            l95_cumvalue = HDInterval::hdi(cumvalue)[[1]],
            h95_cumvalue = HDInterval::hdi(cumvalue)[[2]],
            median_cumvalue = median(cumvalue),
            mean_cumvalue = mean(cumvalue)) %>% ungroup

dcolors <- c("#003366", "#00A087FF", "#99CC33", "#66CCFF", "#993300")
names(dcolors) <- sort(demes$deme)
dcolors1 <- c(dcolors, dash_color="firebrick")
dcolors2 <- c(dcolors, dash_color="transparent")

gribbon <- ggplot(traj_summary %>% filter(var == "D")) + 
  geom_ribbon(aes(date, ymin = l95_cumvalue, ymax = h95_cumvalue, fill = deme), alpha = 0.5) +
  geom_line(aes(date, median_cumvalue, colour = deme)) +
  geom_line(data = cum_empresi, aes(obsDate, cumvalue, color="dash_color"), linetype="dashed") +
  facet_wrap(~deme, ncol=5) +
  ylab("Cumulative number of no-longer infectious outbreaks/cases") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  theme(legend.position="bottom") +
  scale_x_date(limits = c(ymd("2016-06-01"), ymd("2017-05-09")), date_breaks = "2 month", date_labels = "%b, %Y", expand = c(0,0)) +
  scale_fill_manual(name = "", values = dcolors2, labels = c("dash_color" = "Cumulative number of officially reported outbreaks/cases in each deme (source: empres-i)",
                                                            "Poultry farms in Czech Republic" = "Inferred 95%HPD",
                                                            "Poultry farms in Germany" = "Inferred 95%HPD",
                                                            "Poultry farms in Hungary" = "Inferred 95%HPD",
                                                            "Poultry farms in Poland" = "Inferred 95%HPD",
                                                            "Wild birds in the four countries" = "Inferred 95%HPD")) +
  scale_color_manual(name = "", values = dcolors1, labels = c("dash_color" = "Cumulative number of officially reported outbreaks/cases in each deme (source: empres-i)",
                                                              "Poultry farms in Czech Republic" = "Inferred median",
                                                              "Poultry farms in Germany" = "Inferred median",
                                                              "Poultry farms in Hungary" = "Inferred median",
                                                              "Poultry farms in Poland" = "Inferred median",
                                                              "Wild birds in the four countries" = "Inferred median")) +
  
  theme_bw() +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  theme(strip.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("")


tiff("Results/Figure 3.tiff", width = 35, height = 15, units = "cm",
     compression = "lzw", res = 600)
gribbon
dev.off()

sum <- traj_summary %>% 
  filter(var == "D")


## outbreaks arising from different transmission events--------------------------------------------------------
# Five types of transmission in absolute numbers and per week
df_traj$week_date <- floor_date(df_traj$date, "week")

df_traj2 <- df_traj %>%
  group_by(traj, var, deme, partner, week_date)%>%
  summarise(value_w = sum(value), .groups = "drop_last") %>%
  arrange(traj, week_date, deme) %>%
  mutate(cumvalue_w = cumsum(value_w))

traj_summary2 <- df_traj2 %>%
  group_by(var, deme, partner, week_date) %>%
  summarise(l95_value_w = HDInterval::hdi(value_w)[[1]],
            h95_value_w = HDInterval::hdi(value_w)[[2]],
            median_value_w = median(value_w),
            l95_cumvalue = HDInterval::hdi(cumvalue_w)[[1]],
            h95_cumvalue = HDInterval::hdi(cumvalue_w)[[2]],
            mean_value_w = mean(value_w),
            median_cumvalue_w = median(cumvalue_w),
            mean_cumvalue_w = mean(cumvalue_w)) %>% ungroup

dcolors <- c("#003366", "#00A087FF", "#99CC33", "#66CCFF", "#993300","#CCCCFF")

destmig <- traj_summary2 %>%
  filter(var %in% c("B", "IC")) %>%
  mutate(varcolor = case_when(var == "B" ~ "Local transmission", 
                              var == "IC" & partner == "Poultry farms in Czech Republic" ~ "Imported transmission from poultry farms in Czech Republic",
                              var == "IC" & partner == "Poultry farms in Germany" ~ "Imported transmission from poultry farms in Germany",
                              var == "IC" & partner == "Poultry farms in Hungary" ~ "Imported transmission from poultry farms in Hungary",
                              var == "IC" & partner == "Poultry farms in Poland" ~ "Imported transmission from poultry farms in Poland",
                              var == "IC" & partner == "Wild birds in the four countries" ~ "Imported transmission from wild birds")) 

blank_data <- data.frame(deme = c("Poultry farms in Czech Republic", "Poultry farms in Czech Republic", 
                                  "Poultry farms in Germany", "Poultry farms in Germany", 
                                  "Poultry farms in Hungary", "Poultry farms in Hungary", 
                                  "Poultry farms in Poland", "Poultry farms in Poland", 
                                  "Wild birds in the four countries", "Wild birds in the four countries"), 
                         x = 0, 
                         y = c(0, 50, 0, 50, 0, 50, 0, 50, 0, 400))

destmig_bar <- destmig %>%
  ggplot(aes(x = week_date, y = median_value_w, fill = varcolor)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_date(limits = c(ymd("2016-08-01"), ymd("2017-05-09")), date_breaks = "1 month", date_labels = "%b, %Y", expand = c(0,0)) +  
  scale_fill_manual(values = dcolors) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  theme(strip.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Median number of outreaks/cases") +
  xlab("") +
  facet_wrap(~deme, ncol=5, scales="free")

tiff("Results/Figure 5.tiff", width = 35, height = 15, units = "cm",
     compression = "lzw", res = 600)
destmig_bar
dev.off()

