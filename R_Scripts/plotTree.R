# Load libraries ---------------------------------------------------------------
library(ggtree)
library(ggplot2)
library(treeio)
library(dplyr)


# Source files -----------------------------------------------------------------
setwd("/Users/cguinat/Documents/Postdoc_ETHZurich/Project_O1/Analysis/Europe")


# Data files -----------------------------------------------------------------
beast <- read.beast("Analysis/Demes_WBDPc/BDMMPrime/Test_04052021_Final/Mapper_GLM_29072021Sub_Final/Mapper_GLM_29072021Sub/HAera.trajectoryMapper_GLM_29072021Sub.summary.trees")

tree_figure <- ggtree(
  tr = beast, 
  mrsd="2017-05-09", 
  as.Date = T,
  aes(color = type)) +
  theme_tree2() +
  geom_vline(xintercept = as.Date(c("2016-08-01","2016-09-01","2016-10-01","2016-11-01","2016-12-01","2017-01-01","2017-02-01","2017-03-01","2017-04-01","2017-05-01")), color="grey", alpha=0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b, %Y") 
show(tree_figure)

tree_figure <- tree_figure + 
   scale_color_manual(values = c("DPCzech_Republic" = "#003366", 
                                "DPGermany" = "#00A087FF", 
                                "DPHungary" = "#99CC33", 
                                "DPPoland" = "#66CCFF", 
                                "WB" = "#993300",
                                "DPCzech_Republic+WB" = "grey",
                                "DPPoland+DPCzech_Republic" = "grey"), 
                     breaks=c("DPCzech_Republic", "DPGermany", "DPHungary", "DPPoland", "WB"),
                     labels = c("DPCzech_Republic" = "Poultry farms in Czech Republic",
                                "DPGermany" = "Poultry farms in Germany",
                                "DPHungary" = "Poultry farms in Hungary",
                                "DPPoland" = "Poultry farms in Poland",
                                "WB" = "Wild birds in the four countries"),
                     name = "Most probable deme type") +
  theme(axis.text = element_text(size = 8),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))  
show(tree_figure)

