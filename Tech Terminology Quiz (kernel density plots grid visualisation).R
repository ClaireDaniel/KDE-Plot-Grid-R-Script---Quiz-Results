#R Script - PlanTech Quiz Results

#Load required libraries
require(ggplot2)
require(grid)
require(gridExtra)

#set working directory
dir <- "C:/Users" #Change to filepath for where you have saved the data
setwd(dir)

#Load data
# Each column of the demo dataset has been randomised individually
# whilst the overall distribution remains correct, the rows do not correspond to an individual's response
data <- read.csv("PlanTech Quiz Data (random sort).csv") #Set to file path for your 

plots <- c()

#Loop through each column of the input data and create a plot
i = 1
for (col in colnames(data)){

term <- gsub("\\.", " ", col) #Extract title of plot from column name and fix formatting

score <- round(mean(data[,col]), 1) #Calculate average score to insert line

#Create the plot
plots[[i]] <-ggplot(data, aes_string(col)) +
    theme(plot.title = element_text(colour = "grey39", face = "bold", size = 50),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 30, face = "bold", vjust=(3)),
          panel.background = element_rect(fill = 'white'),
          plot.margin = unit(c(2,1,2,1), "cm")) +
    scale_x_continuous("Familiarity", breaks=c(1, score ,7), labels = c("1",as.character(score), "7"), limits=c(1,7)) +
    scale_y_continuous(name = "# Planners") +
    ggtitle(paste0("  ", term)) +
    geom_density(fill = 'red', size = 1, colour = 'red3', alpha =0.6) +
    geom_vline(xintercept = score, size = 1, colour = "darkred", linetype = "dashed")

i = i + 1
}

# Use the grid.arrange function to arrange plots on page
title = textGrob("Urban Planners' Familiarity with Tech Terminology", hjust = 0.56, gp=gpar(fontface="bold", fontsize=80, col = "grey39"))
note = textGrob("Scale from 1 (never heard of) to 7 (know well and could explain). \n Results based on 16 responses. Survey ran from 16 - 24 March 2019, promoted at PIA Congress 2019 on the Gold Coast", hjust = 0.56, gp=gpar(fontface="bold", fontsize=40, col = "grey39"))
g <- grid.arrange(grobs = plots, ncol = 4, top = title, bottom = note)

ggsave("poster.png", g, width = 80, height = 100, units = "cm",
       dpi = 300, limitsize = TRUE)
