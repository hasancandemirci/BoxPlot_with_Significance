
################theKaplanLab#############

#################Hasan Can Demirci############

#################Cell_Length with Box plot by Emphasizing the Significance #####



############### Necessary Libraries ###########

library(readxl)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(dplyr)
library(forcats)
library(colorspace)
library(ggpubr) #to add significance


                ##### PART 1 : Creation of the Box Plot ######

##################### Set your working area ####################################

setwd("C:\\Users\\Hasan Can Demirci\\Desktop\\R_projects")

####################### Uploading the File #####################################

data <- read_xlsx("data.xlsx")
#data$`yyy` <- as.double(data$`yyy`)
#data$`xxx` <- as.double(data$`xxx`)
#data$`zzz` <- as.double(data$`zzz` )

###################### Rearrange the data for Box plot with Pivot_longer #######
###################### Filter and Mutate data to make sense of the values ######
###################### With Mutate : values are made as numeric#################
###################### With Filter: values, between %1-%99, are counted ########
colnames(data)

data <- data %>%
     pivot_longer(
      cols = c("yyy","xxx" , "zzz"),
        names_to = "trashing",
      values_to = "speed",
     values_drop_na = TRUE) %>%
  mutate(speed = as.numeric(speed)) %>%
   filter(speed >= quantile(speed, 0.01) & speed <= quantile(speed, 0.99))

###################### Calling Box plot without Significance ###################
###################### "group" function: is necessary for clear plot ###########

data$trashing <- factor(data$trashing, levels = c("yyy" ,  "xxx" , "zzz")) #determine order of X members

ggplot(data, aes(x=trashing, y=speed, group=trashing)) +
  geom_boxplot(fill="#CCFFFF", notch=FALSE, outlier.shape = NA) +
  geom_jitter(size=1, color="black", width=0.1) +
  theme_ipsum() +
  ylim(0,6)

##################### PART2 for adding Significance ############################

#####################  Determination of Comparisons ############################

my_comparisons <- list(c("yyy","xxx"), c("yyy","zzz" ))



##################### Add pairwise comparisons p-value #########################

p <- ggboxplot(data, x = "trashing", y = "speed",
               palette = c("#FFCCCC", "#FFFFCC", "#CCFFE5","#CCFFFF"),
               add = "boxplot", add.params = list(fill = "white"))

p + stat_compare_means(comparisons = my_comparisons)+
  stat_compare_means(label.y = 50)

########################## Violin plot with Significance #######################
######################## Note: Colors' value should be the same number of
                                                          # the x -axis values#

ggviolin(data, x = "trashing", y = "speed", fill = "trashing",
         palette = c("#FFCCCC", "#FFFFCC", "#CCFFE5","#CCFFFF", "red"),
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+
  stat_compare_means(label.y = 20)

#######################   Box plot with Significance ###########################

ggboxplot(data, x = "trashing", y = "speed", fill = "trashing",
         palette = c("#FFCCCC", "#FFFFCC", "#CCFFFF"),
         add = "boxplot", add.params = list(fill = rep(c("#FFCCCC", "#FFFFCC", "#CCFFFF")))) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+
  stat_compare_means(label.y = 60)


        #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
