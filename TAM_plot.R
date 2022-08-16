
# likeability and expectation analysis
library(reshape2)
library(ggplot2)
library(gridExtra)
library(stringr)
library(ggpubr)
library(readr)
library(dplyr)
library(magrittr)


boxplot(tam_data[,c("pu", "peou", "at", "bi", "c", "e")])



min_out <- tam_data[,c("stid", "pu", "peou", "at", "bi", "c", "e")] %>% slice_min(c)
#max_out <- tam_data[,c("stid", "pu", "peou", "at", "bi", "c", "e")] %>% slice_max(c)
no_outlier_ce <- tam_data %>% filter((!stid %in% min_out[,c('stid')]$stid) )
#no_outlier_ce <- tam_data %>% filter( (!stid %in% max_out[,c('stid')]$stid))
no_outlier_ce[,c("stid", "pu", "peou", "at", "bi", "c", "e")] %>% slice_min(c)
boxplot(no_outlier_ce[,c("pu", "peou", "at", "bi", "c", "e")])


library("vioplot")
vioplot(tam_data$pu)
hist(tam_data$pu)
#------------

tam_long = melt(data = tam_data, id.vars = c("stid"), measure.vars = c(3:23))
unique(tam_long$variable)
# 1, Extremely disagree
# 2, Disagree
# 3, Somewhat disagree
# 4, Neither agree nor disagree
# 5, Somewhat agree
# 6, Agree
# 7, Extremely agree
tam_long$vlabel <- 
  ifelse(tam_long$value == 1 , "Extremely disagree",
         ifelse(tam_long$value == 2 , "Disagree",
                ifelse(tam_long$value == 3 , "Somewhat disagree",
                       ifelse(tam_long$value == 4 , "Neither agree nor disagree",
                              ifelse(tam_long$value == 5 , "Somewhat agree",
                                     ifelse(tam_long$value == 6 , "Agree",
                                            ifelse(tam_long$value == 7 , "Extremely agree", "")))))))
tam_long

tam_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), n = n(), median = median(value), min = min(value), max = max(value), sd = sd(value))

tam_long_Summ <- tam_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
tam_long_Summ
tam_long_Summ$variable <- reorder(tam_long_Summ$variable, (tam_long_Summ$freq * tam_long_Summ$count), mean)
#likenExpSumm$count <- ifelse(likenExpSumm$count > 2, likenExpSumm$count, "")
tam_long_Summ

puCol <- c("Q1","Q2","Q3","Q4","Q5")
pu_long <- subset(tam_long, variable %in% puCol)
pu_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), min = min(value), max = max(value), sd = sd(value))
pu_long
pu_longSumm <- pu_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
pu_longSumm2 <- pu_long %>%
  group_by(vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
pu_longSumm
pu_longSumm$variable <- reorder(pu_longSumm$variable, (pu_longSumm$freq * pu_longSumm$count), mean)
pu_longSumm$count <- ifelse(pu_longSumm$count > 2, pu_longSumm$count, "")
pu_longSumm

peouCol <- c('Q6','Q7','Q8','Q9')
peou_long <- subset(tam_long, variable %in% peouCol)
peou_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), min = min(value), max = max(value), sd = sd(value))
peou_long
peou_longSumm <- peou_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
peou_longSumm
peou_longSumm$variable <- reorder(peou_longSumm$variable, 
                                  (peou_longSumm$freq * peou_longSumm$count), 
                                  mean)
peou_longSumm$count <- ifelse(peou_longSumm$count > 2, peou_longSumm$count, "")
peou_longSumm


atCol <- c('Q10','Q11','Q12','Q13')
at_long <- subset(tam_long, variable %in% atCol)
at_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), min = min(value), max = max(value), sd = sd(value))
at_long
at_longSumm <- at_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
at_longSumm
at_longSumm$variable <- reorder(at_longSumm$variable, 
                                (at_longSumm$freq * at_longSumm$count), 
                                mean)
at_longSumm$count <- ifelse(at_longSumm$count > 2, at_longSumm$count, "")
at_longSumm

biCol <- c('Q14','Q15')
bi_long <- subset(tam_long, variable %in% biCol)
bi_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), min = min(value), max = max(value), sd = sd(value))
bi_long
bi_longSumm <- bi_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
bi_longSumm
bi_longSumm$variable <- reorder(bi_longSumm$variable, 
                                (bi_longSumm$freq * bi_longSumm$count), 
                                mean)
bi_longSumm$count <- ifelse(bi_longSumm$count > 2, bi_longSumm$count, "")
bi_longSumm

cCol <- c('Q16','Q17','Q18')
c_long <- subset(tam_long, variable %in% cCol)
c_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), min = min(value), max = max(value), sd = sd(value))
c_long
c_longSumm <- c_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
c_longSumm
c_longSumm$variable <- reorder(c_longSumm$variable, 
                               (c_longSumm$freq * c_longSumm$count), 
                               mean)
c_longSumm$count <- ifelse(c_longSumm$count > 2, c_longSumm$count, "")
c_longSumm

eCol <- c('Q19','Q20', 'Q21')
e_long <- subset(tam_long, variable %in% eCol)
e_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value), min = min(value), max = max(value), sd = sd(value))
e_long
e_longSumm <- e_long %>%
  group_by(variable, vlabel) %>%
  summarize(count = length(stid), mean = mean(value)) %>%
  mutate(freq = count / sum(count)) %>%
  mutate(textColor = case_when(
    vlabel == "Strongly Disagree" ~ "#000000",
    TRUE ~ "#ffffff"
  ))
e_longSumm
e_longSumm$variable <- reorder(e_longSumm$variable, 
                               (e_longSumm$freq * e_longSumm$count), 
                               mean)
e_longSumm$count <- ifelse(e_longSumm$count > 2, e_longSumm$count, "")
e_longSumm

myColors <- c("#024070","#278FDF", "#68AFE5",
              "#a3d0ff",
              "#ffbaba",
              "#ff5252", "#7c0000")

#myColors <- c("#000000", "#333333", "#666666", "#888888","#BBBBBB", "#CCCCCC")
#myColors <- c("#000000", "#333333", "#666666", "#888888","#aaaaaa", "#dddddd")
tam_long_Summ$variable <- factor(tam_long_Summ$variable, levels=c("Q21", "Q20", "Q19", "Q18", "Q17", "Q16", "Q15", 
                                                                  "Q14", "Q13", "Q12", "Q11", "Q10", 
                                                                  "Q9", "Q8", "Q7", "Q6", "Q5", 
                                                                  "Q4", "Q3", "Q2", "Q1"
))

#tam_long_Summ$variable <- factor(tam_long_Summ$variable, ordered = T)
#paste(sort((levels(tam_long_Summ$variable))))

plot <- ggplot(tam_long_Summ, 
               aes(fill = factor(vlabel,
                                 levels = c("Extremely agree", 
                                            "Agree", 
                                            "Somewhat agree", 
                                            "Neither agree nor disagree", 
                                            "Somewhat disagree", 
                                            "Disagree", 
                                            "Extremely disagree")),
                   y=freq, x=variable, label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = tam_long_Summ$textColor) +
  ylab("Percentage") +
  xlab("All TAM") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10))
  ) +
  coord_flip()
plot


pu_longSumm
pu_longSumm$vlabel
plotpu <- ggplot(pu_longSumm, 
                 aes(fill = factor(vlabel,
                                   levels = c("Extremely agree", 
                                              "Agree", 
                                              "Somewhat agree", 
                                              "Neither agree nor disagree", 
                                              "Somewhat disagree", 
                                              "Disagree", 
                                              "Extremely disagree")),
                     y=freq, x=factor(variable, levels = c("Q5", "Q4", "Q3", "Q2", "Q1")), label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = pu_longSumm$textColor) +
  ggtitle("Perceived Usefulness") +
  ylab("Percentage") +
  xlab("PU") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  #theme(axis.title.y = element_text(margin = margin(r = 10)),
  #      axis.title.x = element_text(margin = margin(t = 10))
  #) +
  theme(#axis.title.x=element_blank(),
    axis.title.y=element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))  + coord_flip()

plotpu

plotLegend <- get_legend(plotpu)
plotpu <- plotpu + theme(legend.position="none")
plotLegend


peou_longSumm
peou_longSumm$vlabel
plotpeou <- ggplot(peou_longSumm, 
                   aes(fill = factor(vlabel,
                                     levels = c("Extremely agree", 
                                                "Agree", 
                                                "Somewhat agree", 
                                                "Neither agree nor disagree", 
                                                "Somewhat disagree", 
                                                "Disagree", 
                                                "Extremely disagree")),
                       y=freq, x=factor(variable, levels = c("Q9", "Q8", "Q7", "Q6")), label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = peou_longSumm$textColor) +
  ggtitle("Perceived Ease of Use") +
  ylab("Percentage") +
  xlab("peou") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  #theme(axis.title.y = element_text(margin = margin(r = 10)),
  #      axis.title.x = element_text(margin = margin(t = 10))
  #) +
  theme(#axis.title.x=element_blank(),
    axis.title.y=element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))  + coord_flip()

plotpeou

plotLegend <- get_legend(plotpeou)
plotpeou <- plotpeou + theme(legend.position="none")
plotLegend

at_longSumm
at_longSumm$vlabel
plotat <- ggplot(at_longSumm, 
                 aes(fill = factor(vlabel,
                                   levels = c("Extremely agree", 
                                              "Agree", 
                                              "Somewhat agree", 
                                              "Neither agree nor disagree", 
                                              "Somewhat disagree", 
                                              "Disagree", 
                                              "Extremely disagree")),
                     y=freq, x=factor(variable, levels = c("Q13", "Q12", "Q11", "Q10")), label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = at_longSumm$textColor) +
  ggtitle("Attitude toward using") +
  ylab("Percentage") +
  xlab("at") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  #theme(axis.title.y = element_text(margin = margin(r = 10)),
  #      axis.title.x = element_text(margin = margin(t = 10))
  #) +
  theme(#axis.title.x=element_blank(),
    axis.title.y=element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))  + coord_flip()

plotat

plotLegend <- get_legend(plotat)
plotat <- plotat + theme(legend.position="none")
plotLegend


bi_longSumm
bi_longSumm$vlabel
plotbi <- ggplot(bi_longSumm, 
                 aes(fill = factor(vlabel,
                                   levels = c("Extremely agree", 
                                              "Agree", 
                                              "Somewhat agree", 
                                              "Neither agree nor disagree", 
                                              "Somewhat disagree", 
                                              "Disagree", 
                                              "Extremely disagree")),
                     y=freq, x=factor(variable, levels = c("Q15", "Q14")), label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = bi_longSumm$textColor) +
  ggtitle("Behavioral Intention") +
  ylab("Percentage") +
  xlab("bi") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  #theme(axis.title.y = element_text(margin = margin(r = 10)),
  #      axis.title.x = element_text(margin = margin(t = 10))
  #) +
  theme(#axis.title.x=element_blank(),
    axis.title.y=element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))  + coord_flip()

plotbi

plotLegend <- get_legend(plotbi)
plotbi <- plotbi + theme(legend.position="none")
plotLegend


#####

c_longSumm
c_longSumm$vlabel
plotc <- ggplot(c_longSumm, 
                aes(fill = factor(vlabel,
                                  levels = c("Extremely agree", 
                                             "Agree", 
                                             "Somewhat agree", 
                                             "Neither agree nor disagree", 
                                             "Somewhat disagree", 
                                             "Disagree", 
                                             "Extremely disagree")),
                    y=freq, x=factor(variable, levels = c("Q18", "Q17", "Q16")), label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = c_longSumm$textColor) +
  ggtitle("Compatibility") +
  ylab("Percentage") +
  xlab("c") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  #theme(axis.title.y = element_text(margin = margin(r = 10)),
  #      axis.title.x = element_text(margin = margin(t = 10))
  #) +
  theme(#axis.title.x=element_blank(),
    axis.title.y=element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))  + coord_flip()

plotc

plotLegend <- get_legend(plotc)
plotc <- plotc + theme(legend.position="none")
plotLegend

#####

e_longSumm
e_longSumm$vlabel
plote <- ggplot(e_longSumm, 
                aes(fill = factor(vlabel,
                                  levels = c("Extremely agree", 
                                             "Agree", 
                                             "Somewhat agree", 
                                             "Neither agree nor disagree", 
                                             "Somewhat disagree", 
                                             "Disagree", 
                                             "Extremely disagree")),
                    y=freq, x=factor(variable, levels = c("Q21", "Q20", "Q19")), label = count)
) + 
  geom_bar(stat="identity", width = 0.9) +
  geom_text(size = 3.2, position = position_stack(vjust = 0.5), 
            color = e_longSumm$textColor) +
  ggtitle("Enjoyment") +
  ylab("Percentage") +
  xlab("e") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
  scale_fill_manual(name = "Agreement", values=myColors, drop=FALSE) +
  #theme(axis.title.y = element_text(margin = margin(r = 10)),
  #      axis.title.x = element_text(margin = margin(t = 10))
  #) +
  theme(#axis.title.x=element_blank(),
    axis.title.y=element_blank(), panel.background = element_rect(fill = 'white', colour = 'black'))  + coord_flip()

plote

plotLegend <- get_legend(plote)
plote <- plote + theme(legend.position="none")
plotLegend

plotAll <- ggarrange(plotpu, plotpeou, plotat, plotbi, plotc, plote, common.legend = TRUE, legend = "right")
annotate_figure(plotAll, left = "Questions")


puq1 <- pu_longSumm[pu_longSumm$variable=='Q1',]
puq1 %>% filter(vlabel %in% c('Agree', 'Extremely agree', 'Somewhat agree')) %>% mutate(sum = sum(freq))
puq1 %>% filter(vlabel %in% c('Disagree', 'Extremely disagree', 'Somewhat disagree')) %>% mutate(sum = sum(freq))
puq1 %>% filter(vlabel %in% c('Neither agree nor disagree')) %>% mutate(sum = sum(freq))

puq2 <- pu_longSumm[pu_longSumm$variable=='Q2',]
puq2 %>% filter(vlabel %in% c('Agree', 'Extremely agree', 'Somewhat agree')) %>% mutate(sum = sum(freq))
puq2 %>% filter(vlabel %in% c('Disagree', 'Extremely disagree', 'Somewhat disagree')) %>% mutate(sum = sum(freq))
puq2 %>% filter(vlabel %in% c('Neither agree nor disagree')) %>% mutate(sum = sum(freq))

puq3 <- pu_longSumm[pu_longSumm$variable=='Q3',]
puq3 %>% filter(vlabel %in% c('Agree', 'Extremely agree', 'Somewhat agree')) %>% mutate(sum = sum(freq))
puq3 %>% filter(vlabel %in% c('Disagree', 'Extremely disagree', 'Somewhat disagree')) %>% mutate(sum = sum(freq))
puq3 %>% filter(vlabel %in% c('Neither agree nor disagree')) %>% mutate(sum = sum(freq))

puq4 <- pu_longSumm[pu_longSumm$variable=='Q4',]
puq4 %>% filter(vlabel %in% c('Agree', 'Extremely agree', 'Somewhat agree')) %>% mutate(sum = sum(freq))
puq4 %>% filter(vlabel %in% c('Disagree', 'Extremely disagree', 'Somewhat disagree')) %>% mutate(sum = sum(freq))
puq4 %>% filter(vlabel %in% c('Neither agree nor disagree')) %>% mutate(sum = sum(freq))

puq5 <- pu_longSumm[pu_longSumm$variable=='Q5',]
puq5 %>% filter(vlabel %in% c('Agree', 'Extremely agree', 'Somewhat agree')) %>% mutate(sum = sum(freq))
puq5 %>% filter(vlabel %in% c('Disagree', 'Extremely disagree', 'Somewhat disagree')) %>% mutate(sum = sum(freq))
puq5 %>% filter(vlabel %in% c('Neither agree nor disagree')) %>% mutate(sum = sum(freq))


# Q1: 41 a, 29 D, 29 N
# Q2: 59 A, 20 D, 20 N
# Q3: 53 A, 14 D, 32 N
# Q4: 47 A, 23 D, 29 N
# Q5: 47 A, 18 D, 35 N

pu_longSumm %>% ungroup() %>% filter(vlabel %in% c('Agree', 'Extremely agree', 'Somewhat agree')) %>% mutate(sum = sum(freq)/5)
pu_longSumm %>% ungroup() %>% filter(vlabel %in% c('Disagree', 'Extremely disagree', 'Somewhat disagree')) %>% mutate(sum = sum(freq)/5)
pu_longSumm %>% ungroup() %>% filter(vlabel %in% c('Neither agree nor disagree')) %>% mutate(sum = sum(freq)/5)
pu_longSumm %>% ungroup() %>% mutate(sum = sum(freq))
