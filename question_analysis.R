library('readr')
library('dplyr')
library('tidyr')
require ('ggplot2')
library("ggpubr")

# run after running oop_prepost_analaysis.R
# 36 --> 34
length(question_data[question_data$group == 'cont',]$group)/20/2
# 42 --> 40
length(question_data[question_data$group == 'exp',]$group)/20/2


summary(EC_post$group)
summary(fdata$group)
# remove outliers
question_data <- question_data %>% filter(!userid %in% c('615','494', '563', '454'))

questions_wide <- question_data[,c('type',  'group', 'correct', 'qid')] %>% group_by(group, type, qid) %>% 
  summarise(ans=sum(correct), .groups = "drop")
questions_wide <- questions_wide %>% spread(type, ans)

summary(questions_wide)

# 0
count(questions_wide[questions_wide$group=='cont' & questions_wide$pre==34 & questions_wide$post==34,])
# 0
count(questions_wide[questions_wide$group=='exp' & questions_wide$pre==40 & questions_wide$post==40,])

questions_wide <- questions_wide %>% mutate(diff=post-pre)
questions_wide$q_nc <- with(questions_wide, ifelse(diff<0, diff / (questions_wide$pre), 
                                                    ifelse(questions_wide$group=='exp', diff / (40-questions_wide$pre), diff / (34-questions_wide$pre))
                                                   )
                            )

questions_wide <- questions_wide %>% arrange(q_nc)

ggplot(questions_wide, aes(x = qid, y=post,  fill=group)) + 
#ggplot(questions_wide, aes(x = qid, y=diff, colour = as.character(qid), fill=group)) + 
#ggplot(questions_wide, aes(x = forcats::fct_inorder(as.factor(qid)), y=q_nc, colour = group, fill=group, width=.60)) + 
  geom_bar(alpha = 0.5, position = "dodge", stat='identity', size =1)

questions2 <- questions_wide[,c('qid', 'group', 'q_nc')] %>% spread(group, q_nc)
questions2$diff = questions2$exp - questions2$cont
questions2 <- questions2 %>% arrange(diff)
ggplot(questions2, aes(x = forcats::fct_inorder(as.factor(qid)), y=diff, width=.60, fill=factor(ifelse(qid=="15","Practical",ifelse(qid=="17","Practical",ifelse(qid=="20","Practical","Normal")))))) + 
    scale_fill_manual(name = "qid", values=c("grey50", "red")) +
    geom_bar(alpha = 0.5, position = "dodge", stat='identity', size =1) + ylab("q_nc") 

#ggplot(questions_wide, aes(x = ty, y=diff, colour = group, fill=group)) + 
#  geom_point()


questions3 <- questions_wide[,c('qid', 'group', 'post')] %>% spread(group, post)
# normalize the exp and cont number of students since each group has different total
questions3$cont = (questions3$cont/34)*100
questions3$exp = (questions3$exp/40)*100
questions3$diff = questions3$exp - questions3$cont
questions3 <- questions3 %>% arrange(diff)
ggplot(questions3, aes(x = forcats::fct_inorder(as.factor(qid)), y=diff, width=.60, fill=factor(ifelse(qid=="15","Practical",ifelse(qid=="17","Practical",ifelse(qid=="20","Practical","Normal")))))) + 
#ggplot(questions3, aes(x = as.factor(qid), y=diff, width=.60, fill=factor(ifelse(qid=="15","Practical",ifelse(qid=="17","Practical",ifelse(qid=="20","Practical","Normal")))))) + 
  geom_bar(alpha = 0.5, position = "dodge", stat='identity', size =1) + 
  scale_fill_manual(name = "qid", values=c("grey50", "red")) +
  ylab("post difference") 


summary(lm(qid ~ diff, data=questions_wide))

write.csv(questions_wide, "./questions_wide.csv", row.names = F)

