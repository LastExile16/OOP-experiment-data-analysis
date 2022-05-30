# ctrl+Alt+B
require("dplyr")
library('readr')
library('effsize')
library('Hmisc')
source("./functions.R")
library('psych')
require ('ggplot2')
library("ggpubr")


question_data <- read_csv("./score.csv",  col_types=c('userid'='c', 'topicid'='f', 'type'='c', 'qsid'='f', 'options'='c', 'correct'='n', 'mid'='f', 'group'='c'))
delay_score_df <- read_csv("./delay_score.csv", col_types=c('uid'='c','stid'='c'))
delay_score_df <- na.omit(delay_score_df)
delay_score_df <- within(delay_score_df, rm('stid', 'user', 'start_time'))
delay_score_df$score <- delay_score_df$score/5
feedback_df <- read_csv("./feedback_count.csv", col_types=c('userid'='c'))
timeOnTask <- read_csv("./time_on_task.csv", col_types=c('uid'='c'))

question_data$group <- as.factor(question_data$group)
question_data$type <- as.factor(question_data$type)

levels(question_data$type)
levels(question_data$group)
summary(question_data)
any(is.na(question_data$group))
which(is.na(question_data$group))

map_score_df <- read_csv("./map_score.csv", col_types=c('uid'='c'))
map_score_df <- within(map_score_df, rm('stid'))
any(duplicated(map_score_df$uid)) # user 477 is duplicated, I deleted it from the CSV file
which(duplicated(map_score_df$uid))

# step 1: extract rows related to pre-post-delay and keep the needed columns
ppd_raw <- question_data[question_data$type %in% c('pre','post'),c('userid', 'user', 'correct', 'type', 'group')]
ppd_raw$type <- factor(ppd_raw$type)
head(ppd_raw)
summary(ppd_raw)
# step 2: use `group by` to format it as `wide data`
ppd_grouped <- ppd_raw %>% group_by(group, userid, user, type)
ppd_grouped %>% summarise(test_score = sum(correct))
# experiment_control_prePostDelay
EC_ppd <- ppd_grouped %>% summarise(test_score = sum(correct), .groups = 'drop')

summary(EC_ppd)
# process delay_score to merge it with this EC_PPD
delay_score_df <- left_join(delay_score_df, EC_ppd%>%select(group, userid)%>%distinct(), by=c("uid" = "userid"))
delay_score_df$type <- as.factor('delay')
delay_score_df <- delay_score_df %>% relocate(group, uid, type, score, duration)
tmp <- within(delay_score_df, rm('duration'))
user <- c(1:length(tmp$uid))
tmp <- tibble::add_column(tmp, user, .after = 2)

EC_ppd <- rbind(EC_ppd, setNames(tmp, names(EC_ppd))) %>% arrange(group, userid, user, type)
summary(EC_ppd)

# create some interested dataframes 
C_ppd <- EC_ppd[EC_ppd$group=='cont',]
E_ppd <- EC_ppd[EC_ppd$group=='exp',]
EC_pre <- EC_ppd[EC_ppd$type=='pre',]
EC_post <- EC_ppd[EC_ppd$type=='post',]
EC_delay <- EC_ppd[EC_ppd$type=='delay',]

# exp vs cont
wilcox.test(test_score ~ group, EC_post, paired = F)
wilcox.test(test_score ~ group, EC_delay, paired = F)
coin::wilcox_test(test_score ~ group, EC_post, exact = T, paired = F)
summary(EC_post$test_score ~ EC_post$group)
summary(EC_delay$test_score ~ EC_delay$group)

plot(EC_ppd[EC_ppd$type=='pre',]$test_score, EC_ppd[EC_ppd$type=='post',]$test_score, pch=19, col=factor(levels(EC_ppd$group)))
legend("topleft",
       legend = levels(EC_ppd$group),
       pch = 19,
       col = factor(levels(EC_ppd$group)))

boxplot(test_score ~ group, EC_post) # almost same
boxplot(test_score ~ group, EC_delay) # somehow different
# any outliers?
outliers::grubbs.test(EC_post[EC_post$group=='exp',]$test_score)
outliers::grubbs.test(EC_post[EC_post$group=='cont',]$test_score)
outliers::dixon.test(EC_post[EC_post$group=='exp',]$test_score)
EnvStats::rosnerTest(EC_post[EC_post$group=='cont',]$test_score, k = 2)
EnvStats::rosnerTest(EC_post[EC_post$group=='exp',]$test_score, k = 2)

# homogenity of knowledge about the material - using pretest score
homogenity("test_score", "group", EC_pre)

# transform all data to wide format
fdata <- EC_ppd[EC_ppd$type == 'pre', c('group', 'userid', 'user', 'test_score')]
fdata <- fdata %>% rename(pre_score = test_score)
fdata <- left_join(fdata, EC_ppd[EC_ppd$type == 'post', c('userid', 'test_score')], by="userid")
fdata <- fdata %>% rename(post_score = test_score)
fdata <- left_join(fdata, EC_ppd[EC_ppd$type == 'delay', c('userid', 'test_score')], by="userid")
fdata <- fdata %>% rename(delay_score = test_score)

## join the map_score
fdata <- left_join(fdata, map_score_df, by=c("userid" = "uid"))
#fdata$pre_score <- fdata$pre_score/10
#fdata$post_score <- fdata$post_score/10
#fdata$delay_score <- fdata$delay_score/10
#fdata$map_score <- fdata$map_score/10

## join the feedback
fdata <- left_join(fdata, feedback_df, by="userid")
fdata <- fdata %>% mutate(feedbacks=replace(feedbacks, (is.na(feedbacks) & group == 'exp'), 0))

# stid_to-uid for those who responded the self-efficacy as well as prepost test
stid_to_uid <- read_csv("./stid_to_uid_those_who_participated_in_the_final_experiment.csv", col_types=c('uid'='c','stid'='c'))
fdata <- left_join(fdata, stid_to_uid, by=c("userid" = "uid"))

library(magrittr)
#We can use the %<>% compound assignment operator from magrittr to change in place
fdata %<>% mutate(stid = case_when((is.na(stid) & numeric.string(user)) ~ user, TRUE~stid))

# below match is gained through xlsx file
fdata[fdata$user=='stu67',]$stid = '1402017084'
fdata[fdata$user=='stu72',]$stid = '1402020103'
fdata[fdata$user=='stu76',]$stid = '1402020102'
fdata[fdata$user=='stu79',]$stid = '1402020023'
fdata[fdata$user=='stu80',]$stid = '1402020005'
fdata[fdata$user=='stu94',]$stid = '1402018119'
fdata[fdata$user=='stu100',]$stid =  '1402020095'
fdata[fdata$user=='stu111',]$stid =  '1402018173'

## join the timeOnTask
#fdata <- left_join(fdata, timeOnTask,  by=c("userid" = "uid"))


# https://www.physport.org/recommendations/Entry.cfm?ID=93334
#### Normalized Change

count(fdata[fdata$pre_score==20 & fdata$post_score==20,])
fdata %>% select(userid, pre_score, post_score, map_score, group) %>% filter((pre_score==post_score)&(pre_score==20 | pre_score==0))
ggplot(exp_fdata(fdata), aes(x = pre_score, y = post_score)) +
  #geom_point(aes(shape=group, color=group)) +
  # geom_text(aes(label=userid),hjust=0, vjust=0) +
  geom_count(aes(color = ..n.., size = ..n..)) +
  geom_abline() +
  ggtitle("KB group")
#guides(color = 'legend')
#scale_shape_manual(values=c(4,6))

# here, I filter out those having 20 or 0 in pre and post at the same time.
fdata %>% select(userid, pre_score, post_score, group) %>%
  filter(!((pre_score==post_score)&(pre_score==20 | pre_score==0))) %>%
  mutate(diff=post_score-pre_score)

# another way is to calculate diff for all, then change diff to NA for those
# having 20 or 0 in pre and post
fdata <- fdata %>% mutate(diff=post_score-pre_score)
fdata <- fdata %>% mutate(diff=replace(diff, ((pre_score==post_score)&(pre_score==20 | pre_score==0)), NA))

fdata$post_nc <- with(fdata, ifelse(diff<0, diff / (fdata$pre_score), diff / (20-fdata$pre_score)))
#### Normazlied gain - biased toward low scores https://journals.aps.org/prper/pdf/10.1103/PhysRevPhysEducRes.16.010108
fdata$post_ng <- fdata$diff / (20-fdata$pre_score)
fdata[ is.infinite(fdata$post_ng),]$post_ng = NA

#### delayed Normalized Change
count(fdata[fdata$pre_score==20 & fdata$delay_score==20,])
fdata %>% select(userid, pre_score, delay_score, map_score, group) %>% filter((pre_score==delay_score)&(pre_score==20 | pre_score==0))
ggplot(exp_fdata(fdata), aes(x = post_score, y = delay_score)) +
  geom_count(aes(color = ..n.., size = ..n..)) +
  geom_abline() +
  ggtitle("KB group")

fdata <- fdata %>% mutate(diff=delay_score-pre_score)
fdata <- fdata %>% mutate(diff=replace(diff, ((pre_score==delay_score)&(pre_score==20 | pre_score==0)), NA))

fdata$delay_nc <- with(fdata, ifelse(diff<0, diff / (fdata$pre_score), diff / (20-fdata$pre_score)))
#### Normazlied gain - dbiased toward low scores
fdata$delay_ng <- fdata$diff / (20-fdata$pre_score)
fdata[ is.infinite(fdata$delay_ng),]$delay_ng = NA
# remove column diff
fdata <- within(fdata, rm('diff'))

fdata %>% mean_med_sd_postdelay_nc()
fdata %>% drop_na(delay_score) %>% mean_med_sd_postdelay_nc()

fdata %>% mean_med_sd_prepostdelay()
fdata %>% drop_na(delay_score) %>% mean_med_sd_prepostdelay()

# for cont: delay_score, pre_score are NOT normal
# for exp: delay_score, map_score are NOT normal
fdata %>% group_by(group) %>% shapiro_test(vars = c('pre_score', 'post_score', 'post_nc', 'delay_score', 'delay_nc', 'map_score') )

# delay_score, pre_score, map_score are not normal
fdata %>% shapiro_test(vars = c('pre_score', 'post_score', 'post_nc', 'delay_score', 'delay_nc', 'map_score') )

x = pivot_longer(fdata[,c('userid', 'post_nc', 'delay_nc')], cols = c('post_nc', 'delay_nc'))

long = list(melt(fdata[,c('group', 'userid', 'pre_score', 'post_score', 'delay_score')], id.vars= c('group','userid')))
long = append(long, list(melt(fdata[,c('group', 'userid', 'post_nc', 'delay_nc')], id.vars= c('group','userid'))))
long[1]

as.data.frame(long[1]) %>% group_by(variable, group) %>% summarise(count = n(), mean = mean(value, na.rm=T), sd = sd(value, na.rm = T))

for (i in 1:2) {
  print(ggplot(as.data.frame(long[i]), aes (value)) +
    geom_density(aes(color = variable)) +
  # or maybe you wanted separate plots on the same page?
  #print(ggplot(as.data.frame(long[i]), aes (value)) +
    #geom_density() +
    facet_wrap(~group))
  
  # QQPLOT
  print(ggqqplot(as.data.frame(long[i]), x = "value") + 
          facet_wrap(~group+variable))
}

# map score
ggplot(fdata, aes (map_score)) +
  geom_density(aes(color = group))
ggplot(fdata, aes(y=map_score)) + 
  geom_boxplot()
ggqqplot(fdata, x = "map_score")

## compare NC
boxplot(post_nc~group, fdata)
boxplot(delay_nc~group, fdata)


outliers::grubbs.test(fdata[fdata$group=='exp',]$post_nc, type = 11, two.sided = T)
outliers::grubbs.test(fdata[fdata$group=='cont',]$post_nc, type = 11, two.sided = T)

EnvStats::rosnerTest(fdata[fdata$group=='exp',]$post_nc, k = 1)
EnvStats::rosnerTest(fdata[fdata$group=='cont',]$post_nc, k = 1)
no_outlier_postnc <- tukey_outlier("post_nc", fdata[fdata$group=='cont',c('userid','group', 'post_nc')])

summary(no_outlier_postnc) # two people removed
summary(fdata[fdata$group=='cont',c('userid','group', 'post_nc')])
boxplot(no_outlier_postnc$post_nc, fdata[fdata$group=='exp',]$post_nc)
t.test(no_outlier_postnc$post_nc, fdata[fdata$group=='exp',]$post_nc, paired = F)
t.test(fdata[fdata$group=='cont',]$post_nc, fdata[fdata$group=='exp',]$post_nc, paired = F)

# 615, 494
fdata %>% 
  group_by(group) %>% 
  slice_min(post_nc)
# 563, 454
fdata %>% 
  group_by(group) %>% 
  slice_max(post_nc)


fdata <- fdata %>% filter(!userid %in% c('615','494', '563', '454'))
summary(fdata)
# after removing the two outliers per group, go back to for loop to re-draw the plots



# check for more outliers
outliers::grubbs.test(fdata[fdata$group=='exp',]$post_nc, type = 11, two.sided = T)
outliers::grubbs.test(fdata[fdata$group=='cont',]$post_nc, type = 11, two.sided = T)
# 578, 501
min_out <- fdata %>% 
  group_by(group) %>% 
  slice_min(post_nc)
# 574, 453
max_out <- fdata %>% 
  group_by(group) %>% 
  slice_max(post_nc)
fdata2 <- fdata %>% filter((!userid %in% min_out[,c('userid')]$userid) & (!userid %in% max_out[,c('userid')]$userid))
summary(fdata2$group)
fdata2 %>% group_by(group) %>% shapiro_test(vars = c('pre_score', 'post_score', 'post_nc', 'delay_score', 'delay_nc', 'map_score') )
t.test(fdata2[fdata2$group=='cont',]$post_nc, fdata2[fdata2$group=='exp',]$post_nc, paired = F)
t.test(fdata2[fdata2$group=='cont',]$delay_nc, fdata2[fdata2$group=='exp',]$delay_nc, paired = F)
## repeat this section many times
outliers::grubbs.test(fdata2[fdata2$group=='exp',]$post_nc, type = 11, two.sided = T)
outliers::grubbs.test(fdata2[fdata2$group=='cont',]$post_nc, type = 11, two.sided = T)
# 617, 478
min_out <- fdata2 %>% 
  group_by(group) %>% 
  slice_min(post_nc)
# 564, 599, 602
max_out <- fdata2 %>% 
  group_by(group) %>% 
  slice_max(post_nc)
fdata2 <- fdata2 %>% filter((!userid %in% min_out[,c('userid')]$userid) & (!userid %in% max_out[,c('userid')]$userid))
summary(fdata2$group)
fdata2 %>% group_by(group) %>% shapiro_test(vars = c('pre_score', 'post_score', 'post_nc', 'delay_score', 'delay_nc', 'map_score') )
t.test(fdata2[fdata2$group=='cont',]$post_nc, fdata2[fdata2$group=='exp',]$post_nc, paired = F)
t.test(fdata2[fdata2$group=='cont',]$delay_nc, fdata2[fdata2$group=='exp',]$delay_nc, paired = F)
###########

#### summary
summarytools::stby(
  data = fdata,
  INDICES = fdata$group, # by Species
  FUN = summarytools::descr, # descriptive statistics
  stats = "common", # most common descr. stats
  transpose = TRUE
)

#### PRE vs POST vs DELAY
# homogenity after outlier removal
homogenity("pre_score", "group", fdata)

ggplot(fdata, aes(x=group, y=post_nc, fill=group)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE)
ggplot(fdata, aes(x=group, y=delay_nc, fill=group)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE)

data <- fdata %>% select(c('userid','group', 'pre_score', 'post_score', 'delay_score')) %>% filter(!is.na(delay_score))
data <- fdata %>% select(c('userid','group', 'pre_score', 'post_score'))


data <- fdata %>% select(c('userid','group', 'post_ng', 'delay_ng')) %>% filter(!is.na(delay_ng))
data <- fdata %>% select(c('userid','group', 'post_nc', 'delay_nc')) %>% filter(!is.na(delay_nc))

vars_wide <- melt(data,id.vars = c('userid', 'group'))

means <- aggregate(value ~  group+variable, vars_wide, FUN = function(x) {round(mean(x), digits=2)})
p <- ggplot(vars_wide, aes(x=variable, y=value, fill=group)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point",
               shape=18, size=3, show.legend=FALSE, position = position_dodge(width = 0.78)) +
  geom_text(data = means, aes(label = value, y = value + 0.08), position = position_dodge(width = 0.78))
p+xlab("") +
  ylab("Score") 

p <- ggplot(vars_wide, aes(x=variable, y=value, fill=group)) +
  geom_violin(alpha = 0.5) 

p+geom_boxplot(width = 0.07, position = position_dodge(width = 0.9)) +
  xlab("") +
  ylab("Score")

## after confirming normality of data
## second condition is about equality of variances between the groups
## for that do F-Tets
res.ftest <- var.test(post_nc ~ group, data = fdata)
res.ftest # significantly different
res.ftest <- var.test(delay_nc ~ group, data = fdata)
res.ftest # not diffirent

#--
t_test(data = fdata, formula = post_score ~ group, paired = F, var.equal = T)

t_test(data = fdata, formula = post_ng ~ group, paired = F, var.equal = F)
t_test(data = fdata, formula = post_nc ~ group, paired = F, var.equal = F)
#t.test(cont_fdata(fdata)$post_nc, cont_fdata(fdata)$delay_nc, paired = T)
#t.test(exp_fdata(fdata)$post_nc, exp_fdata(fdata)$delay_nc, paired = T)

# why difference in delay_nc is not significant while it is very similar to post_nc!!
t_test(data = fdata, formula = delay_score ~ group, paired = F, var.equal = T)
t_test(data = fdata, formula = delay_ng ~ group, paired = F, var.equal = T)
t_test(data = fdata, formula = delay_nc ~ group, paired = F, var.equal = T)


effsize::cohen.d(post_nc ~ group, data=fdata)
effsize::cohen.d(post_ng ~ group, data=fdata)

effsize::cohen.d(delay_nc ~ group, data=fdata)
effsize::cohen.d(delay_ng ~ group, data=fdata)


t_test(data = vars_wide[vars_wide$group=='cont',], formula = value ~ variable, paired = T)
t_test(data = vars_wide[vars_wide$group=='exp',], formula = value ~ variable, paired = T)

# raw score comparison
t.test(cont_fdata(fdata)$pre_score, cont_fdata(fdata)$post_score, paired = T)
t.test(cont_fdata(fdata)$post_score, cont_fdata(fdata)$delay_score, paired = T)
t.test(exp_fdata(fdata)$pre_score, exp_fdata(fdata)$post_score, paired = T)
t.test(exp_fdata(fdata)$post_score, exp_fdata(fdata)$delay_score, paired = T)

#### Linear Regression Model
# experimental group
summary(lm(post_score ~ map_score+pre_score, data=exp_fdata(fdata)))
summary(lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata)))

summary(lm(delay_score ~ map_score+pre_score+post_score, data=exp_fdata(fdata)))
summary(lm(delay_nc ~ map_score+pre_score+post_score, data=exp_fdata(fdata)))

lm_post_mapScore_exp <- lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata))
summary(lm_post_mapScore_exp)
plot(exp_fdata(fdata)$map_score, exp_fdata(fdata)$post_nc)
abline(lm_post_mapScore_exp)

ggplot(exp_fdata(fdata), aes(x=map_score, y=post_nc)) + 
  geom_point() +
  geom_abline(slope = coef(lm_post_mapScore_exp)[["map_score"]], 
              intercept = coef(lm_post_mapScore_exp)[["(Intercept)"]])
# control group
summary(lm(post_score ~ pre_score+sm_score, data=cont_fdata(fdata)))
summary(lm(post_nc ~ pre_score+sm_score, data=cont_fdata(fdata)))

summary(lm(delay_score ~ pre_score+post_score+sm_score, data=cont_fdata(fdata)))
summary(lm(delay_nc ~ pre_score+post_score+sm_score, data=cont_fdata(fdata)))


#### Correlation
cor.test(exp_fdata(fdata)$post_score, exp_fdata(fdata)$map_score)

cols1 <- c('pre_score', 'post_score', 'delay_score', 'post_nc', 'delay_nc', 'sm_score', 'post_ng', 'delay_ng')
corr.test(cont_fdata(fdata)[,cols1], method = "pearson", use = "pairwise.complete.obs")

cols2 <- c('pre_score', 'post_score', 'delay_score', 'feedbacks', 'post_nc', 'delay_nc', 'map_score', 'post_ng', 'delay_ng')
corr.test(exp_fdata(fdata)[,cols2], method = "pearson", use = "pairwise.complete.obs")

x <- corr.test(fdata[,cols1], method = "pearson", use = "pairwise.complete.obs")
print(corr.p(x$r,n=39),short=FALSE)
round(x$p, 5)


#PerformanceAnalytics::chart.Correlation(cor(cont_fdata(fdata)[,cols1], method = "pearson", use = "pairwise.complete.obs"), histogram=TRUE, pch=19)
#PerformanceAnalytics::chart.Correlation(cor(exp_fdata(fdata)[,cols2], method = "pearson", use = "pairwise.complete.obs"), histogram=TRUE, pch=19)
PerformanceAnalytics::chart.Correlation(cont_fdata(fdata)[,cols1], method = "pearson", histogram=TRUE, pch=19) + mtext("Summarization Group", side=3, line=3)
PerformanceAnalytics::chart.Correlation(cont_fdata(fdata)[,cols1], method = "spearman", histogram=TRUE, pch=19) + mtext("Summarization Group", side=3, line=3)
PerformanceAnalytics::chart.Correlation(exp_fdata(fdata)[,cols2], method = "pearson", histogram=TRUE, pch=19) + mtext("KB Group", side=3, line=3)
PerformanceAnalytics::chart.Correlation(exp_fdata(fdata)[,cols2], method = "spearman", histogram=TRUE, pch=19) + mtext("KB Group", side=3, line=3)

b <- Hmisc::rcorr(as.matrix(fdata[,cols1]))
corrplot::corrplot(b$r, type="upper", order="hclust", 
                   p.mat = b$P, sig.level = 0.05, insig = "p-value")

WHAT ABOUT THE CORRELATION IN MIDTERM AND SO ON
WHAT about the dropout rate between the groups
###anova test ...
# result is equalt to ttest with var.equal = T
one.way <- aov(post_nc ~ group, data = fdata)
summary(one.way)
one.way <- aov(delay_nc ~ group, data = fdata)
summary(one.way)

two.way <- aov(post_nc ~ group + pre_score, data = fdata)

summary(two.way)

# MANOVA test
res.man <- manova(cbind(post_nc, delay_nc) ~ group, data = fdata)
summary(res.man)
# Look to see which differ
summary.aov(res.man)

#### Summarization score
smgroup_score <- read_csv("./sm_score.csv", col_types=c('stid'='c'))
fdata <- left_join(fdata, smgroup_score, by='stid')
fdata <- fdata %>% mutate(activity_score = ifelse(!is.na(map_score), map_score, sm_score))
count(fdata[is.na(fdata$activity_score),])
fdata[ is.na(fdata$activity_score),]$activity_score = 0

