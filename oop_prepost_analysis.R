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

fdata %>% mean_med_sd("pre_score", "post_score", "post_nc")
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
t_test(post_nc~group, data=fdata, paired = F)
summary(fdata$group)

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
t_test(post_nc~group, data=fdata2, paired = F)
t_test(delay_nc~group, data=fdata2, paired = F)
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
t_test(post_nc~group, data=fdata2, paired = F)
t_test(delay_nc~group, data=fdata2, paired = F)
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
               shape=18, size=3, show.legend=FALSE) +
  annotate(
    "text", label = "P-value = 0.0399",
    x = 2, y = 0.8, size = 4, colour = "red"
  )

tmpdata <- fdata                     # Duplicate data
levels(tmpdata$group) <- c("SUM",  # Relevel factor labels
                                       "CRS")
ggplot(tmpdata, aes(x=group, y=post_nc)) + 
  geom_boxplot() + theme_bw() +
  stat_summary(fun=mean, colour="black", geom="point", 
               shape=2, size=2, show.legend=FALSE) +
  stat_compare_means(method = "t.test", label =  "p.signif", label.x = 1.99, size=7) +
  xlab("") +
  ylab("Normalized Change Score")

remove(tmpdata)

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

vars_wide_postnc <- vars_wide[vars_wide$variable != 'delay_nc',]
vars_wide_postnc$group <- ifelse(vars_wide_postnc$group=='cont', 'Summarization', 'CRS')
p <- ggboxplot(vars_wide_postnc, x="group", y="value",
                 palette = "jco", legend = "none") +
                xlab("") +  ylab("Post Normalized Gain")
# Change method
p + stat_compare_means(method = "t.test", label.x = 1.5)

# the value in this test is very different from ttest of fdata because this test 
# uses vars_wide_postnc of students who participated in pre-post and delay test equally
# the difference lies in excluding two students from experiment group
compare_means(value ~ group, data = vars_wide_postnc, paired = F, method = "t.test")


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

effsize::cohen.d(cont_fdata(fdata)$post_score, cont_fdata(fdata)$pre_score)
effsize::cohen.d(exp_fdata(fdata)$post_score, exp_fdata(fdata)$pre_score)

# Common Language Effect Size
#An interesting, though not often used, interpretation of differences between groups can be provided by the common language effect size (McGraw and Wong, 1992), also known as the probability of superiority (Grissom and Kim, 2005), which is a more intuitively understandable statistic than Cohen's d or r. It can be calculated directly from Cohen's d, converts the effect size into a percentage, and expresses the probability that a randomly sampled person from one group will have a higher observed measurement than a randomly sampled person from the other group (for between designs) or (for within-designs) the probability that an individual has a higher value on one measurement than the other. 
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3840331/#:~:text=An%20interesting%2C%20though,than%20the%20other.
# pnorm(d / sqrt(2))
canprot::CLES(cont_fdata(fdata)$pre_score, cont_fdata(fdata)$post_score)

canprot::CLES(exp_fdata(fdata)$pre_score, exp_fdata(fdata)$post_score)
# [1] 0.6786666
canprot::CLES(cont_fdata(fdata)$post_nc, exp_fdata(fdata)$post_nc)
#[1] 0.6320122

#### Linear Regression Model
# experimental group
summary(lm(post_score ~ map_score+pre_score, data=exp_fdata(fdata)))
sjPlot::tab_model(lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata)), p.style = "numeric_stars")
 

tab_model(glm(post_nc ~ activity_score+pre_score+group, data=fdata))
# prior knowledge (pre_score) affects the post score most significantly,
# and then comes the activity score which means students prior knowledge is most
# important to learn the material.
# the group is not significant means the scores for both groups are reliable because
# the difference in score doesn't make difference in post score
tab_model(glm(post_score ~ activity_score+pre_score+group, data=fdata))

summary(lm(delay_score ~ map_score+pre_score+post_score, data=exp_fdata(fdata)))
summary(lm(delay_nc ~ map_score+pre_score+post_score, data=exp_fdata(fdata)))

lm_post_mapScore_exp <- lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata))
summary(lm_post_mapScore_exp)
plot(exp_fdata(fdata)$map_score, exp_fdata(fdata)$post_nc)
abline(lm_post_mapScore_exp)

ggplot(exp_fdata(fdata), aes(x=map_score, y=post_nc*100)) + 
  geom_point() +
  geom_abline(slope = coef(lm_post_mapScore_exp)[["map_score"]], 
              intercept = coef(lm_post_mapScore_exp)[["(Intercept)"]])
# control group
lm_post_pre_cont <- lm(post_score ~ pre_score+sm_score, data=cont_fdata(fdata))
summary(lm_post_pre_cont)
summary(lm(post_nc ~ pre_score+sm_score, data=cont_fdata(fdata)))

summary(lm(delay_score ~ pre_score+post_score+sm_score, data=cont_fdata(fdata)))
summary(lm(delay_nc ~ pre_score+post_score+sm_score, data=cont_fdata(fdata)))

plot(cont_fdata(fdata)$pre_score, cont_fdata(fdata)$post_score)
abline(lm_post_pre_cont)
plot(cont_fdata(fdata)$pre_score, cont_fdata(fdata)$post_nc)

plot(cont_fdata(fdata)$sm_score, cont_fdata(fdata)$post_nc)

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

two.way <- aov(delay_nc ~ group + pre_score, data = fdata)

summary(two.way)

# MANOVA test
res.man <- manova(cbind(post_nc, delay_nc) ~ group, data = fdata)
summary(res.man)
# Look to see which differ
summary.aov(res.man)

#### Summarization score
smgroup_score <- read_csv("./sm_score.csv", col_types=c('stid'='c'))

library(irr)
smScoreKappaCoeff <- kappam.fleiss(smgroup_score[,c('sm_score_hanna', 'sm_score_ulpa')])
smScoreKappaCoeff
#for (i in 1:8) {
#  print(smScoreKappaCoeff[i])
#}

#hanna_score <- openxlsx::read.xlsx("./summary answer/PBO.21.22.1-Sumarisasi-jawaban_ENG_Nilai_Hanna.xlsx", 'PBO.21.22.1 Sumarisasi', startRow=2, cols = c(2, 13:22))
#ulpa_score <- openxlsx::read.xlsx("./summary answer/PBO.21.22.1-Sumarisasi-jawaban_ENG_ulpa.xlsx", 'PBO.21.22.1 Sumarisasi', startRow=2, cols = c(2, 13:22))
#names(hanna_score)
#names(ulpa_score)

#for (i in 2:11) {
#  print(i)
#  print(names(hanna_score)[i])
#  print(kappam.fleiss(cbind(hanna_score[,i], ulpa_score[,i]))['value'])
#}

#cbind(hanna_score[,1] - ulpa_score[,1])
#cbind(hanna_score[,8] - ulpa_score[,8])

fdata <- left_join(fdata, smgroup_score[,c('stid', 'sm_score')], by='stid')
fdata <- fdata %>% mutate(activity_score = ifelse(!is.na(map_score), map_score, sm_score))
count(fdata[is.na(fdata$activity_score),])
fdata[ is.na(fdata$activity_score),]$activity_score = 0




# Histogram
hist(fdata$map_score, breaks = 10, prob = TRUE,
     col = "white",
     main = "")

# Add new plot
par(new = TRUE)

# Box plot
boxplot(fdata$map_score, horizontal = TRUE, axes = FALSE,
        col = rgb(0, 0.8, 1, alpha = 0.5))

sjPlot::tab_model(lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata)), p.style = "numeric_stars")
summary(lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata)))
apaTables::apa.reg.boot.table(lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata)))
sjPlot::tab_model(lm(post_nc ~ sm_score+pre_score, data=cont_fdata(fdata)), p.style = "numeric_stars")
summary(lm(post_nc ~ sm_score+pre_score, data=cont_fdata(fdata)))
apaTables::apa.reg.boot.table(lm(post_nc ~ sm_score+pre_score, data=cont_fdata(fdata)))
texreg::texreg(lm(post_nc ~ sm_score+pre_score, data=cont_fdata(fdata)), booktabs = TRUE, dcolumn = TRUE)

apaTables::apa.reg.boot.table(lm(post_nc ~ map_score+pre_score, data=exp_fdata(fdata)),lm(post_nc ~ sm_score+pre_score, data=cont_fdata(fdata)), filename = "regression")

xxx <- aov(formula = delay_nc ~ post_nc+group, data=fdata)
Anova(xxx, type="III")
# ANCOVA: after controlling for post_nc we want to see if the difference in 
# delay is because of groups. results show it is not.
#Anova Table (Type III tests)

#Response: delay_nc
#Sum Sq Df F value    Pr(>F)    
#(Intercept) 1.2924  1 14.9703 0.0002422 ***
#  post_nc     1.5779  1 18.2784 5.924e-05 ***
#  group       0.0002  1  0.0023 0.9620393    
#Residuals   6.0430 70 


# write.csv(fdata, "./fdata.csv", row.names = F)

#### two way ANOVA
#https://statistics.laerd.com/spss-tutorials/two-way-anova-using-spss-statistics.php#:~:text=The%20two%2Dway%20ANOVA%20compares,variables%20on%20the%20dependent%20variable.
#https://mcn-www.jwu.ac.jp/~kuto/kogo_lab/psi-home/stat2000/DATA/07/09.HTM
#http://www.sthda.com/english/wiki/two-way-anova-test-in-r
#time=time
#sup=group
#len=learning-gain

fdata_long <- melt(fdata[,c('userid', 'group', 'post_nc', 'delay_nc')], id.vars= c('group','userid'), na.rm = T)
fdata_long <- melt(fdata[!is.na(fdata$delay_nc),c('userid', 'group', 'post_nc', 'delay_nc')], id.vars= c('group','userid'))
names(fdata_long) <- c('group', "userid", 'time', 'learning-gain')
table(fdata_long$group, fdata_long$time)

# Box plot with multiple groups
# +++++++++++++++++++++
# Plot learning-gain by groups ("time")
# Color box plot by a second group: "group"
ggboxplot(fdata_long, x = "time", y = "learning-gain", color = "group", palette = c("#00AFBB", "#E7B800"))

# Line plots with multiple groups
# +++++++++++++++++++++++
# Plot learning-gain by groups ("time")
# Color box plot by a second group: "group"
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(fdata_long, x = "time", y = "learning-gain", color = "group",
       add = c("mean_se"),
       palette = c("#00AFBB", "#E7B800"))
#R Base functions:
boxplot(`learning-gain` ~ group * time, data=fdata_long, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="learning-gain")
# Two-way interaction plot
interaction.plot(x.factor = fdata_long$time, trace.factor = fdata_long$group, 
                 response = fdata_long$`learning-gain`, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "time", ylab="learning-gain",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

# We want to know if learning-gain depends on group and time.
res.aov2 <- aov(`learning-gain` ~ group + time, data = fdata_long)
summary(res.aov2)
#               Df   Sum-Sq   Mean-Sq   F-value  Pr(>F)  
# group         1    0.600    0.5996    6.280    0.0133 *
# time          1    0.389    0.3888    4.073    0.0455 *
# Residuals    143  13.652    0.0955   
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# From the ANOVA table above, we can conclude that both group and time are statistically significant. 
# group is the most significant factor variable. These results would lead us to believe that 
# changing time or the group, will impact significantly the mean learning-gain.

# Not the above fitted model is called additive model. 
# It makes an assumption that the two factor variables are independent. 
# If you think that these two variables might interact to create an synergistic effect, 
# replace the plus symbol (+) by an asterisk (*), as follow.

# Two-way ANOVA with interaction effect
res.aov3 <- aov(`learning-gain` ~ group*time, data = fdata_long)
summary(res.aov3)

#              Df    Sum-Sq   Mean-Sq    F value   Pr(>F)  
# group         1    0.600    0.5996     6.262     0.0135 *
# time          1    0.389    0.3888     4.061     0.0458 *
# group:time    1    0.055    0.0552     0.577     0.4488  
# Residuals   142   13.597    0.0958 
# It can be seen that the two main effects (group and time) are statistically significant, but not their interaction.
# Note that, in the situation where the interaction is not significant you should use the additive model.

# From the ANOVA results, you can conclude the following, based on the p-values and a significance level of 0.05:
  
# the p-value of group is 0.0135 (significant), which indicates that the groups are associated with significant different in learning-gain.
# the p-value of time is 0.0458 (significant), which indicates that the times (after class and after one week) are associated with significant different learning-gain.
# the p-value for the interaction between group*time is 0.44 (not significant). in case of significant difference: it indicates that the relationships between time and learning-gain depends on the group.

# summary statistics
group_by(fdata_long, group, time) %>%
  summarise(
    count = n(),
    mean = mean(`learning-gain`, na.rm = TRUE),
    sd = sd(`learning-gain`, na.rm = TRUE)
  )

model.tables(res.aov3, type="means", se = TRUE)

