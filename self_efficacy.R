

# run after running oop_prepos_analysis.R

pre_slfEfcy <- read_csv("./pre_selfEfficacy.csv", col_types=c('stid'='c'))
post_slfEfcy <- read_csv("./post_selfEfficacy.csv", col_types=c('stid'='c'))

  #### join the self_efficacy
fdata <- left_join(fdata, pre_slfEfcy[, c('stid', 'pre_eff_avg')], by=("stid"))
fdata <- left_join(fdata, post_slfEfcy[, c('stid', 'post_eff_avg')], by=("stid"))

fdata %>% group_by(group) %>% shapiro_test(vars = c('pre_eff_avg', 'post_eff_avg') )

long = list(melt(fdata[,c('group', 'userid', 'pre_eff_avg', 'post_eff_avg')], id.vars= c('group','userid')))
ggplot(as.data.frame(long[i]), aes (value)) +
  geom_density(aes(color = variable)) +
  # or maybe you wanted separate plots on the same page?
  #print(ggplot(as.data.frame(long[i]), aes (value)) +
  #geom_density() +
  facet_wrap(~group)

ggqqplot(as.data.frame(long[i]), x = "value") + 
  facet_wrap(~group+variable)

ggplot(as.data.frame(long[i]), aes(y = value, x=variable)) + 
  geom_boxplot(aes(color=group))


means <- aggregate(post_eff_avg ~  group, fdata, FUN = function(x) {round(mean(x), digits=2)})
ggplot(fdata, aes(x=group, y=post_eff_avg, fill=group)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) +
geom_text(data = means, aes(label = post_eff_avg, y = post_eff_avg + 0.08), position = position_dodge(width = 0.78))

# cont vs exp
t.test(cont_fdata(fdata)$post_eff_avg, exp_fdata(fdata)$post_eff_avg, paired = F)

t.test(cont_fdata(fdata)$pre_eff_avg, cont_fdata(fdata)$post_eff_avg, paired = T)
t.test(exp_fdata(fdata)$pre_eff_avg, exp_fdata(fdata)$post_eff_avg, paired = T)

#### internal reliability
pre_slfEfcy <- left_join(pre_slfEfcy, fdata[, c('stid', 'group')], by=("stid"))
post_slfEfcy <- left_join(post_slfEfcy, fdata[, c('stid', 'group')], by=("stid"))

slf_efc_cont <- cont_fdata(post_slfEfcy)[, c('q1','q2','q3','q4','q5','q6','q7','q8')]
slf_efc_exp <- exp_fdata(post_slfEfcy)[, c('q1','q2','q3','q4','q5','q6','q7','q8')]
slf_efcAlpha<-alpha(slf_efc_cont)
slf_efcAlpha<-alpha(slf_efc_exp)
slf_efcAlpha
####
