

# run after running oop_prepos_analysis.R

pre_slfEfcy <- read_csv("./pre_selfEfficacy.csv", col_types=c('stid'='c'))
post_slfEfcy <- read_csv("./post_selfEfficacy.csv", col_types=c('stid'='c'))

pre_slfEfcy <- pre_slfEfcy %>% mutate(HL = ifelse(pre_eff_avg < 4, "L",
                                             ifelse(pre_eff_avg > 4, "H", "N")))
post_slfEfcy <- post_slfEfcy %>% mutate(HL = ifelse(post_slfEfcy < 4, "L",
                                             ifelse(post_slfEfcy > 4, "H", "N")))
  #### join the self_efficacy
fdata_slfefcy <- left_join(fdata, pre_slfEfcy[, c('stid', 'pre_eff_avg')], by=("stid"))
fdata_slfefcy <- left_join(fdata_slfefcy, post_slfEfcy[, c('stid', 'post_eff_avg')], by=("stid"))

fdata_slfefcy <- left_join(fdata_slfefcy, firstexp_kbsmgroup, by='stid') # get this data from oopdata_prepost.R of the first experiment
names(fdata_classScore)

any(is.na(fdata_classScore$groupexp1))
which(is.na(fdata_classScore$groupexp1))

fdata_slfefcy %>% group_by(group) %>% shapiro_test(vars = c('pre_eff_avg', 'post_eff_avg') )

long = list(melt(fdata_slfefcy[,c('group', 'userid', 'pre_eff_avg', 'post_eff_avg')], id.vars= c('group','userid')))
ggplot(as.data.frame(long), aes (value)) +
  geom_density(aes(color = variable)) +
  # or maybe you wanted separate plots on the same page?
  #print(ggplot(as.data.frame(long[i]), aes (value)) +
  #geom_density() +
  facet_wrap(~group)

ggqqplot(as.data.frame(long), x = "value") + 
  facet_wrap(~group+variable)

ggplot(as.data.frame(long), aes(y = variable, x=variable)) + 
  geom_boxplot(aes(color=group))


d=data.frame(x1=c(1,4,1,4), x2=c(4,7,4,7), 
             y1=c(1,4,4,1), y2=c(4,7,7,4), 
             t=c('a','b', 'c','d'), 
             pre_eff_avg=c(6,3,5,3), # without this it will give error
             post_eff_avg=c(4,5,6,3))
ggplot(fdata_slfefcy, aes(x = post_eff_avg, y = pre_eff_avg)) + 
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, # without giving a different data it will create many rectangles over each other so the alpha value will become useless
                                ymin=y1, ymax=y2, 
                                fill=t), color="white", alpha=0.2, show.legend=FALSE) +
  geom_point() +
  #geom_count(aes(colour = ..n.., size=2), shape=17) +
  #scale_colour_gradient(low = "grey", high = "black")
  #scale_colour_gradient2() +
  geom_abline() +
  facet_wrap(~group)


d=data.frame(x1=c(1,10,1,10), x2=c(10,20,10,20), 
             y1=c(1,10,10,1), y2=c(10,20,20,10), 
             t=c('a','b', 'c','d'), 
             pre_score=c(6,3,5,3),
             post_score=c(6,3,5,3))
ggplot(fdata_slfefcy, aes(x = post_score, y = pre_score)) + 
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, # without giving a different data it will create many rectangles over each other so the alpha value will become useless
                                ymin=y1, ymax=y2, 
                                fill=t), color="white", alpha=0.2, show.legend=FALSE) +
  geom_count(aes(colour = ..n.., shape=factor(group)), size=5) +
  scale_shape_manual(values = c(2, 20)) +
  #scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  scale_colour_gradient(low = "grey", high = "black") +
  #scale_colour_gradient2() +
  geom_abline() +
  #facet_wrap(~group) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(shape = "group", colour = "n")
  

d=data.frame(x1=c(-0.5,0,-0.5,0), x2=c(0,1,0,1), 
             y1=c(-0.5,0,0,-0.5), y2=c(0,1,1,0), 
             t=c('a','b', 'c','d'), 
             post_nc=c(0,1,0,1), # without this it will give error
             delay_nc=c(0,1,0,1))
ggplot(fdata_slfefcy, aes(x = delay_nc, y = post_nc)) + 
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, # without giving a different data it will create many rectangles over each other so the alpha value will become useless
                                ymin=y1, ymax=y2, 
                                fill=t), color="white", alpha=0.2, show.legend=FALSE) +
  geom_count(aes(colour = ..n.., size=1, alpha=0.9), shape=17) +
  #scale_colour_gradient(low = "grey", high = "black")
  scale_colour_gradient2() +
  geom_abline() +
  facet_wrap(~group)


means <- aggregate(post_eff_avg ~  group, fdata_slfefcy, FUN = function(x) {round(mean(x), digits=2)})
ggplot(fdata_slfefcy, aes(x=group, y=post_eff_avg, fill=group)) + 
  geom_boxplot() +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) +
geom_text(data = means, aes(label = post_eff_avg, y = post_eff_avg + 0.08), position = position_dodge(width = 0.78))



# cont vs exp
t.test(cont_fdata_slfefcy(fdata_slfefcy)$post_eff_avg, exp_fdata_slfefcy(fdata_slfefcy)$post_eff_avg, paired = F)

t.test(cont_fdata_slfefcy(fdata_slfefcy)$pre_eff_avg, cont_fdata_slfefcy(fdata_slfefcy)$post_eff_avg, paired = T)
t.test(exp_fdata_slfefcy(fdata_slfefcy)$pre_eff_avg, exp_fdata_slfefcy(fdata_slfefcy)$post_eff_avg, paired = T)

#### internal reliability
pre_slfEfcy <- left_join(pre_slfEfcy, fdata_slfefcy[, c('stid', 'group')], by=("stid"))
post_slfEfcy <- left_join(post_slfEfcy, fdata_slfefcy[, c('stid', 'group')], by=("stid"))

slf_efc_cont <- cont_fdata_slfefcy(post_slfEfcy)[, c('q1','q2','q3','q4','q5','q6','q7','q8')]
slf_efc_exp <- exp_fdata_slfefcy(post_slfEfcy)[, c('q1','q2','q3','q4','q5','q6','q7','q8')]
slf_efcAlpha<-alpha(slf_efc_cont)
slf_efcAlpha<-alpha(slf_efc_exp)
slf_efcAlpha
####
