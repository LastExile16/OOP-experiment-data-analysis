require('reshape2')
library('tidyr')
library('broom')
library('rstatix')
library('stringr')

CalculateNormalizedLearningGain = function(pre, post){
  
  c1 = pre
  c2 = post
  
  CalculateDiff = function(c1, c2){
    
    #c2 = c2[c2$parent2 %in% c1$parent2,]
    #c1 = c1[c1$parent2 %in% c2$parent2,]
    #c2 = c2[with(c2, order(parent2)),]
    #c1 = c1[with(c1, order(parent2)),]
    post = c2
    pre = c1
    diff = post-pre
    c2 = diff / (20-pre)
    
    c2
  }
  
  d1 = CalculateDiff(pre, post)
  d1$parent1 = "post - pre"
  d1
  
}

cont_fdata <- function(fdata) {
  within(fdata[fdata$group=='cont',], rm('group'))
}
exp_fdata <- function(fdata) {
  within(fdata[fdata$group=='exp',], rm('group'))
}

tukey_outlier <- function(col, data) {
  Q1 <- quantile(data[[col]], .25, na.rm = T)
  Q3 <- quantile(data[[col]], .75, na.rm = T)
  IQR <- IQR(data[[col]], na.rm = T)
  subset(data, data[[col]] > (Q1 - 1.5*IQR) & data[[col]] < (Q3 + 1.5*IQR))
}
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
mean_med_sd_prepostdelay <- function(data) {
  data %>% group_by(group) %>% summarise(
    pre_mean = mean(pre_score), pre_med = median(pre_score), pre_sd = sd(pre_score),
    post_mean = mean(post_score), post_med = median(post_score), post_sd = sd(post_score),
    delay_mean = mean(delay_score), delay_med = median(delay_score), delay_sd = sd(delay_score),
    n())
}
mean_med_sd_postdelay_nc <- function(data) {
  data %>% group_by(group) %>% summarise(
    post_mean = mean(post_nc), post_med = median(post_nc), post_sd = sd(post_nc),
    delay_mean = mean(delay_nc), delay_med = median(delay_nc), delay_sd = sd(delay_nc),
    n())
}
mean_med_sd <- function(data, ...) {
  print(list(...))
  formula = "data %>% group_by(group) %>% summarise("
  counter = 0
  size = length(list(...))
  for(i in list(...)) {
    counter = counter+1
    tmp = str_glue("{i}_mean = mean({i}), {i}_med = median({i}), {i}_sd = sd({i})")
    if(counter<size) {
      tmp = paste(tmp, ',')
    }else{
      tmp = paste(tmp, ')')
    }
    formula = paste(formula, tmp)
  }
  evformula = parse(text=formula)
  eval(evformula)
  #evformula
}
homogenity = function (v1, by, multi_col_data) {
  # homogenity test
  # F-test: Compare the variances of two groups. The data must be normally distributed.
  # Bartlett’s test: Compare the variances of two or more groups. The data must be normally distributed.
  # Levene’s test: A robust alternative to the Bartlett’s test that is less sensitive to departures from normality.
  # Fligner-Killeen’s test: a non-parametric test which is very robust against departures from normality.
  formula = as.formula(paste(v1, "~", by))
  print("fligner.test (used for non-normal data) [BEST]")
  x <- fligner.test(formula, multi_col_data) #used for non-normal data
  print(x)
  print("=====================================")
  print("leveneTest (used for non-normal data)")
  x <- car:: leveneTest(formula, multi_col_data)
  print(x)
  print("=====================================")
  print("bartlett.test (used for normal data)")
  bartlett.test(formula, multi_col_data) # used for normal data
}

glmByGroup = function(v1, by, multi_col_data) {
  x <- multi_col_data %>% group_by(group)
  message("groups")
  print(group_size(x))
  
  formula = as.formula(paste(v1, "~", by))
  tmp <- multi_col_data %>% group_by(group) %>%
    do(fitGroup = tidy(glm(formula, data = .))) %>% 
    unnest(fitGroup)
  
  message("glm tidy")
  print(tmp)
  
  tmp <- multi_col_data %>% group_by(group) %>%
    do(model = augment(glm(formula, data = .))) %>% 
    unnest(model)
  message("glm augment")
  print(tmp)
  
  tmp <- multi_col_data %>% group_by(group) %>%
    do(model = glance(lm(formula, data = .))) %>% 
    unnest(model)
  message("glm glance")
  print(tmp)
  
}

effSize = function(data, v1, v2, pair) {
  data <- data[!is.na(data[v2]),]
  oneColData <- rbind(data.frame("score" = data[[v1]], "type"=v1)
                      ,data.frame("score" = data[[v2]], "type"=v2))
  oneColData$type <- as.factor(oneColData$type)
  print('EFF SIZE 1 ==========')
  tmp <- effsize::cliff.delta(score ~ type, oneColData)
  print(tmp)
  print('EFF SIZE 2==========')
  print('wilcox_effsize')
  wilcox_effsize(score ~ type, data=oneColData, paired = pair)
}
GetDemographicsData = function (question_data) {
  demographics <- question_data[question_data$type=='demographic', c('userid', 'qid','ans')]
  demographics$qid <- as.character(demographics$qid)
  
  demographics[demographics$qid == '17',]$qid = 'age_group'
  demographics[demographics$qid == '18',]$qid = 'gender'
  demographics[demographics$qid == '19',]$qid = 'education'
  
  demographics$choice <- ""
  # set the age group
  demographics[demographics$ans == 44 & demographics$qid == 'age_group',]$choice = 'A'
  demographics[demographics$ans == 45 & demographics$qid == 'age_group',]$choice = 'B'
  demographics[demographics$ans == 46 & demographics$qid == 'age_group',]$choice = 'C'
  demographics[demographics$ans == 47 & demographics$qid == 'age_group',]$choice = 'D'
  demographics[demographics$ans == 48 & demographics$qid == 'age_group',]$choice = 'E'
  demographics[demographics$ans == 49 & demographics$qid == 'age_group',]$choice = 'F'
  
  # set the gender
  demographics[demographics$ans == 50 & demographics$qid == 'gender',]$choice = 'A'
  demographics[demographics$ans == 51 & demographics$qid == 'gender',]$choice = 'B'
  demographics[demographics$ans == 52 & demographics$qid == 'gender',]$choice = 'C'
  
  # set the education
  demographics[demographics$ans == 53 & demographics$qid == 'education',]$choice = 'A'
  demographics[demographics$ans == 54 & demographics$qid == 'education',]$choice = 'B'
  demographics[demographics$ans == 55 & demographics$qid == 'education',]$choice = 'C'
  demographics[demographics$ans == 56 & demographics$qid == 'education',]$choice = 'D'
  demographics[demographics$ans == 57 & demographics$qid == 'education',]$choice = 'E'
  demographics[demographics$ans == 58 & demographics$qid == 'education',]$choice = 'F'
  demographics[demographics$ans == 59 & demographics$qid == 'education',]$choice = 'G'
  demographics[demographics$ans == 60 & demographics$qid == 'education',]$choice = 'H'
  demographics[demographics$ans == 61 & demographics$qid == 'education',]$choice = 'I'
  #remove column ans
  demographics <- within(demographics, rm('ans'))
  
  #require(reshape2)
  # dcast(demographics, qid ~ userid, value.var="ans")
  # row to column by group
  demog_row_to_col = dcast(demographics, userid ~ qid, value.var="choice")
  sapply(demog_row_to_col, class)
  demog_row_to_col <- demog_row_to_col %>% mutate_if(is.character, as.factor)
  demog_row_to_col
}

GetCognitiveLoadData = function (question_data) {
  cognitiveLoad <- question_data[question_data$type=='cognitive', c('userid', 'qid','ans')]
  cognitiveLoad$qid <- as.character(cognitiveLoad$qid)
  
  cognitiveLoad[cognitiveLoad$qid == '42',]$qid = 'cog_difficulty'
  cognitiveLoad[cognitiveLoad$qid == '43',]$qid = 'cog_effort'
  
  cognitiveLoad$choice <- ""
  
  # set the cog_difficulty
  cognitiveLoad[cognitiveLoad$ans == 206 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '1'
  cognitiveLoad[cognitiveLoad$ans == 207 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '2'
  cognitiveLoad[cognitiveLoad$ans == 208 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '3'
  cognitiveLoad[cognitiveLoad$ans == 209 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '4'
  cognitiveLoad[cognitiveLoad$ans == 210 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '5'
  cognitiveLoad[cognitiveLoad$ans == 211 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '6'
  cognitiveLoad[cognitiveLoad$ans == 212 & cognitiveLoad$qid == 'cog_difficulty',]$choice = '7'
  
  # set the cog_effort
  cognitiveLoad[cognitiveLoad$ans == 213 & cognitiveLoad$qid == 'cog_effort',]$choice = '1'
  cognitiveLoad[cognitiveLoad$ans == 214 & cognitiveLoad$qid == 'cog_effort',]$choice = '2'
  cognitiveLoad[cognitiveLoad$ans == 215 & cognitiveLoad$qid == 'cog_effort',]$choice = '3'
  cognitiveLoad[cognitiveLoad$ans == 216 & cognitiveLoad$qid == 'cog_effort',]$choice = '4'
  cognitiveLoad[cognitiveLoad$ans == 217 & cognitiveLoad$qid == 'cog_effort',]$choice = '5'
  cognitiveLoad[cognitiveLoad$ans == 218 & cognitiveLoad$qid == 'cog_effort',]$choice = '6'
  cognitiveLoad[cognitiveLoad$ans == 219 & cognitiveLoad$qid == 'cog_effort',]$choice = '7'
  #remove column ans
  cognitiveLoad <- within(cognitiveLoad, rm('ans'))
  
  #require(reshape2)
  # dcast(cognitiveLoad, qid ~ userid, value.var="ans")
  # row to column by group
  cog_row_to_col = dcast(cognitiveLoad, userid ~ qid, value.var="choice")
  sapply(cog_row_to_col, class)
  cog_row_to_col <- cog_row_to_col %>% mutate_if(is.character, as.numeric)
  cog_row_to_col
}

GetFlowData = function (question_data) {
  flow <- question_data[question_data$type=='flow', c('userid', 'qid','ans')]
  flow$qid <- as.character(flow$qid)
  
  flow[flow$qid == '20',]$qid = 'flow01'
  flow[flow$qid == '21',]$qid = 'flow02'
  flow[flow$qid == '27',]$qid = 'flow03'
  flow[flow$qid == '28',]$qid = 'flow04'
  flow[flow$qid == '29',]$qid = 'flow05'
  flow[flow$qid == '30',]$qid = 'flow06'
  flow[flow$qid == '31',]$qid = 'flow07'
  flow[flow$qid == '32',]$qid = 'flow08'
  flow[flow$qid == '33',]$qid = 'flow09'
  flow[flow$qid == '34',]$qid = 'flow10'
  flow[flow$qid == '35',]$qid = 'importance01'
  flow[flow$qid == '36',]$qid = 'importance02'
  flow[flow$qid == '37',]$qid = 'importance03'
  flow[flow$qid == '38',]$qid = 'fun'
  flow[flow$qid == '39',]$qid = 'flow11'
  flow[flow$qid == '40',]$qid = 'flow12'
  flow[flow$qid == '41',]$qid = 'flow13'
  
  flow$choice <- ""
  
  # set the flow01
  flow[flow$ans == 62 & flow$qid == 'flow01',]$choice = '1'
  flow[flow$ans == 63 & flow$qid == 'flow01',]$choice = '2'
  flow[flow$ans == 64 & flow$qid == 'flow01',]$choice = '3'
  flow[flow$ans == 65 & flow$qid == 'flow01',]$choice = '4'
  flow[flow$ans == 66 & flow$qid == 'flow01',]$choice = '5'
  flow[flow$ans == 67 & flow$qid == 'flow01',]$choice = '6'
  flow[flow$ans == 68 & flow$qid == 'flow01',]$choice = '7'
  
  # set the flow02
  flow[flow$ans == 69 & flow$qid == 'flow02',]$choice = '1'
  flow[flow$ans == 70 & flow$qid == 'flow02',]$choice = '2'
  flow[flow$ans == 71 & flow$qid == 'flow02',]$choice = '3'
  flow[flow$ans == 72 & flow$qid == 'flow02',]$choice = '4'
  flow[flow$ans == 73 & flow$qid == 'flow02',]$choice = '5'
  flow[flow$ans == 74 & flow$qid == 'flow02',]$choice = '6'
  flow[flow$ans == 75 & flow$qid == 'flow02',]$choice = '7'
  
  
  # set the flow03
  flow[flow$ans == 101 & flow$qid == 'flow03',]$choice = '1'
  flow[flow$ans == 102 & flow$qid == 'flow03',]$choice = '2'
  flow[flow$ans == 103 & flow$qid == 'flow03',]$choice = '3'
  flow[flow$ans == 104 & flow$qid == 'flow03',]$choice = '4'
  flow[flow$ans == 105 & flow$qid == 'flow03',]$choice = '5'
  flow[flow$ans == 106 & flow$qid == 'flow03',]$choice = '6'
  flow[flow$ans == 107 & flow$qid == 'flow03',]$choice = '7'
  
  
  # set the flow04
  flow[flow$ans == 108 & flow$qid == 'flow04',]$choice = '1'
  flow[flow$ans == 109 & flow$qid == 'flow04',]$choice = '2'
  flow[flow$ans == 110 & flow$qid == 'flow04',]$choice = '3'
  flow[flow$ans == 111 & flow$qid == 'flow04',]$choice = '4'
  flow[flow$ans == 112 & flow$qid == 'flow04',]$choice = '5'
  flow[flow$ans == 113 & flow$qid == 'flow04',]$choice = '6'
  flow[flow$ans == 114 & flow$qid == 'flow04',]$choice = '7'
  
  
  # set the flow05
  flow[flow$ans == 115 & flow$qid == 'flow05',]$choice = '1'
  flow[flow$ans == 116 & flow$qid == 'flow05',]$choice = '2'
  flow[flow$ans == 117 & flow$qid == 'flow05',]$choice = '3'
  flow[flow$ans == 118 & flow$qid == 'flow05',]$choice = '4'
  flow[flow$ans == 119 & flow$qid == 'flow05',]$choice = '5'
  flow[flow$ans == 120 & flow$qid == 'flow05',]$choice = '6'
  flow[flow$ans == 121 & flow$qid == 'flow05',]$choice = '7'
  
  
  # set the flow06
  flow[flow$ans == 122 & flow$qid == 'flow06',]$choice = '1'
  flow[flow$ans == 123 & flow$qid == 'flow06',]$choice = '2'
  flow[flow$ans == 124 & flow$qid == 'flow06',]$choice = '3'
  flow[flow$ans == 125 & flow$qid == 'flow06',]$choice = '4'
  flow[flow$ans == 126 & flow$qid == 'flow06',]$choice = '5'
  flow[flow$ans == 127 & flow$qid == 'flow06',]$choice = '6'
  flow[flow$ans == 128 & flow$qid == 'flow06',]$choice = '7'
  
  
  # set the flow07
  flow[flow$ans == 129 & flow$qid == 'flow07',]$choice = '1'
  flow[flow$ans == 130 & flow$qid == 'flow07',]$choice = '2'
  flow[flow$ans == 131 & flow$qid == 'flow07',]$choice = '3'
  flow[flow$ans == 132 & flow$qid == 'flow07',]$choice = '4'
  flow[flow$ans == 133 & flow$qid == 'flow07',]$choice = '5'
  flow[flow$ans == 134 & flow$qid == 'flow07',]$choice = '6'
  flow[flow$ans == 135 & flow$qid == 'flow07',]$choice = '7'
  
  
  # set the flow08
  flow[flow$ans == 136 & flow$qid == 'flow08',]$choice = '1'
  flow[flow$ans == 137 & flow$qid == 'flow08',]$choice = '2'
  flow[flow$ans == 138 & flow$qid == 'flow08',]$choice = '3'
  flow[flow$ans == 139 & flow$qid == 'flow08',]$choice = '4'
  flow[flow$ans == 140 & flow$qid == 'flow08',]$choice = '5'
  flow[flow$ans == 141 & flow$qid == 'flow08',]$choice = '6'
  flow[flow$ans == 142 & flow$qid == 'flow08',]$choice = '7'
  
  
  # set the flow09
  flow[flow$ans == 143 & flow$qid == 'flow09',]$choice = '1'
  flow[flow$ans == 144 & flow$qid == 'flow09',]$choice = '2'
  flow[flow$ans == 145 & flow$qid == 'flow09',]$choice = '3'
  flow[flow$ans == 146 & flow$qid == 'flow09',]$choice = '4'
  flow[flow$ans == 147 & flow$qid == 'flow09',]$choice = '5'
  flow[flow$ans == 148 & flow$qid == 'flow09',]$choice = '6'
  flow[flow$ans == 149 & flow$qid == 'flow09',]$choice = '7'
  
  
  # set the flow10
  flow[flow$ans == 150 & flow$qid == 'flow10',]$choice = '1'
  flow[flow$ans == 151 & flow$qid == 'flow10',]$choice = '2'
  flow[flow$ans == 152 & flow$qid == 'flow10',]$choice = '3'
  flow[flow$ans == 153 & flow$qid == 'flow10',]$choice = '4'
  flow[flow$ans == 154 & flow$qid == 'flow10',]$choice = '5'
  flow[flow$ans == 155 & flow$qid == 'flow10',]$choice = '6'
  flow[flow$ans == 156 & flow$qid == 'flow10',]$choice = '7'
  
  
  # set the importance01
  flow[flow$ans == 157 & flow$qid == 'importance01',]$choice = '1'
  flow[flow$ans == 158 & flow$qid == 'importance01',]$choice = '2'
  flow[flow$ans == 159 & flow$qid == 'importance01',]$choice = '3'
  flow[flow$ans == 160 & flow$qid == 'importance01',]$choice = '4'
  flow[flow$ans == 161 & flow$qid == 'importance01',]$choice = '5'
  flow[flow$ans == 162 & flow$qid == 'importance01',]$choice = '6'
  flow[flow$ans == 163 & flow$qid == 'importance01',]$choice = '7'
  
  
  # set the importance02
  flow[flow$ans == 164 & flow$qid == 'importance02',]$choice = '1'
  flow[flow$ans == 165 & flow$qid == 'importance02',]$choice = '2'
  flow[flow$ans == 166 & flow$qid == 'importance02',]$choice = '3'
  flow[flow$ans == 167 & flow$qid == 'importance02',]$choice = '4'
  flow[flow$ans == 168 & flow$qid == 'importance02',]$choice = '5'
  flow[flow$ans == 169 & flow$qid == 'importance02',]$choice = '6'
  flow[flow$ans == 170 & flow$qid == 'importance02',]$choice = '7'
  
  
  # set the importance03
  flow[flow$ans == 171 & flow$qid == 'importance03',]$choice = '1'
  flow[flow$ans == 172 & flow$qid == 'importance03',]$choice = '2'
  flow[flow$ans == 173 & flow$qid == 'importance03',]$choice = '3'
  flow[flow$ans == 174 & flow$qid == 'importance03',]$choice = '4'
  flow[flow$ans == 175 & flow$qid == 'importance03',]$choice = '5'
  flow[flow$ans == 176 & flow$qid == 'importance03',]$choice = '6'
  flow[flow$ans == 177 & flow$qid == 'importance03',]$choice = '7'
  
  
  # set the fun
  flow[flow$ans == 178 & flow$qid == 'fun',]$choice = '1'
  flow[flow$ans == 179 & flow$qid == 'fun',]$choice = '2'
  flow[flow$ans == 180 & flow$qid == 'fun',]$choice = '3'
  flow[flow$ans == 181 & flow$qid == 'fun',]$choice = '4'
  flow[flow$ans == 182 & flow$qid == 'fun',]$choice = '5'
  flow[flow$ans == 183 & flow$qid == 'fun',]$choice = '6'
  flow[flow$ans == 184 & flow$qid == 'fun',]$choice = '7'
  
  
  # set the flow11
  flow[flow$ans == 185 & flow$qid == 'flow11',]$choice = '1'
  flow[flow$ans == 186 & flow$qid == 'flow11',]$choice = '2'
  flow[flow$ans == 187 & flow$qid == 'flow11',]$choice = '3'
  flow[flow$ans == 188 & flow$qid == 'flow11',]$choice = '4'
  flow[flow$ans == 189 & flow$qid == 'flow11',]$choice = '5'
  flow[flow$ans == 190 & flow$qid == 'flow11',]$choice = '6'
  flow[flow$ans == 191 & flow$qid == 'flow11',]$choice = '7'
  
  
  # set the flow12
  flow[flow$ans == 192 & flow$qid == 'flow12',]$choice = '1'
  flow[flow$ans == 193 & flow$qid == 'flow12',]$choice = '2'
  flow[flow$ans == 194 & flow$qid == 'flow12',]$choice = '3'
  flow[flow$ans == 195 & flow$qid == 'flow12',]$choice = '4'
  flow[flow$ans == 196 & flow$qid == 'flow12',]$choice = '5'
  flow[flow$ans == 197 & flow$qid == 'flow12',]$choice = '6'
  flow[flow$ans == 198 & flow$qid == 'flow12',]$choice = '7'
  
  
  # set the flow13
  flow[flow$ans == 199 & flow$qid == 'flow13',]$choice = '1'
  flow[flow$ans == 200 & flow$qid == 'flow13',]$choice = '2'
  flow[flow$ans == 201 & flow$qid == 'flow13',]$choice = '3'
  flow[flow$ans == 202 & flow$qid == 'flow13',]$choice = '4'
  flow[flow$ans == 203 & flow$qid == 'flow13',]$choice = '5'
  flow[flow$ans == 204 & flow$qid == 'flow13',]$choice = '6'
  flow[flow$ans == 205 & flow$qid == 'flow13',]$choice = '7'
  
  
  #remove column ans
  flow <- within(flow, rm('ans'))
  
  #require(reshape2)
  # dcast(flow, qid ~ userid, value.var="ans")
  # row to column by group
  flow_row_to_col = dcast(flow, userid ~ qid, value.var="choice")
  sapply(flow_row_to_col, class)
  flow_row_to_col <- flow_row_to_col %>% mutate_if(is.character, as.numeric)
  flow_row_to_col
}
