library("psych")
library('readr')

tam_data <- read_csv("./TAM_OOP_data.csv", col_types=c('stid'='c'))
pu<-tam_data[, c('Q1','Q2','Q3','Q4','Q5')]
peou<-tam_data[,c('Q6','Q7','Q8','Q9')]
at<-tam_data[, c('Q10','Q11','Q12','Q13')]
bi<-tam_data[, c('Q14','Q15')]
c<-tam_data[, c('Q16','Q17','Q18')]
e<-tam_data[, c('Q19','Q20', 'Q21')]

puAlpha<-alpha(pu)
peouAlpha<-alpha(peou)
atAlpha<-alpha(at)
biAlpha<-alpha(bi)
cAlpha<-alpha(c)
eAlpha<-alpha(e)

puAlpha
peouAlpha
atAlpha
biAlpha
cAlpha
eAlpha


tam_data$pu <- rowMeans(tam_data[, c('Q1','Q2','Q3','Q4','Q5')])
tam_data$peou <- rowMeans(tam_data[,c('Q6','Q7','Q8','Q9')])
tam_data$at <- rowMeans(tam_data[, c('Q10','Q11','Q12','Q13')])
tam_data$bi <- rowMeans(tam_data[, c('Q14','Q15')])
tam_data$c <- rowMeans(tam_data[, c('Q16','Q17','Q18')])
tam_data$e <- rowMeans(tam_data[, c('Q19','Q20', 'Q21')])

par(mfrow=c(2,3))

hist(tam_data$pu)
hist(tam_data$peou)
hist(tam_data$at)
hist(tam_data$bi)
hist(tam_data$c)
hist(tam_data$e)
par(mfrow=c(1,1))

shapiro.test(tam_data$pu)
shapiro.test(tam_data$peou)
shapiro.test(tam_data$at)
shapiro.test(tam_data$bi)
shapiro.test(tam_data$c)
shapiro.test(tam_data$e)


describeBy(tam_data[,c("pu", "peou", "at", "bi", "c", "e")])

# H1
at_pu = lm(at ~ pu, data = tam_data)
# H2
at_peou = lm(at ~ peou, data = tam_data)

at_puPeou = lm(at ~ pu+peou, data = tam_data)

# H3
bi_pu = lm(bi ~ pu, data = tam_data)
# H4
bi_at = lm(bi ~ at, data = tam_data)

bi_puAT = lm(bi ~ pu+at, data = tam_data)


# H5
pu_peou = lm(pu ~ peou, data = tam_data)
# h6
pu_C = lm(pu ~ c, data = tam_data)
# H7
pu_e = lm(pu ~ e, data = tam_data)

pu_PeouCE = lm(pu ~ peou+c+e, data = tam_data)

# H8
peou_C = lm(peou ~ c, data = tam_data)
# H9
peou_e = lm(peou ~ e, data = tam_data)

peou_CE = lm(peou ~ c+e, data = tam_data)

summary(at_pu)
summary(at_peou)
summary(at_puPeou)

summary(bi_pu)
summary(bi_at)
summary(bi_puAT)

summary(pu_peou)
summary(pu_C)
summary(pu_e)
summary(pu_PeouCE)

summary(peou_C)
summary(peou_e)
summary(peou_CE)



BITest <- tam_data[, c('pu', 'at', 'bi')]
BITest <- BITest[-nrow(BITest), ]

g <- data.frame("score" = BITest$pu, "type"='pu')
g2 <- data.frame("score" = BITest$at, "type"='at')
g3 <- data.frame("score" = BITest$bi, "type"='bi')
groupedData <- rbind(g,g2,g3)
groupedData$type <- as.factor(groupedData$type)

#library(Rmisc)
#sum = summarySE(BITest, measurevar="bi", groupvars=c("pu","at"))
