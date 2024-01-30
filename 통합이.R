library(sas7bdat)
library(dplyr)
library(rpart)
library(rpart.plot)
library(survey)
library(svyglm)
library(svydesign)
library(survey)
library(ggplot2)
library(purrr)
library(tidyr)
library(ggplot2)


table(HNP_All1$Body.weight.perception)
table(HNP_All2$Body.weight.perception)
table(HNP_All3$Body.weight.perception)
table(HNP_All4$Body.weight.perception)

te1<-cbind(HN10$BO1,HN10$age,HN10$HE_BMI,HN10$LW_pr_1)
te2<-cbind(HN11$BO1,HN11$age,HN11$HE_BMI,HN11$LW_pr_1)
te3<-cbind(HN12$BO1,HN12$age,HN12$HE_BMI,HN12$LW_pr_1)
te4<-cbind(HN13$BO1,HN13$age,HN13$HE_BMI,HN13$LW_pr_1)
te5<-cbind(HN14$BO1,HN14$age,HN14$HE_BMI,HN14$LW_pr_1)
te6<-cbind(HN15$BO1,HN15$age,HN15$HE_BMI,HN15$LW_pr_1)
te7<-cbind(HN16$BO1,HN16$age,HN16$HE_BMI,HN16$LW_pr_1)
te<-rbind(te1,te2,te3,te4,te5,te6,te7)
te<-as.data.frame(te)
names(te)=c("체형인식","나이","BMI","임신횟수")
A<-te

A<-HNNNE

A$ageg = 1
A$ageg[A$나이<=18]=1
A$ageg[A$나이>=19 & A$나이 <46]=2
A$ageg[A$나이>=46 & A$나이 <60]=3
A$ageg[A$나이>=60]=4
table(A$ageg, A$임신횟수)
table(HNP1$음주빈도)

rm(HNN16)

attach(HN10)
HNN10<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, LW_mp, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN10)
attach(HN11)
HNN11<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, LW_mp, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN11)
attach(HN12)
HNN12<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, LW_mp, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN12)
attach(HN13)
HNN13<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, LW_mp, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN13)
attach(HN14)
HNN14<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN14)
attach(HN15)
HNN15<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN15)
attach(HN16)
HNN16<-cbind(HE_BMI ,	wt_hs,	marri_1,	EC1_1,	edu, age,	kstrata,	incm,	LW_ms, BD1_11,	LW_pr,	D_1_1,	BO3_01,	BO1,	BS3_1, LQ_4EQL, LQ_5EQL, LW_pr_1, LW_mt, DI1_dg, DI2_dg, DE1_dg, DF2_dg, DF1_yd, D_4_1, EC_wht_23,BO2_1, BE3_31, BE5_1)			
detach(HN16)



table(HN14$LW_mp)

write.csv(HN14,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HN14.csv")
write.csv(HNN14,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNN14.csv")

write.csv(HN15,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HN15.csv")
write.csv(HNN15,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNN15.csv")

HNN14<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNN14.csv")
HNN15<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNN15.csv")

HNND1<-rbind(HNN10,HNN11,HNN12,HNN13)
HNND2<-rbind(HNN14,HNN15,HNN16)
  
write.csv(HNND1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNND1.csv")
write.csv(HNND2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNND2.csv")

HNND2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNND2.csv")
HNP_All<-rbind(HNND1,HNND2)


HNP_All <- transform(HNP_All, 체형인식 = ifelse(체형인식 > 3, 1, 0))
HNP_All <- na.omit(HNP_All)


write.csv(HNP_All,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All.csv")
HNP_Alll<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All.csv")


write.csv(HNP_Pog_scale,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_ASD.csv")
HNP_Pog_scale<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_ASD.csv")


HNP_Alll<-HNP_All
HNP_Alll$

rm(df)

HNP_Pog<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_Pog.csv")
#시작
HNP_Pog_scale<-HNP_Pog


HNP_Alll %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#스케일링, factor 처리

#rename(테이블이름, "바꿀 이름" = "원래 이름")
HNP_Pog_scale <- rename(HNP_Pog_scale, "Annual.personal.income.level" = "X.Annual.personal.income.level")
HNP_Pog_scale[,-c(10:13)]<-scale(HNP_Pog_scale[,-c(10:13)])
HNP_Pog_scale$Weight.loss.intervention<-factor(HNP_Pog_scale$Weight.loss.intervention)
#HNP_Pog_scale$나이<-factor(HNP_Pog_scale$나이)

attach(HNP_Pog_scale)
HNP_Pog_scale<- within((HNP_Pog_scale), Weight.loss.intervention <- relevel(Weight.loss.intervention, ref = 1))
#HNP_Pog_scale<- within((HNP_Pog_scale), 나이 <- relevel(나이, ref = 1))

#glm
attach(HNP_Pog_scale)
dstrat_all<-svydesign(id=~1,strata=~분산추정층, weights=~가구가중치, data=HNP_Pog_scale,family= binomial)
dstrat_all_result<-svyglm(Body.weight.perception~Age + Annual.personal.income.level +	Education.level +	
                            Marital.status +	Perceived.health.status +
                            Work.status +	Annual.drink.consumption +	
                            Previous.pregnancy.experience +	Menopause.status +	Weight.loss.intervention,design=dstrat_all)

summary(svyglm(Body.weight.perception~Age + Annual.personal.income.level +	Education.level +	
                 Marital.status +	Perceived.health.status +
                 Work.status +	Annual.drink.consumption +	
                 Previous.pregnancy.experience +	Menopause.status +	Weight.loss.intervention,design=dstrat_all))

detach(HNP_Pog_scale)

write.csv(trainall,"C:/Users/User/Desktop/JooLee_KoreanHN_data/train(전체포괄).csv")
write.csv(testall,"C:/Users/User/Desktop/JooLee_KoreanHN_data/test(전체포괄).csv")
write.csv(trainsep1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/train(전체세부).csv")
write.csv(testsep1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/t(전체세부).csv")
write.csv(atrainp1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/train(18세).csv")
write.csv(atestp1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/test(18세).csv")
write.csv(atrainp2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/train(19-45).csv")
write.csv(atestp2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/test(19-45).csv")
write.csv(atrainp3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/train(46-59).csv")
write.csv(atestp3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/test(46-59).csv")
write.csv(atrainp4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/train(60세).csv")
write.csv(atestp4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/test(60세).csv")


HNP_Pog_scale$분산추정층<-NULL
HNP_Pog_scale$가구가중치<-NULL

HNP_Pog_scale$체형인식<-factor(HNP_Pog_scale$체형인식)
HNP_Pog_scale<- downSample(subset(HNP_Pog_scale, select=-체형인식), HNP_Pog_scale$체형인식)
set.seed(123)
idx<-sample(1:nrow(HNP_Pog_scale),nrow(HNP_Pog_scale)*0.7,replace=FALSE)
trainall<-HNP_Pog_scale[idx,]
testall<-HNP_Pog_scale[-idx,]

library(Epi)
library(adabag)
bagall1<-bagging(Class~.,data=trainall,mfinal=15)
names(bagall1)
bagall1$importance
pred.bagall1<-predict(bagall1,testall,type="class")
confusionMatrix(data=as.factor(pred.bagall1$class),reference=as.factor(testall$Class),positive='0')
bag_ROC <- ROC(form=Class~pred.bagall1$class, data=testall, plot="ROC")

testall
pred.bagall1

#로지스틱 회귀분석
logistic1<-glm(Class~.,data=trainall,family="binomial")
summary(logistic1)

predl1<-predict(logistic1,testall[,-11],type="response")
predl1<-as.data.frame(predl1)
predl1$grade<-ifelse(predl1$pred<0.5,predl1$grade<-0,predl1$grade<-1)
confusionMatrix(data=as.factor(predl1$grade),reference=testall[,11],positive='0')

bag_ROC <- ROC(form=Class~predl1$grade, data=testall, plot="ROC")


#세부변수 관련
HNP_sep_scale<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_sep_scale1.csv")
HNP_sep_scale$Pain.or.discomfort.status<-HNP_sep_scale$Pain.or.discomfort.status
HNP_sep_scale$Anxiety.or.depression.status<-HNP_sep_scale$Anxiety.or.depression.status


#히스토그램
HNP_sep_scale %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

HNP_sep_scale[,-c(6:13)]<-scale(HNP_sep_scale[,-c(6:13)])


HNP_sep_scale$Exercise.status.for.weight.loss<-factor(HNP_sep_scale$Exercise.status.for.weight.loss)
HNP_sep_scale$Pain.or.discomfort.status<-factor(HNP_sep_scale$Pain.or.discomfort.status)
HNP_sep_scale$Anxiety.or.depression.status<-factor(HNP_sep_scale$Anxiety.or.depression.status)
HNP_sep_scale$Weight.loss.intervention<-factor(HNP_sep_scale$Weight.loss.intervention)
HNP_sep_scale$Menopause.type<-factor(HNP_sep_scale$Menopause.type)
HNP_sep_scale$Marital.status<-factor(HNP_sep_scale$Marital.status)



HNP_sep_scale<- within((HNP_sep_scale), Menopause.type <- relevel(Menopause.type, ref = 1))
HNP_sep_scale<- within((HNP_sep_scale), Exercise.status.for.weight.loss <- relevel(Exercise.status.for.weight.loss, ref = 1))
HNP_sep_scale<- within((HNP_sep_scale), Weight.loss.intervention <- relevel(Weight.loss.intervention, ref = 1))
HNP_sep_scale<- within((HNP_sep_scale), Pain.or.discomfort.status <- relevel(Pain.or.discomfort.status, ref = 1))
HNP_sep_scale<- within((HNP_sep_scale), Anxiety.or.depression.status <- relevel(Anxiety.or.depression.status, ref = 1))
HNP_sep_scale<- within((HNP_sep_scale), Marital.status <- relevel(Marital.status, ref = 1))

HNP_sep_scale

#glm
attach(HNP_sep_scale)
dstrat_sep<-svydesign(id=~1,strata=~분산추정층, weights=~가구가중치, data=HNP_sep_scale)
dstrat_sep_result<-svyglm(Body.weight.perception~Age+Education.level+Perceived.health.status+
                            Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+
                            Pain.or.discomfort.status+Anxiety.or.depression.status+Weight.loss.intervention+
                            Exercise.status.for.weight.loss+Alcohol.consumption+Gravidity+Income+Marital.status,design=dstrat_sep)
summary(svyglm(Body.weight.perception~Age+Education.level+Perceived.health.status+
                 Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+
                 Pain.or.discomfort.status+Anxiety.or.depression.status+Weight.loss.intervention+
                 Exercise.status.for.weight.loss+Alcohol.consumption+Gravidity+Income+Marital.status,design=dstrat_sep))
detach(HNP_sep_scale)

HNP_sep_scale$분산추정층<-NULL
HNP_sep_scale$가구가중치<-NULL

HNP_sep_scale$체형인식<-factor(HNP_sep_scale$체형인식)
set.seed(123)
HNP_sep_scale<- downSample(subset(HNP_sep_scale, select=-체형인식), HNP_sep_scale$체형인식)
idx<-sample(1:nrow(HNP_sep_scale),nrow(HNP_sep_scale)*0.7,replace=FALSE)
trainsep1<-HNP_sep_scale[idx,]
testsep1<-HNP_sep_scale[-idx,]

bagsep<-bagging(Class~.,data=trainsep1,mfinal=15)
names(bagsep)
bagsep$importance
pred.bagsep<-predict(bagsep,testsep1,type="class")
confusionMatrix(data=as.factor(pred.bagsep$class),reference=as.factor(testsep1$Class),positive='0')


bag_ROC1 <- ROC(form=Class~pred.bagsep$class, data=testsep1, plot="ROC")

#로지스틱
logistic2<-glm(Class~.,data=trainsep1,family="binomial")
summary(logistic2)

predl2<-predict(logistic2,testsep1[,-11],type="response")
predl2<-as.data.frame(predl2)
predl2$grade<-ifelse(predl2$pred<0.5,predl2$grade<-0,predl2$grade<-1)
confusionMatrix(data=as.factor(predl2$grade),reference=testsep1[,11],positive='0')

bag_ROC1 <- ROC(form=Class~predl2$grade, data=testsep1, plot="ROC")

table(HNP_All2$Depression)

#데이터 나이대별로 분리
HNP_All<-HNP_All %>% filter(HNP_All$나이 <=18)
HNP_All2<-HNP_All %>% filter(HNP_All$나이 >18 & HNP_All$나이<=45)
HNP_All3<-HNP_All %>% filter(HNP_All$나이 >=46 & HNP_All$나이<=59)
HNP_All4<-HNP_All %>% filter(HNP_All$나이 >=60)

write.csv(HNP_All1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All1.csv")
HNP_All1<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All1.csv")
write.csv(HNP_All2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All2.csv")
HNP_All2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All2.csv")
write.csv(HNP_All3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All3.csv")
HNP_All3<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All3.csv")
write.csv(HNP_All4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All4.csv")
HNP_All4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All4.csv")

HNP_All2p<-HNP_All2

HNP_All1<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All1.csv")
HNP_All2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All2.csv")
HNP_All3<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All3.csv")
HNP_All4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/HNP_All4-1.csv")

HNP_All2_scalep

table(HNP_All1$Body.weight.perception)
table(HNP_All2$Body.weight.perception)
table(HNP_All3$Body.weight.perception)
table(HNP_All4$Body.weight.perception)

table(HNP_All4$Menopause.type)



#히스토그램
HNP_All2 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
HNP_All1$현재흡연여부<-NULL

fivenum(HNP_All2$임신횟수)
table(HNP_All2$임신횟수)
fivenum(HNP_All3$임신횟수)
table(HNP_All3$임신횟수)
fivenum(HNP_All4$임신횟수)
table(HNP_All4$임신횟수)

#factor처리
HNP_All1_scalep<-HNP_All1
HNP_All2_scalep<-HNP_All2
HNP_All3_scalep<-HNP_All3
HNP_All4_scalep<-HNP_All4

HNP_All1_scalep$Weight.loss.intervention<-factor(HNP_All1_scalep$Weight.loss.intervention)
HNP_All1_scalep$Exercise.status.for.weight.loss<-factor(HNP_All1_scalep$Exercise.status.for.weight.loss)


HNP_All2_scalep$Exercise.status.for.weight.loss<-factor(HNP_All2_scalep$Exercise.status.for.weight.loss)
HNP_All2_scalep$Weight.loss.intervention<-factor(HNP_All2_scalep$Weight.loss.intervention)
HNP_All2_scalep$Pain.or.discomfort.status<-factor(HNP_All2_scalep$Pain.or.discomfort.status)
HNP_All2_scalep$Anxiety.or.depression.status<-factor(HNP_All2_scalep$Anxiety.or.depression.status)


HNP_All3_scalep$Exercise.status.for.weight.loss<-factor(HNP_All3_scalep$Exercise.status.for.weight.loss)
HNP_All3_scalep$Weight.loss.intervention<-factor(HNP_All3_scalep$Weight.loss.intervention)
HNP_All3_scalep$Pain.or.discomfort.status<-factor(HNP_All3_scalep$Pain.or.discomfort.status)
HNP_All3_scalep$Anxiety.or.depression.status<-factor(HNP_All3_scalep$Anxiety.or.depression.status)


HNP_All4_scalep$Weight.loss.intervention<-factor(HNP_All4_scalep$Weight.loss.intervention)
HNP_All4_scalep$Exercise.status.for.weight.loss<-factor(HNP_All4_scalep$Exercise.status.for.weight.loss)
HNP_All4_scalep$Menopause.type<-factor(HNP_All4_scalep$Menopause.type)
HNP_All4_scalep$Pain.or.discomfort.status<-factor(HNP_All4_scalep$Pain.or.discomfort.status)
HNP_All4_scalep$Anxiety.or.depression.status<-factor(HNP_All4_scalep$Anxiety.or.depression.status)


HNP_All4_scalep<-HNP_All4_scalep[-2452,]

#스케일링
HNP_All1_scalep[,-c(10:15)]<-scale(HNP_All1_scalep[,-c(10:16)])
HNP_All2_scalep[,-c(9:18)]<-scale(HNP_All2_scalep[,-c(9:18)])
HNP_All3_scalep[,-c(13:21)]<-scale(HNP_All3_scalep[,-c(13:21)])
HNP_All4_scalep[,-c(13:21)]<-scale(HNP_All4_scalep[,-c(13:21)])

HNP_All1_scalep<- within((HNP_All1_scalep), Exercise.status.for.weight.loss <- relevel(Exercise.status.for.weight.loss, ref = 1))
HNP_All1_scalep<- within((HNP_All1_scalep), Weight.loss.intervention <- relevel(Weight.loss.intervention, ref = 1))

HNP_All2_scalep<- within((HNP_All2_scalep), Exercise.status.for.weight.loss <- relevel(Exercise.status.for.weight.loss, ref = 1))
HNP_All2_scalep<- within((HNP_All2_scalep), Weight.loss.intervention <- relevel(Weight.loss.intervention, ref = 1))
HNP_All2_scalep<- within((HNP_All2_scalep), Pain.or.discomfort.status <- relevel(Pain.or.discomfort.status, ref = 1))
HNP_All2_scalep<- within((HNP_All2_scalep), Anxiety.or.depression.status <- relevel(Anxiety.or.depression.status, ref = 1))

HNP_All3_scalep<- within((HNP_All3_scalep), Exercise.status.for.weight.loss <- relevel(Exercise.status.for.weight.loss, ref = 1))
HNP_All3_scalep<- within((HNP_All3_scalep), Weight.loss.intervention <- relevel(Weight.loss.intervention, ref = 1))
HNP_All3_scalep<- within((HNP_All3_scalep), Pain.or.discomfort.status <- relevel(Pain.or.discomfort.status, ref = 1))
HNP_All3_scalep<- within((HNP_All3_scalep), Anxiety.or.depression.status <- relevel(Anxiety.or.depression.status, ref = 1))

HNP_All4_scalep<- within((HNP_All4_scalep), Menopause.type <- relevel(Menopause.type, ref = 1))
HNP_All4_scalep<- within((HNP_All4_scalep), Exercise.status.for.weight.loss <- relevel(Exercise.status.for.weight.loss, ref = 1))
HNP_All4_scalep<- within((HNP_All4_scalep), Weight.loss.intervention <- relevel(Weight.loss.intervention, ref = 1))
HNP_All4_scalep<- within((HNP_All4_scalep), Pain.or.discomfort.status <- relevel(Pain.or.discomfort.status, ref = 1))
HNP_All4_scalep<- within((HNP_All4_scalep), Anxiety.or.depression.status <- relevel(Anxiety.or.depression.status, ref = 1))

#glm
attach(HNP_All1_scalep)
dstrat_sep1<-svydesign(id=~1,strata=~분산추정층, weights=~가구가중치, data=HNP_All1_scalep)
dstrat_sep1_result<-svyglm(Body.weight.perception~Education.level+Annual.personal.income.level+
                             Carbide.status+Annual.drink.consumption+Perceived.health.status+Disease.status.for.Adolescence.+
                             Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+Exercise.status.for.weight.loss+Weight.loss.intervention
                   ,design=dstrat_sep1)
summary(svyglm(Body.weight.perception~Education.level+Annual.personal.income.level+
                 Carbide.status+Annual.drink.consumption+Perceived.health.status+Disease.status.for.Adolescence.+
                 Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+Exercise.status.for.weight.loss+Weight.loss.intervention
               ,design=dstrat_sep1))
detach(HNP_All1_scalep)

attach(HNP_All2_scalep)
dstrat_sep2<-svydesign(id=~1,strata=~분산추정층, weights=~가구가중치, data=HNP_All2_scalep)
dstrat_sep2_result<-svyglm(Body.weight.perception~Marital.status+Education.level+Annual.personal.income.level+Annual.drink.consumption+Perceived.health.status+Previous.pregnancy.times+Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+Pain.or.discomfort.status+Anxiety.or.depression.status+Weight.loss.intervention+Exercise.status.for.weight.loss+Menopause.status+Depression,design=dstrat_sep2)
summary(svyglm(Body.weight.perception~Marital.status+Education.level+Annual.personal.income.level+Annual.drink.consumption+Perceived.health.status+Previous.pregnancy.times+Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+Pain.or.discomfort.status+Anxiety.or.depression.status+Weight.loss.intervention+Exercise.status.for.weight.loss+Menopause.status+Depression,design=dstrat_sep2))
detach(HNP_All2_scalep)

attach(HNP_All3_scalep)
dstrat_sep3<-svydesign(id=~1,strata=~분산추정층, weights=~가구가중치, data=HNP_All3_scalep)
dstrat_sep3_result<-svyglm(Body.weight.perception~Marital.status+Education.level+
                             Annual.personal.income.level+Annual.drink.consumption+
                             Previous.pregnancy.times+Perceived.health.status+
                             Pain.or.discomfort.status+Anxiety.or.depression.status+
                             Self.reported.hypertension+
                             Self.reported.hyperlipidemia+
                             Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+
                             Menopause.status+Weight.loss.intervention+Exercise.status.for.weight.loss,design=dstrat_sep3)

summary(svyglm(Body.weight.perception~Marital.status+Education.level+
                 Annual.personal.income.level+Annual.drink.consumption+
                 Previous.pregnancy.times+Perceived.health.status+
                 Pain.or.discomfort.status+Anxiety.or.depression.status+
                 Self.reported.hypertension+
                 Self.reported.hyperlipidemia+
                 Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+
                 Menopause.status+Weight.loss.intervention+Exercise.status.for.weight.loss,design=dstrat_sep3))
detach(HNP_All3_scalep)

attach(HNP_All4_scalep)
dstrat_sep4<-svydesign(id=~1,strata=~분산추정층, weights=~가구가중치, data=HNP_All4_scalep)
dstrat_sep4_result<-svyglm(Body.weight.perception~Marital.status+Education.level+
                             Annual.personal.income.level+Annual.drink.consumption+
                             Previous.pregnancy.times+Perceived.health.status+
                             Pain.or.discomfort.status+Anxiety.or.depression.status
                             +Self.reported.hypertension+
                             Self.reported.hyperlipidemia+Self.reported.diabetes..status.+
                             Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+
                             Menopause.type+Weight.loss.intervention+Exercise.status.for.weight.loss,design=dstrat_sep4)
summary(svyglm(Body.weight.perception~Marital.status+Education.level+
                 Annual.personal.income.level+Annual.drink.consumption+
                 Previous.pregnancy.times+Perceived.health.status+
                 Pain.or.discomfort.status+Anxiety.or.depression.status
                 +Self.reported.hypertension+
                 Self.reported.hyperlipidemia+Self.reported.diabetes..status.+
                 Days.of.walking..per.week.+Days.of.anaerobic.workout.per.week.+
                 Menopause.type+Weight.loss.intervention+Exercise.status.for.weight.loss,design=dstrat_sep4))
detach(HNP_All4_scalep)

#배깅
HNP_All1_scalep$분산추정층<-NULL
HNP_All1_scalep$가구가중치<-NULL
HNP_All1_scalep$나이<-NULL
HNP_All1_scalep$BMI<-NULL


as.data.frame(HNP_All1_scalep)
HNP_All1_scalep$체형인식<-factor(HNP_All1_scalep$체형인식)
HNP_All1_scalep<- downSample(subset(HNP_All1_scalep, select=-체형인식), HNP_All1_scalep$체형인식)

set.seed(123)
idx<-sample(1:nrow(HNP_All1_scalep),nrow(HNP_All1_scalep)*0.7,replace=FALSE)
atrainp1<-HNP_All1_scalep[idx,]
atestp1<-HNP_All1_scalep[-idx,]

library(adabag)
abagp1<-bagging(Class~.,data=atrainp1,mfinal=15)
names(abagp1)
abagp1$importance
apred.bagp1<-predict(abagp1,atestp1,type="class")
confusionMatrix(data=as.factor(apred.bagp1$class),reference=as.factor(atestp1$Class),positive='0')

bag_ROC <- ROC(form=Class~apred.bagp1$class, data=atestp1, plot="ROC")


logistic3<-glm(Class~.,data=atrainp1,family="binomial")
summary(logistic3)

predl3<-predict(logistic3,atestp1[,-12],type="response")
predl3<-as.data.frame(predl3)
predl3$grade<-ifelse(predl3$pred<0.5,predl3$grade<-0,predl3$grade<-1)
confusionMatrix(data=as.factor(predl3$grade),reference=atestp1[,12],positive='0')

bag_ROC <- ROC(form=Class~predl3$grade, data=atestp1, plot="ROC")


#2
HNP_All2_scalep$분산추정층<-NULL
HNP_All2_scalep$가구가중치<-NULL
HNP_All2_scalep$나이<-NULL
HNP_All2_scalep$BMI<-NULL


HNP_All2_scalep$체형인식<-factor(HNP_All2_scalep$체형인식)
set.seed(123)
HNP_All2_scalep<- downSample(subset(HNP_All2_scalep, select=-체형인식), HNP_All2_scalep$체형인식)
idx<-sample(1:nrow(HNP_All2_scalep),nrow(HNP_All2_scalep)*0.7,replace=FALSE)
atrainp2<-HNP_All2_scalep[idx,]
atestp2<-HNP_All2_scalep[-idx,]

atrainp2

library(adabag)
abagp2<-bagging(Class~.,data=atrainp2,mfinal=15)
names(abagp2)
abagp2$importance
apred.bagp2<-predict(abagp2,atestp2,type="class")
confusionMatrix(data=as.factor(apred.bagp2$class),reference=as.factor(atestp2$Class),positive='0')

bag_ROC <- ROC(form=Class~apred.bagp2$class, data=atestp2, plot="ROC")

logistic4<-glm(Class~.,data=atrainp2,family="binomial")
summary(logistic4)

predl4<-predict(logistic4,atestp2[,-15],type="response")
predl4<-as.data.frame(predl4)
predl4$grade<-ifelse(predl4$pred<0.5,predl4$grade<-0,predl4$grade<-1)
confusionMatrix(data=as.factor(predl4$grade),reference=atestp2[,15],positive='0')

bag_ROC <- ROC(form=Class~predl4$grade, data=atestp2, plot="ROC")

#3
HNP_All3_scalep$분산추정층<-NULL
HNP_All3_scalep$가구가중치<-NULL
HNP_All3_scalep$나이<-NULL
HNP_All3_scalep$BMI<-NULL
HNP_All4_scalep$분산추정층<-NULL
HNP_All4_scalep$가구가중치<-NULL
HNP_All4_scalep$나이<-NULL
HNP_All4_scalep$BMI<-NULL

HNP_All3_scalep$Body.weight.perception<-factor(HNP_All3_scalep$Body.weight.perception)
HNP_All4_scalep$Body.weight.perception<-factor(HNP_All4_scalep$Body.weight.perception)


X1<- downSample(subset(HNP_All3_scalep, select=-Body.weight.perception), HNP_All3_scalep$Body.weight.perception)
X2<- downSample(subset(HNP_All4_scalep, select=-Body.weight.perception), HNP_All4_scalep$Body.weight.perception)


set.seed(123)
idx<-sample(1:nrow(X1),nrow(X1)*0.7,replace=FALSE)
atrainp3<-X1[idx,]
atestp3<-X1[-idx,]

idx<-sample(1:nrow(X2),nrow(X2)*0.7,replace=FALSE)
atrainp4<-X2[idx,]
atestp4<-X2[-idx,]

write.csv(atrainp1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/train1.csv")
write.csv(atrainp2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/train2.csv")
write.csv(atrainp3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/train3.csv")
write.csv(atrainp4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/train4.csv")


write.csv(atestp1,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/test1.csv")
write.csv(atestp2,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/test2.csv")
write.csv(atestp3,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/test3.csv")
write.csv(atestp4,"C:/Users/User/Desktop/JooLee_KoreanHN_data/데이터/test4.csv")

library(adabag)
abagp3<-bagging(Class~.,data=atrainp3,mfinal=15)
names(abagp3)
abagp3$importance
apred.bagp3<-predict(abagp3,atestp3,type="class")
confusionMatrix(data=as.factor(apred.bagp3$class),reference=as.factor(atestp3$Class),positive='0')

bag_ROC <- ROC(form=Class~apred.bagp3$class, data=atestp3, plot="ROC")


logistic5<-glm(Class~.,data=atrainp3,family="binomial")
summary(logistic5)

predl5<-predict(logistic5,atestp3[,-19],type="response")
predl5<-as.data.frame(predl5)
predl5$grade<-ifelse(predl5$pred<0.5,predl5$grade<-0,predl5$grade<-1)
confusionMatrix(data=as.factor(predl5$grade),reference=atestp3[,19],positive='0')

bag_ROC <- ROC(form=Class~predl5$grade, data=atestp3, plot="ROC")


abagp6<-bagging(Class~.,data=atrainp4,mfinal=15)
names(abagp6)
abagp6$importance
apred.bagp6<-predict(abagp6,atestp4,type="class")
confusionMatrix(data=as.factor(apred.bagp6$class),reference=as.factor(atestp4$Class),positive='0')

bag_ROC <- ROC(form=Class~apred.bagp6$class, data=atestp4, plot="ROC")


logistic6<-glm(Class~.,data=atrainp4,family="binomial")
summary(logistic6)

predl4<-predict(logistic6,atestp4[,-19],type="response")
predl4<-as.data.frame(predl4)
predl4$grade<-ifelse(predl4$pred<0.5,predl4$grade<-0,predl4$grade<-1)
confusionMatrix(data=as.factor(predl4$grade),reference=atestp4[,19],positive='0')

bag_ROC <- ROC(form=Class~predl4$grade, data=atestp4, plot="ROC")

table(HNP_All4_scale$폐경여부)


#표 작업(잡코드)-chi square

#Overestimate
correct = c(2603,	4534,	2155,	2300)
incorrect= c(569,	1266,	379,	159)

data1 <- data.frame(correct)
data2 <- data.frame(incorrect)

chisq.test(data1)
chisq.test(data2)


#Underestimate
correct2 = c(479,	2903,	2702,	2612)
incorrect2= c(31,	303,	673,	2134)
data3 <- data.frame(correct2)
data4 <- data.frame(incorrect2)
chisq.test(data3)
chisq.test(data4)

#BMIOverestimate

BMI1 = c(18.1,20.3,	21.1,	21.1)
data5 <- data.frame(BMI1)
chisq.test(data5)

#BMIUnderestimate

BMI2 = c(25.6,	26.3,	26,	26.2)
data6 <- data.frame(BMI2)
chisq.test(data6)

#
sum(correct3)
sum(under)
sum(over)

Correct3<-correct3/sum(correct3)
Under<-under/sum(under)
Over<-over/sum(over)



correct3 = c(3369,
             2314,
             2964,
             2915,
             2730,
             2811,
             3184)
under= c(430,
         499,
         492,
         447,
         419,
         426,
         428
)
over= c(431,
        320,
        343,
        313,
        356,
        265,
        347
)
data5 <- data.frame(Correct3)
data6 <- data.frame(Under)
data7 <- data.frame(Over)
chisq.test(data5)
chisq.test(data6)
chisq.test(data7)

trend.test(data5)


#그냥 대통합
HNP_sep_scale1<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/New1.csv")
HNP_sep_scale2<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/New2.csv")
HNP_sep_scale3<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/New3.csv")
HNP_sep_scale4<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/New4.csv")
HNP_sep_scale5<-read.csv("C:/Users/User/Desktop/JooLee_KoreanHN_data/New5.csv")


HNP_sep_scale$Pain.or.discomfort.status<-HNP_sep_scale$Pain.or.discomfort.status
HNP_sep_scale$Anxiety.or.depression.status<-HNP_sep_scale$Anxiety.or.depression.status

HNP_sep_scale[,-c(6:13)]<-scale(HNP_sep_scale[,-c(6:13)])

HNP_sep_scale1$Exercise.for.weight.reduction<-factor(HNP_sep_scale1$Exercise.for.weight.reduction)
HNP_sep_scale1$Chronic.pain<-factor(HNP_sep_scale1$Chronic.pain)
HNP_sep_scale1$Anxiety.and.depressive.mood<-factor(HNP_sep_scale1$Anxiety.and.depressive.mood)
HNP_sep_scale1$Weight.effort<-factor(HNP_sep_scale1$Weight.effort)
HNP_sep_scale1$Marital.status<-factor(HNP_sep_scale1$Marital.status)

HNP_sep_scale2$Exercise.for.weight.reduction<-factor(HNP_sep_scale2$Exercise.for.weight.reduction)
HNP_sep_scale2$Chronic.pain<-factor(HNP_sep_scale2$Chronic.pain)
HNP_sep_scale2$Anxiety.and.depressive.mood<-factor(HNP_sep_scale2$Anxiety.and.depressive.mood)
HNP_sep_scale2$Weight.effort<-factor(HNP_sep_scale2$Weight.effort)
HNP_sep_scale2$Marital.status<-factor(HNP_sep_scale2$Marital.status)

HNP_sep_scale3$Exercise.for.weight.reduction<-factor(HNP_sep_scale3$Exercise.for.weight.reduction)
HNP_sep_scale3$Chronic.pain<-factor(HNP_sep_scale3$Chronic.pain)
HNP_sep_scale3$Anxiety.and.depressive.mood<-factor(HNP_sep_scale3$Anxiety.and.depressive.mood)
HNP_sep_scale3$Weight.effort<-factor(HNP_sep_scale3$Weight.effort)
HNP_sep_scale3$Marital.status<-factor(HNP_sep_scale3$Marital.status)
HNP_sep_scale3$Menopause.status<-factor(HNP_sep_scale3$Menopause.status)


HNP_sep_scale4$Exercise.for.weight.reduction<-factor(HNP_sep_scale4$Exercise.for.weight.reduction)
HNP_sep_scale4$Chronic.pain<-factor(HNP_sep_scale4$Chronic.pain)
HNP_sep_scale4$Anxiety.and.depressive.mood<-factor(HNP_sep_scale4$Anxiety.and.depressive.mood)
HNP_sep_scale4$Weight.effort<-factor(HNP_sep_scale4$Weight.effort)
HNP_sep_scale4$Marital.status<-factor(HNP_sep_scale4$Marital.status)
HNP_sep_scale4$Menopause.type<-factor(HNP_sep_scale4$Menopause.type)

HNP_sep_scale5$Exercise.for.weight.reduction<-factor(HNP_sep_scale5$Exercise.for.weight.reduction)
HNP_sep_scale5$Chronic.pain<-factor(HNP_sep_scale5$Chronic.pain)
HNP_sep_scale5$Anxiety.and.depressive.mood<-factor(HNP_sep_scale5$Anxiety.and.depressive.mood)
HNP_sep_scale5$Weight.effort<-factor(HNP_sep_scale5$Weight.effort)
HNP_sep_scale5$Marital.status<-factor(HNP_sep_scale5$Marital.status)

HNP_sep_scale1<- within((HNP_sep_scale1), Menopause.type <- relevel(Menopause.type, ref = 1))
HNP_sep_scale1<- within((HNP_sep_scale1), Menopause.type <- relevel(Menopause.type, ref = 1))
HNP_sep_scale1<- within((HNP_sep_scale1), Menopause.type <- relevel(Menopause.type, ref = 1))
HNP_sep_scale1<- within((HNP_sep_scale1), Menopause.type <- relevel(Menopause.type, ref = 1))
HNP_sep_scale1<- within((HNP_sep_scale1), Menopause.type <- relevel(Menopause.type, ref = 1))


HNP_sep_scale1$癤풺ody.image

#glm
attach(HNP_sep_scale1)
dstrat_sep<-svydesign(id=~1,strata=~aa, weights=~bb, data=HNP_sep_scale1)
dstrat_sep_result_1<-svyglm(癤풺ody.image~Age+Alcohol.consumption+Anxiety.and.depressive.mood+Chronic.pain+Days.of.anaerobic.workout+Days.of.walking.per.week+Education.level+Exercise.for.weight.reduction+Gravidity+Income+Marital.status+Self.perceived.health.status+Weight.effort
,design=dstrat_sep)
summary(svyglm(癤풺ody.image~Age+Alcohol.consumption+Anxiety.and.depressive.mood+Chronic.pain+Days.of.anaerobic.workout+Days.of.walking.per.week+Education.level+Exercise.for.weight.reduction+Gravidity+Income+Marital.status+Self.perceived.health.status+Weight.effort
,design=dstrat_sep))
detach(HNP_sep_scale1)

baginter1<-bagging(Class~.,data=HNP_sep_scale1_k,mfinal=15)
names(baginter1)
baginter1$importance

baginter2<-bagging(Class~.,data=HNP_sep_scale2_k,mfinal=15)
names(baginter2)
baginter2$importance

baginter3<-bagging(Class~.,data=HNP_sep_scale3_k,mfinal=15)
names(baginter3)
baginter3$importance

baginter4<-bagging(Class~.,data=HNP_sep_scale4_k,mfinal=15)
names(baginter4)
baginter4$importance




names(HNP_sep_scale2)

table(HNP_sep_scale2$aa)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

attach(HNP_sep_scale2)
dstrat_sep<-svydesign(id=~1,strata=~aa, weights=~bb, data=HNP_sep_scale2)
dstrat_sep_result_2<-svyglm(癤풟ody.image~Alcohol.consumption          
                              + Anxiety.and.depressive.mood   + Chronic.pain                 
                              + Days.of.anaerobic.workout     + Days.of.walking..per.week    
                              + Depression                    + Education.level              
                              + Exercise.for.weight.reduction + Gravidity                    
                              + Income                        + Marital.status               
                              + Menopause.status              + Self.perceived.health.status 
                              + Weight.effort                 
                              ,design=dstrat_sep)
summary(svyglm(癤풟ody.image~Alcohol.consumption          
                 + Anxiety.and.depressive.mood   + Chronic.pain                 
                 + Days.of.anaerobic.workout     + Days.of.walking..per.week    
                 + Depression                    + Education.level              
                 + Exercise.for.weight.reduction + Gravidity                    
                 + Income                        + Marital.status               
                 + Menopause.status              + Self.perceived.health.status 
                 + Weight.effort                 
                 ,design=dstrat_sep))
detach(HNP_sep_scale2)

names(HNP_sep_scale3)

attach(HNP_sep_scale3)
dstrat_sep<-svydesign(id=~1,strata=~aa, weights=~bb, data=HNP_sep_scale3)
dstrat_sep_result_3<-svyglm(癤풟ody.image~Alcohol.consumption          
                              + Anxiety.and.depressive.mood   + Chronic.pain                 
                              + Days.of.anaerobic.workout     + Days.of.walking.per.week    
                              + Depression                   + Diabetes + Education.level              
                              + Exercise.for.weight.reduction + Gravidity     + Hyperlipidemia	
                              +Hypertension
                              + Income                        + Marital.status               
                              + Menopause.status              + Self.perceived.health.status 
                              + Weight.effort                 
                              ,design=dstrat_sep)
summary(svyglm(癤풟ody.image~Alcohol.consumption          
                 + Anxiety.and.depressive.mood   + Chronic.pain                 
                 + Days.of.anaerobic.workout     + Days.of.walking.per.week    
                 + Depression                    + Education.level              
                 + Exercise.for.weight.reduction + Gravidity     + Hyperlipidemia	
                 +Hypertension
                 + Income                        + Marital.status               
                 + Menopause.status              + Self.perceived.health.status 
                 + Weight.effort                 
                 ,design=dstrat_sep))
detach(HNP_sep_scale3)

names(HNP_sep_scale4)

attach(HNP_sep_scale4)
dstrat_sep<-svydesign(id=~1,strata=~aa, weights=~bb, data=HNP_sep_scale4)
dstrat_sep_result_4<-svyglm(癤풟ody.image~Alcohol.consumption          
                              + Anxiety.and.depressive.mood   + Chronic.pain                 
                              + Days.of.anaerobic.workout     + Days.of.walking..per.week    
                              + Depression                    + Diabetes + Education.level              
                              + Exercise.for.weight.reduction + Gravidity     + Hyperlipidemia	
                              + Hypertension
                              + Income                        + Marital.status               
                              + Menopause.type              + Self.perceived.health.status 
                              + Weight.effort                 
                              ,design=dstrat_sep)
summary(svyglm(癤풟ody.image~Alcohol.consumption          
                 + Anxiety.and.depressive.mood   + Chronic.pain                 
                 + Days.of.anaerobic.workout     + Days.of.walking..per.week    
                 + Depression                    + Diabetes + Education.level              
                 + Exercise.for.weight.reduction + Gravidity     + Hyperlipidemia	
                 + Hypertension
                 + Income                        + Marital.status               
                 + Menopause.type              + Self.perceived.health.status 
                 + Weight.effort                 
                 ,design=dstrat_sep))
detach(HNP_sep_scale4)

attach(HNP_sep_scale5)
dstrat_sep<-svydesign(id=~1,strata=~gagoo, weights=~boonsan, data=HNP_sep_scale5)
dstrat_sep_result_5<-svyglm(癤풺ody.image~Age+Alcohol.consumption+Anxiety.and.depressive.mood+Chronic.pain+Days.of.anaerobic.workout+Days.of.walking.per.week+Education.level+Exercise.for.weight.reduction+Gravidity+Income+Marital.status+Self.perceived.health.status+Weight.effort
                              ,design=dstrat_sep)
summary(svyglm(癤풺ody.image~Age+Alcohol.consumption+Anxiety.and.depressive.mood+Chronic.pain+Days.of.anaerobic.workout+Days.of.walking.per.week+Education.level+Exercise.for.weight.reduction+Gravidity+Income+Marital.status+Self.perceived.health.status+Weight.effort
                 ,design=dstrat_sep))
detach(HNP_sep_scale1)



library(caret)
library(rpart)

#kfold1
HNP_sep_scale1

HNP_sep_scale1_k <- HNP_sep_scale1
#colnames(x) <- c('bmi', 'age', 'Class') -> baseline model에서 사용하는 코드
names(HNP_sep_scale1_k)[1] <- c('Class')
HNP_sep_scale1_k$aa<-NULL
HNP_sep_scale1_k$bb<-NULL

HNP_sep_scale1_k$Exercise.for.weight.reduction<-factor(HNP_sep_scale1_k$Exercise.for.weight.reduction)
HNP_sep_scale1_k$Chronic.pain<-factor(HNP_sep_scale1_k$Chronic.pain)
HNP_sep_scale1_k$Anxiety.and.depressive.mood<-factor(HNP_sep_scale1_k$Anxiety.and.depressive.mood)
HNP_sep_scale1_k$Weight.effort<-factor(HNP_sep_scale1_k$Weight.effort)
HNP_sep_scale1_k$Marital.status<-factor(HNP_sep_scale1_k$Marital.status)
HNP_sep_scale1_k$Class <- as.factor(HNP_sep_scale1_k$Class)
set.seed(42)
HNP_sep_scale1_k <- downSample(subset(HNP_sep_scale1_k, select=-Class), HNP_sep_scale1_k$Class)

library(adabag)
V = 5 #V-fold CV
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(HNP_sep_scale2_k), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning
  
  train = HNP_sep_scale2_k[id != i,] 
  test = HNP_sep_scale2_k[id == i,] 
  
  ## Boosting
  
  
  fit = bagging(Class~.,data=train,mfinal=15)
  
  ## Predicting and Evaluating
  
  pred = predict.bagging(fit, newdata=test)
  yhat = pred$class
  miss.err.test = miss.err.test + mean(test$Class != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test



accuracy <- 1- cv.err.test
accuracy

ctable = table(test$Class, yhat, dnn=c("Actual", "Predicted")); ctable


```{r}
df <- data.frame(test$Class, yhat)
df$test.Class <- as.numeric(df$test.Class)
df$yhat <- as.numeric(df$yhat)

roc(df$test.Class, df$yhat)
```
install.package("C50")
library(pROC)


#kfold2
HNP_sep_scale2

HNP_sep_scale2_k <- HNP_sep_scale2
#colnames(x) <- c('bmi', 'age', 'Class') -> baseline model에서 사용하는 코드
names(HNP_sep_scale2_k)[1] <- c('Class')
HNP_sep_scale2_k$aa<-NULL
HNP_sep_scale2_k$bb<-NULL

HNP_sep_scale2_k$Exercise.for.weight.reduction<-factor(HNP_sep_scale2_k$Exercise.for.weight.reduction)
HNP_sep_scale2_k$Chronic.pain<-factor(HNP_sep_scale2_k$Chronic.pain)
HNP_sep_scale2_k$Anxiety.and.depressive.mood<-factor(HNP_sep_scale2_k$Anxiety.and.depressive.mood)
HNP_sep_scale2_k$Weight.effort<-factor(HNP_sep_scale2_k$Weight.effort)
HNP_sep_scale2_k$Marital.status<-factor(HNP_sep_scale2_k$Marital.status)
HNP_sep_scale2_k$Class <- as.factor(HNP_sep_scale2_k$Class)
set.seed(42)
HNP_sep_scale2_k <- downSample(subset(HNP_sep_scale2_k, select=-Class), HNP_sep_scale2_k$Class)

library(adabag)
V = 5 #V-fold CV
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(HNP_sep_scale2_k), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning
  
  train = HNP_sep_scale2_k[id != i,] 
  test = HNP_sep_scale2_k[id == i,] 
  
  ## Boosting
  
  
  fit = bagging(Class~.,data=train,mfinal=15)
  
  ## Predicting and Evaluating
  
  pred = predict.bagging(fit, newdata=test)
  yhat = pred$class
  miss.err.test = miss.err.test + mean(test$Class != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test



accuracy <- 1- cv.err.test
accuracy

ctable = table(test$Class, yhat, dnn=c("Actual", "Predicted")); ctable


```{r}
df <- data.frame(test$Class, yhat)
df$test.Class <- as.numeric(df$test.Class)
df$yhat <- as.numeric(df$yhat)

roc(df$test.Class, df$yhat)
```
install.package("C50")
library(pROC)


names(HNP_sep_scale4)[1] <- c('Class')


#kfold3
HNP_sep_scale3

HNP_sep_scale3_k <- HNP_sep_scale3
#colnames(x) <- c('bmi', 'age', 'Class') -> baseline model에서 사용하는 코드
names(HNP_sep_scale3_k)[1] <- c('Class')
HNP_sep_scale3_k$aa<-NULL
HNP_sep_scale3_k$bb<-NULL



HNP_sep_scale3_k$Exercise.for.weight.reduction<-factor(HNP_sep_scale3_k$Exercise.for.weight.reduction)
HNP_sep_scale3_k$Chronic.pain<-factor(HNP_sep_scale3_k$Chronic.pain)
HNP_sep_scale3_k$Anxiety.and.depressive.mood<-factor(HNP_sep_scale3_k$Anxiety.and.depressive.mood)
HNP_sep_scale3_k$Weight.effort<-factor(HNP_sep_scale3_k$Weight.effort)
HNP_sep_scale3_k$Marital.status<-factor(HNP_sep_scale3_k$Marital.status)
HNP_sep_scale3_k$Menopause.status<-factor(HNP_sep_scale3_k$Menopause.status)

HNP_sep_scale3_k$Class <- as.factor(HNP_sep_scale3_k$Class)
set.seed(42)
HNP_sep_scale3_k <- downSample(subset(HNP_sep_scale3_k, select=-Class), HNP_sep_scale3_k$Class)

library(adabag)
V = 5 #V-fold CV
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(HNP_sep_scale3_k), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning
  
  train = HNP_sep_scale3_k[id != i,] 
  test = HNP_sep_scale3_k[id == i,] 
  
  ## Boosting
  
  
  fit = bagging(Class~.,data=train,mfinal=15)
  
  ## Predicting and Evaluating
  
  pred = predict.bagging(fit, newdata=test)
  yhat = pred$class
  miss.err.test = miss.err.test + mean(test$Class != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test



accuracy <- 1- cv.err.test
accuracy

ctable = table(test$Class, yhat, dnn=c("Actual", "Predicted")); ctable


```{r}
df <- data.frame(test$Class, yhat)
df$test.Class <- as.numeric(df$test.Class)
df$yhat <- as.numeric(df$yhat)

roc(df$test.Class, df$yhat)
```
install.package("C50")
library(pROC)

#kfold4
HNP_sep_scale4

HNP_sep_scale4_k <- HNP_sep_scale4
#colnames(x) <- c('bmi', 'age', 'Class') -> baseline model에서 사용하는 코드
names(HNP_sep_scale4_k)[1] <- c('Class')
HNP_sep_scale4_k$aa<-NULL
HNP_sep_scale4_k$bb<-NULL


HNP_sep_scale4_k$Class <- as.factor(HNP_sep_scale4_k$Class)
set.seed(42)
HNP_sep_scale4_k <- downSample(subset(HNP_sep_scale4_k, select=-Class), HNP_sep_scale4_k$Class)

library(adabag)
V = 5 #V-fold CV
miss.err.test = 0

set.seed(1234)
id = sample(1:V, nrow(HNP_sep_scale4_k), replace = T)

for(i in 1:V) {
  
  print(i)
  
  ## Data Partitioning
  
  train = HNP_sep_scale4_k[id != i,] 
  test = HNP_sep_scale4_k[id == i,] 
  
  ## Boosting
  
  
  fit = bagging(Class~.,data=train,mfinal=15)
  
  ## Predicting and Evaluating
  
  pred = predict.bagging(fit, newdata=test)
  yhat = pred$class
  miss.err.test = miss.err.test + mean(test$Class != yhat) 
  
}

cv.err.test = miss.err.test/ V;cv.err.test



accuracy <- 1- cv.err.test
accuracy

ctable = table(test$Class, yhat, dnn=c("Actual", "Predicted")); ctable


```{r}
df <- data.frame(test$Class, yhat)
df$test.Class <- as.numeric(df$test.Class)
df$yhat <- as.numeric(df$yhat)

roc(df$test.Class, df$yhat)





HNP_sep_scale4_k$Exercise.for.weight.reduction<-factor(HNP_sep_scale4_k$Exercise.for.weight.reduction)
HNP_sep_scale4_k$Chronic.pain<-factor(HNP_sep_scale4_k$Chronic.pain)
HNP_sep_scale4_k$Anxiety.and.depressive.mood<-factor(HNP_sep_scale4_k$Anxiety.and.depressive.mood)
HNP_sep_scale4_k$Weight.effort<-factor(HNP_sep_scale4_k$Weight.effort)
HNP_sep_scale4_k$Marital.status<-factor(HNP_sep_scale4_k$Marital.status)
HNP_sep_scale4_k$Menopause.type<-factor(HNP_sep_scale4_k$Menopause.type)
