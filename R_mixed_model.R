#import data file
f1<-read.table(file.choose(), header=TRUE, sep=",") #file1
f2<-read.table(file.choose(), header=TRUE, sep=",") #file2

library(lme4)
library(nlme)

#model 1
mod1<-lm(FEV1~trt+pack_years+current_smoker+emphysema+race+height+bmi,data=f2)
summary(mod1)
#model 2
mod2<-lm(delta_FEV1~pre_FEV1+pack_years+current_smoker+emphysema+race+height+bmi,data=f1)
summary(mod2)

#model 3 random intercept and slope
mod3<-lme(FEV1~trt+pack_years+current_smoker+emphysema+race+height+bmi,
          random=~1+trt|ID,
          data=f2)
mod3a<-lmer(FEV1~trt+pack_years+current_smoker+emphysema+race+height+bmi+
              (1+trt|ID),
          data=f2)
summary(mod3)
summary(mod3a)
#model 4
mod4<-lme(FEV1~trt+pack_years+current_smoker+emphysema+race+height+bmi,
          random=~1|ID,
          data=f2)
mod4a<-lmer(FEV1~trt+pack_years+current_smoker+emphysema+race+height+bmi+
              (1|ID),
            data=f2)
summary(mod4)
summary(mod4a)