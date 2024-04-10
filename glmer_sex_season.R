# 试用一下glmer
# 分析一下性别季节差异，个体差异为随机因子
library(lmerTest)
library(tidyverse)
library(emmeans)

cmc <- read.csv(file.choose())
cmc.df <- select(cmc, c(4:16))
cmc.df[c('Sex','Season')] = lapply(cmc.df[c('Sex','Season')], factor)

contrasts(cmc.df$Sex)=c(-0.5,0.5) #定义对比方式
contrasts(cmc.df$Season)=c(-0.5,0.5) #定义对比方式
M=model.matrix(~Sex*Season,cmc.df) #手动生成虚拟变量

cmc.df[c('F_M','G_NG','interaction')]=M[,c(2:4)]
View(cmc.df)

# 全模型
Modelmax <- glmer(data = cmc.df,
                  formula = (DA/10000) ~ Sex*Season+
                    (1+F_M+G_NG+interaction|id),
                  family = "Gamma")
# 零模型
Modelzero <- glmer(data = cmc.df,
                  formula = (DA/10000) ~ Sex*Season+
                    (1+F_M+G_NG+interaction||id),
                  family = "Gamma")

summary(rePCA(Modelzero))
VarCorr(Modelzero)
# 没有接近0的，所以，没有多余的变量，按照全模型计算

Model=glmer(data = cmc.df,
            formula = (DA/10000) ~ Sex*Season + (1+F_M+G_NG+interaction|id),
            family = "Gamma")

summary(Model)
car::Anova(Model,type=3)

summary(Model)$coef %>% round(3)


emmeans(Model,pairwise~Sex|Season)
cmc.df %>% group_by(Sex, Season)%>%summarise(Reg=mean(DA))
