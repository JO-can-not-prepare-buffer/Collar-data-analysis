# 试用一下glmer
# 分析一下造林地利用与日活动规律的关系，性别季节差异个体差异为随机因子
library(lmerTest)
library(tidyverse)
library(emmeans)

cmc <- read.csv(file.choose())
cmc.df <- select(cmc, c(4:16))
cmc.df[c('Sex','Season','id')] = lapply(cmc.df[c('Sex','Season','id')], factor)


Modelmax = lmer(data = cmc.df,
                formula = log(HR) ~ LC_pl +
                  (1+LC_pl|id) +
                  (1+LC_pl|Season) +
                  (1+LC_pl|Sex),
                control = lmerControl(optimizer = 'bobyqa'))
# 全模型出现畸形协方差矩阵
# “boundary(singular)"

Modelzero = lmer(data = cmc.df,
                 formula = log(HR) ~ LC_pl +
                   (1|id) +
                   (1|Season) +
                   (1|Sex),
                 control = lmerControl(optimizer = 'bobyqa'))
# 零模型依旧出现畸形协方差矩阵

summary(Modelmax)
# 可以先summary一下看看
# 看一下Std.Dev标准差，有的非常小，说明只解释了很少一部分
# 再看一下Corr不同随机效应之间的相关系数，接近1的，说明相关性很强
# 说明有多余的变量，所以就会报错

# 这里显示，造林地占比与季节、性别都有关系
# 所以先不看这两个随机因子

Model = lmer(data = cmc.df,
                formula = log(HR) ~ LC_pl +
                  (1+LC_pl|id),
                control = lmerControl(optimizer = 'bobyqa'))
summary(Model)
