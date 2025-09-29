setwd('~/desktop')
library(readxl)
# the figure1.xls need to have score and HR information
data<-read_excel("figure1.xls")
head(data)
#install.packages('ggpubr')
#install.packages('rstatix')
#install.packages('car_3.1-0.tgz')

library(dplyr)
library(plotrix)
res<-data %>% 
  group_by(score_group) %>% 
  summarise(mean=mean(heart_12mean,na.rm=T),se=std.error(heart_12mean))

res
library(ggplot2)
library(ggpubr)

ggplot(res, aes(x=score_group, y=mean,fill=factor(score_group))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_cartesian(ylim=c(100,150))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position=position_dodge(.9))+
  labs(y='Heart Rate',title='')+theme_classic()+
   scale_fill_manual("",values = c("1" = "gray33", "2" = "gray33","3" = "gray33"),labels=c("8", "9",'10'))+
     scale_x_discrete(name ="Score", 
                      limits=c("<=8","9","10"))+
  theme(legend.position = "none",axis.title = element_text(size = 18),plot.title = element_text(hjust = 0.5,size =24),axis.text = element_text(size = 14))+
   geom_signif(annotation = c('**','*','***'),size=0.5,textsize=8,vjust=0.7,
   y_position = c(145,145,150), xmin = c(1,2.1,1), xmax = c(1.9,3,3),tip_length = c(0.2, 0.2)
  )
ggsave('Figure 1.pdf')

