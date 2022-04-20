library(R.matlab)
library(dplyr)
library(ggplot2)


#direct interaction

di_bb409 <- readMat("Dir_Interact/608034_409/Day_1/Trial_002_0/binned_behavior.mat")  %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(di_bb409) <- c("interacting", "non_social")
di_bb409$non_social  <- ifelse(di_bb409$non_social=="0", 0,2)
di_bb409$type <- di_bb409$interacting+di_bb409$non_social
di_bb409$type <- ifelse(di_bb409$type == "0", "None",
                        ifelse(di_bb409$type =="1","interacting", "non_social"))

# di_bb409 <- cbind(time = rownames(di_bb409), di_bb409)
# rownames(di_bb409)<- NULL

# #ggplot(as.data.frame(di_bb409))+
#   geom_line(aes(x=time, y=interacting))


di_zs409 <- readMat("Dir_Interact/608034_409/Day_1/Trial_002_0/binned_zscore.mat") %>% as.data.frame()
di_zs409 <- cbind(time = rownames(di_zs409), di_zs409)
rownames(di_zs409)<- NULL
# ggplot(as.data.frame(di_zs409))+
#   geom_line(aes(x=as.numeric(time), y=binned.zscore.1))+
#   scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 300))
# #summary(di_zs409)
#plot.ts(di_zs409[44])

di_409 <- cbind.data.frame(di_bb409,di_zs409)
rownames(di_409)<- NULL

color <- c("interacting"="tomato", "non_social"="skyblue", "None" = "plum", "neuron_1"="darkgray")
ggplot(as.data.frame(di_409))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  ylab("zscore/activity")+
  xlab("time")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 300))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right") +
  ggtitle("Direct Interaction of Mice(608034_409)")+
  scale_color_manual(values = color)


#opposite sex

os_bb409 <- readMat("Opp_Sex/608034_409/Day_1/Trial_002_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(os_bb409) <- c("male", "female")
os_bb409$female  <- ifelse(os_bb409$female=="0", 0,2)
os_bb409$type <- os_bb409$male+os_bb409$female
os_bb409$type <- ifelse(os_bb409$type == "0", "none",
                        ifelse(os_bb409$type =="1","male", "female"))

os_zs409 <- readMat("Opp_Sex/608034_409/Day_1/Trial_002_0/binned_zscore.mat")%>% as.data.frame()
os_zs409 <- cbind(time = rownames(os_zs409), os_zs409)
rownames(os_zs409)<- NULL

os_409 <- cbind.data.frame(os_bb409,os_zs409)
rownames(os_409)<- NULL
color <- c("male"="tomato", "female"="skyblue", "none"="plum", "neuron_1"="darkgray")
ggplot(as.data.frame(os_409))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 300))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right") +
  ggtitle("Opposite Sex of Mice(608034_409)")+
  scale_color_manual(values = color)





#zero maze

zm_bb409 <- readMat("Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb409) <- c("closed_arm","open_arm")
zm_bb409$open_arm  <- ifelse(zm_bb409$open_arm=="0", 0,2)
zm_bb409$type <- zm_bb409$closed_arm+zm_bb409$open_arm
zm_bb409$type <- ifelse(zm_bb409$type == "0", "none",
                        ifelse(zm_bb409$type =="1","closed_arm", "open_arm"))
zm_zs409 <- readMat("Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs409 <- cbind(time = rownames(zm_zs409), zm_zs409)
rownames(zm_zs409)<- NULL

zm_409 <- cbind.data.frame(zm_bb409,zm_zs409)
rownames(zm_409)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_1"="darkgray")
ggplot(as.data.frame(zm_409))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 300))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right") +
  ggtitle("Elevated Zero Maze(608034_409)")+
  scale_color_manual(values = color)