library(R.matlab)
library(tidyverse)
library(ggplot2)

#409 
zm_bb409 <- readMat("Data/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb409) <- c("closed_arm","open_arm")
zm_bb409$open_arm  <- ifelse(zm_bb409$open_arm=="0", 0,2)
zm_bb409$type <- zm_bb409$closed_arm+zm_bb409$open_arm
zm_bb409$type <- ifelse(zm_bb409$type == "0", "none",
                        ifelse(zm_bb409$type =="1","closed_arm", "open_arm"))
zm_zs409 <- readMat("Data/Zero_Maze/608034_409/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs409 <- cbind(time = rownames(zm_zs409), zm_zs409)
rownames(zm_zs409)<- NULL

zm_409 <- cbind.data.frame(zm_bb409,zm_zs409)
rownames(zm_409)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_1"="darkgray")
p409 <- ggplot(as.data.frame(zm_409))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 600))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right", legend.text = element_text(size=5)) +
  ggtitle("Elevated Zero Maze(608034_409)")+
  scale_color_manual(values = color)

## 412
zm_bb412 <- readMat("Data/Zero_Maze/608102_412/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb412) <- c("closed_arm","open_arm")
zm_bb412$open_arm  <- ifelse(zm_bb412$open_arm=="0", 0,2)
zm_bb412$type <- zm_bb412$closed_arm+zm_bb412$open_arm
zm_bb412$type <- ifelse(zm_bb412$type == "0", "none",
                        ifelse(zm_bb412$type =="1","closed_arm", "open_arm"))
zm_zs412 <- readMat("Data/Zero_Maze/608102_412/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs412 <- cbind(time = rownames(zm_zs412), zm_zs412)
rownames(zm_zs412)<- NULL

zm_412 <- cbind.data.frame(zm_bb412,zm_zs412)
rownames(zm_412)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_1"="darkgray")
p412 <- ggplot(as.data.frame(zm_412))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 600))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right", legend.text = element_text(size=5)) +
  ggtitle("Elevated Zero Maze(608102_412)")+
  scale_color_manual(values = color)


## 414 
zm_bb414 <- readMat("Data/Zero_Maze/608102_414/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb414) <- c("closed_arm","open_arm")
zm_bb414$open_arm  <- ifelse(zm_bb414$open_arm=="0", 0,2)
zm_bb414$type <- zm_bb414$closed_arm+zm_bb414$open_arm
zm_bb414$type <- ifelse(zm_bb414$type == "0", "none",
                        ifelse(zm_bb414$type =="1","closed_arm", "open_arm"))
zm_zs414 <- readMat("Data/Zero_Maze/608102_414/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs414 <- cbind(time = rownames(zm_zs414), zm_zs414)
rownames(zm_zs414)<- NULL

zm_414 <- cbind.data.frame(zm_bb414,zm_zs414)
rownames(zm_414)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_1"="darkgray")
p414 <- ggplot(as.data.frame(zm_414))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 600))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right", legend.text = element_text(size=5)) +
  ggtitle("Elevated Zero Maze(608102_414)")+
  scale_color_manual(values = color)

#416

zm_bb416 <- readMat("Data/Zero_Maze/608103_416/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb416) <- c("closed_arm","open_arm")
zm_bb416$open_arm  <- ifelse(zm_bb416$open_arm=="0", 0,2)
zm_bb416$type <- zm_bb416$closed_arm+zm_bb416$open_arm
zm_bb416$type <- ifelse(zm_bb416$type == "0", "none",
                        ifelse(zm_bb416$type =="1","closed_arm", "open_arm"))
zm_zs416 <- readMat("Data/Zero_Maze/608103_416/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs416 <- cbind(time = rownames(zm_zs416), zm_zs416)
rownames(zm_zs416)<- NULL

zm_416 <- cbind.data.frame(zm_bb416,zm_zs416)
rownames(zm_416)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_1"="darkgray")
p416 <- ggplot(as.data.frame(zm_416))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 600))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right", legend.text = element_text(size=5)) +
  ggtitle("Elevated Zero Maze(608103_416)")+
  scale_color_manual(values = color)


# 417
zm_bb417 <- readMat("Data/Zero_Maze/608103_417/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb417) <- c("closed_arm","open_arm")
zm_bb417$open_arm  <- ifelse(zm_bb417$open_arm=="0", 0,2)
zm_bb417$type <- zm_bb417$closed_arm+zm_bb417$open_arm
zm_bb417$type <- ifelse(zm_bb417$type == "0", "none",
                        ifelse(zm_bb417$type =="1","closed_arm", "open_arm"))
zm_zs417 <- readMat("Data/Zero_Maze/608103_417/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs417 <- cbind(time = rownames(zm_zs417), zm_zs417)
rownames(zm_zs417)<- NULL

zm_417 <- cbind.data.frame(zm_bb417,zm_zs417)
rownames(zm_417)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_1"="darkgray")
p417 <- ggplot(as.data.frame(zm_417))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 600))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right", legend.text = element_text(size=5)) +
  ggtitle("Elevated Zero Maze(608103_417)")+
  scale_color_manual(values = color)


#418

zm_bb418 <- readMat("Data/Zero_Maze/608103_418/Day_1/Trial_001_0/binned_behavior.mat") %>% as.data.frame() %>% t() %>% as.data.frame()
colnames(zm_bb418) <- c("closed_arm","open_arm")
zm_bb418$open_arm  <- ifelse(zm_bb418$open_arm=="0", 0,2)
zm_bb418$type <- zm_bb418$closed_arm+zm_bb418$open_arm
zm_bb418$type <- ifelse(zm_bb418$type == "0", "none",
                        ifelse(zm_bb418$type =="1","closed_arm", "open_arm"))
zm_zs418 <- readMat("Data/Zero_Maze/608103_418/Day_1/Trial_001_0/binned_zscore.mat")%>% as.data.frame()

zm_zs418 <- cbind(time = rownames(zm_zs418), zm_zs418)
rownames(zm_zs418)<- NULL

zm_418 <- cbind.data.frame(zm_bb418,zm_zs418)
rownames(zm_418)<- NULL
color <- c("closed_arm"="tomato", "open_arm"="skyblue", "none"="plum", "neuron_"="darkgray")
p418 <- ggplot(as.data.frame(zm_418))+
  geom_line(aes(x=as.numeric(time), y=binned.zscore.1, col="neuron_1"))+
  geom_point(aes(x=as.numeric(time), y= binned.zscore.1,colour = type), alpha=I(0.3))+ 
  xlab("time")+ ylab("zscore/activity")+
  scale_x_continuous(limits = c(0, 4300), breaks = seq(0, 4300, 600))+
  theme(legend.title=element_blank(),axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right", legend.text = element_text(size=5)) +
  ggtitle("Elevated Zero Maze(608103_418)")+
  scale_color_manual(values = color)

p418
