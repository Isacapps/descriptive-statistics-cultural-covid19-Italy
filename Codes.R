rm(list=ls())
setwd("E:\\DATA ANALYSIS AND BIG DATA LAB\\ESSAY")
library(readxl)
AVQ_2019 <- read_excel("AVQ_2019.xlsx")
View(AVQ_2019)

#we don't have sufficient observation to consider this variable
#So, remove column numbibm
AVQ_2019$NUMBIB <- NULL

#new database
View(AVQ_2019)

#Create a database without NA
library(tidyverse)
AVQ_2019_wona <- filter(AVQ_2019, !is.na(TEATRO),!is.na(CINE), !is.na(MUSEO), !is.na(MUSIC),
                             !is.na(ACMUS), !is.na(MONUM), !is.na(ETAMI),
                             !is.na(RIPMF), !is.na(SESSO), !is.na(ISTRMI))
View(AVQ_2019_wona)

#Create a new database excluding children whose age is lower than 6 years old.
#population of people that have more than 6 years old.
sixplus_2019 <-  filter(AVQ_2019_wona, ETAMI >=3)
View(sixplus_2019)

#factor variables
attach(sixplus_2019)
ETAMI_2019.f <- factor(ETAMI,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                  labels=c("0-2","3-5","6-10","11-13","14-15","16-17","18-19","20-24",
                           "25-34","35-44","45-54","55-59","60-64","65-74","75più"))

SESSO_2019.f <-factor(SESSO,levels=c(1,2),
                 labels=c("male","female"))

ISTRMI_2019.f <- factor(ISTRMI,levels=c(1,7,9,10,99),
                   labels=c("bachelor’s Degree and Master’s Degree","high school leaving certificate",
                            "middle school license","elementary school license, no educational qualifications",
                            "unavailable"))

RIPMF_2019.f <- factor(RIPMF, levels=c(1,2,3,4,5,9),
                  labels=c("northwest","northeast","centre","south","islands","unavailable"))

TEATRO_2019.f <- factor(TEATRO, levels=c(1,2,3,4,5),
                   labels=c("never","1-3","4-6","7-12","12più"))

CINE_2019.f <- factor(CINE, levels=c(1,2,3,4,5),
                 labels=c("never","1-3","4-6","7-12","12più"))

MUSEO_2019.f <- factor(MUSEO, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

MUSIC_2019.f <- factor(MUSIC, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

ACMUS_2019.f <- factor(ACMUS, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

MONUM_2019.f <- factor(MONUM, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

#----------------------------------------------------
#THEATRE frequency
length(TEATRO_2019.f)

sixplus_2019$ETAMI.f <- ETAMI_2019.f
sixplus_2019$SESSO.f <- SESSO_2019.f
sixplus_2019$ISTRMI.f <- ISTRMI_2019.f
sixplus_2019$RIPMF.f <- RIPMF_2019.f
sixplus_2019$TEATRO.f <- TEATRO_2019.f
sixplus_2019$CINE.f <- CINE_2019.f
sixplus_2019$MUSEO.f <- MUSEO_2019.f
sixplus_2019$MUSIC.f <- MUSIC_2019.f
sixplus_2019$ACMUS.f <- ACMUS_2019.f
sixplus_2019$MONUM.f <- MONUM_2019.f
detach(sixplus_2019)
#----------------------------------------------
#DATASET 2020
setwd("E:\\DATA ANALYSIS AND BIG DATA LAB\\ESSAY")
library(readxl)
AVQ_2020 <- read_excel("AVQ_2020.xlsx")
View(AVQ_2020)

#we don't have sufficient observation to consider this variable
#So, remove column numbibm
AVQ_2020$NUMBIBM <- NULL

#new database
View(AVQ_2020)

#Create a database without NA
library(tidyverse)
AVQ_2020_wona <- filter(AVQ_2020, !is.na(TEATRO),!is.na(CINE), !is.na(MUSEO),
                        !is.na(MUSIC), !is.na(ACMUS), !is.na(MONUM),
                        !is.na(ETAMI), !is.na(RIPMF), !is.na(SESSO), !is.na(ISTRMI))
View(AVQ_2020_wona)

#Create a new database excluding children whose age is lower than 6 years old.
#population of people that have more than 6 years old.
sixplus_2020 <-  filter(AVQ_2020_wona, ETAMI >=3)
View(sixplus_2020)

#factor variables
attach(sixplus_2020)
ETAMI_2020.f <- factor(ETAMI,levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                  labels=c("0-2","3-5","6-10","11-13","14-15","16-17","18-19","20-24",
                           "25-34","35-44","45-54","55-59","60-64","65-74","75più"))

SESSO_2020.f <-factor(SESSO,levels=c(1,2),
                 labels=c("male","female"))

ISTRMI_2020.f <- factor(ISTRMI,levels=c(1,7,9,10,99),
                   labels=c("bachelor’s Degree and Master’s Degree","high school leaving certificate",
                            "middle school license","elementary school license, no educational qualifications",
                            "unavailable"))

RIPMF_2020.f <- factor(RIPMF, levels=c(1,2,3,4,5,9),
                  labels=c("northwest","northeast","centre","south","islands","unavailable"))

TEATRO_2020.f <- factor(TEATRO, levels=c(1,2,3,4,5),
                   labels=c("never","1-3","4-6","7-12","12più"))

CINE_2020.f <- factor(CINE, levels=c(1,2,3,4,5),
                 labels=c("never","1-3","4-6","7-12","12più"))

MUSEO_2020.f <- factor(MUSEO, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

MUSIC_2020.f <- factor(MUSIC, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

ACMUS_2020.f <- factor(ACMUS, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))

MONUM_2020.f <- factor(MONUM, levels=c(1,2,3,4,5),
                  labels=c("never","1-3","4-6","7-12","12più"))


sixplus_2020$ETAMI.f <- ETAMI_2020.f
sixplus_2020$SESSO.f <- SESSO_2020.f
sixplus_2020$ISTRMI.f <- ISTRMI_2020.f
sixplus_2020$RIPMF.f <- RIPMF_2020.f
sixplus_2020$TEATRO.f <- TEATRO_2020.f
sixplus_2020$CINE.f <- CINE_2020.f
sixplus_2020$MUSEO.f <- MUSEO_2020.f
sixplus_2020$MUSIC.f <- MUSIC_2020.f
sixplus_2020$ACMUS.f <- ACMUS_2020.f
sixplus_2020$MONUM.f <- MONUM_2020.f
detach(sixplus_2020)
#---------------------------------------------------------------

#LOWER PARTECIPATION
#difference in the partecipation at cultural services between 2019 and 2020
diff <- nrow(sixplus_2020) - nrow(sixplus_2019)
as.numeric(diff)
prop <- diff/(nrow(sixplus_2019)) *100
prop
print(c(prop, "%"))                #ok


#the decrease focusing on museum attendance
sixplus_2019 %>%
  count(MUSEO.f) %>%   #use this code
  mutate(teatro_rel = round((table(sixplus_2019$MUSEO.f)/length(sixplus_2019$MUSEO.f)),4) *100)
sixplus_2020 %>%
  count(MUSEO.f) %>%
  mutate(teatro_rel = round((table(sixplus_2020$MUSEO.f)/length(sixplus_2020$MUSEO.f)),4) *100)

MUSEOREL_2019 <- round(table(sixplus_2019$MUSEO.f)/length(sixplus_2019$MUSEO.f),4)
MUSEOREL_2019
MUSEOREL_2020 <- round(table(sixplus_2020$MUSEO.f)/length(sixplus_2020$MUSEO.f),4)
MUSEOREL_2020

library(dplyr)
par(mfrow=c(1,2))
barplot(MUSEOREL_2019, horiz=F, xlab = "Museum 2019", ylab = "Proportion",
        main="Museum attendance in 2019", beside = TRUE, ylim = c(0, 0.75),
        las = 1, space=0, col="steelblue")
barplot(MUSEOREL_2020, horiz=F, xlab = "Museum 2020", ylab = "Proportion",
        main="Museum attendance in 2020", beside = TRUE, ylim = c(0, 0.75),
        las = 1, space=0, col= "navyblue")
par(mfrow=c(1,1), mar=c(5, 3, 6, 0))
#mar – A numeric vector of length 4, which sets the margin sizes in the following order:
#bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).


#FOCUS ON THEATRE
#The difference is visible among the high frequencies of theater
TEATROASS <- table(sixplus_2019$TEATRO.f)
TEATROASS
TEATROASS <- table(sixplus_2020$TEATRO.f)
TEATROASS
TEATROREL <- table(sixplus_2019$TEATRO.f)/length(sixplus_2019$TEATRO.f)
TEATROREL
TEATROREL <- table(sixplus_2020$TEATRO.f)/length(sixplus_2020$TEATRO.f)
TEATROREL

sixplus_2019 %>%
  count(TEATRO.f) %>%
  mutate(teatro_rel = round((table(sixplus_2019$TEATRO.f)/length(sixplus_2019$TEATRO.f)),4) *100)
sixplus_2020 %>%
  count(TEATRO.f) %>%
  mutate(teatro_rel = round((table(sixplus_2020$TEATRO.f)/length(sixplus_2020$TEATRO.f)),4) *100)

TEATROREL_2019 <- round(table(sixplus_2019$TEATRO.f)/length(sixplus_2019$TEATRO.f),2)
TEATROREL_2019
TEATROREL_2020 <- table(sixplus_2020$TEATRO.f)/length(sixplus_2020$TEATRO.f)
TEATROREL_2020

library(dplyr)
par(mfrow=c(1,2))
barplot(TEATROREL_2019, horiz=F, xlab = "Theatre 2019", ylab = "Proportion",
        main="Theatre attendance in 2019", beside = TRUE, ylim = c(0, 1),
        las = 1, space=0, col="mediumorchid")
barplot(TEATROREL_2020, horiz=F, xlab = "Theatre 2020", ylab = "Proportion",
        main="Theatre attendance in 2020", beside = TRUE, ylim = c(0, 1),
        las = 1, space=0, col= "darkviolet")
par(mfrow=c(1,1), mar=c(5, 3, 6, 0))


#THEATER BY GEOGRAPHICAL AREA
#theater frequency according to the area of geographical area
install.packages("cowplot")
library(cowplot)
teatro_byrip_2019 <- ggplot(data = sixplus_2019) + 
  geom_bar(mapping = aes(x = TEATRO.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "PuRd") +
  labs(x="Theatre 2019", fill = "Area")
table(sixplus_2019$TEATRO.f, sixplus_2019$RIPMF.f)

teatro_byrip_2020 <- ggplot(data = sixplus_2020) + 
  geom_bar(mapping = aes(x = TEATRO.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "PuRd")  +
  labs(x="Theatre 2020", fill = "Area") 

# arrange the two plots in a single row
library(rlang)
prow <- plot_grid(
  teatro_byrip_2019 + theme(legend.position="none"),
  teatro_byrip_2020 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1)
prow

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  teatro_byrip_2019 + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(2, .4))


#FOCUS ON CINEMA
#CINE frequency
CINEASS <- table(sixplus_2019$CINE.f)
CINEASS
CINEASS <- table(sixplus_2020$CINE.f)
CINEASS
CINEREL <- table(sixplus_2019$CINE.f)/length(sixplus_2019$CINE.f)
CINEREL
CINEREL <- table(sixplus_2020$CINE.f)/length(sixplus_2020$CINE.f)
CINEREL

#CINEMA BY GEOGRAPHICAL AREA
#cinema frequency according to the area of geographical area
library(cowplot)
cine_byrip_2019 <- ggplot(data = sixplus_2019) + 
  geom_bar(mapping = aes(x = CINE.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Purples") +
  labs(x="Cinema 2019", fill = "Area") 

cine_byrip_2020 <- ggplot(data = sixplus_2020) + 
  geom_bar(mapping = aes(x = CINE.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Purples")  +
  labs(x="Cinema 2020", fill = "Area") 

# arrange the two plots in a single row
library(rlang)
prow <- plot_grid(
  cine_byrip_2019 + theme(legend.position="none"),
  cine_byrip_2020 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1)
prow

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  cine_byrip_2019 + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(2, .4))


#FOCUS ON MUSEUM
# frequency
MUSEOASS <- table(sixplus_2019$MUSEO.f)
MUSEOASS
MUSEOASS <- table(sixplus_2020$MUSEO.f)
MUSEOASS
MUSEOREL <- table(sixplus_2019$MUSEO.f)/length(sixplus_2019$MUSEO.f)
MUSEOREL
MUSEOREL <- table(sixplus_2020$MUSEO.f)/length(sixplus_2020$MUSEO.f)
MUSEOREL


#MUSEO BY GEOGRAPHICAL AREA
#theater frequency according to the area of geographical area
museo_byrip_2019 <- ggplot(data = sixplus_2019) + 
  geom_bar(mapping = aes(x = MUSEO.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Blues") +
  labs(x="Museum 2019", fill = "Area")


museo_byrip_2020 <- ggplot(data = sixplus_2020) + 
  geom_bar(mapping = aes(x = MUSEO.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Blues")  +
  labs(x="Museum 2020", fill = "Area") 

# arrange the two plots in a single row
library(rlang)
prow <- plot_grid(
  museo_byrip_2019 + theme(legend.position="none"),
  museo_byrip_2020 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1)
prow

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  museo_byrip_2019 + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(2, .4))


#FOCUS ON MUSIC
# frequency
MUSICASS <- table(sixplus_2019$MUSIC.f)
MUSICASS
MUSICASS <- table(sixplus_2020$MUSIC.f)
MUSICASS
MUSICREL <- table(sixplus_2019$MUSIC.f)/length(sixplus_2019$MUSIC.f)
MUSICREL
MUSICREL <- table(sixplus_2020$MUSIC.f)/length(sixplus_2020$MUSIC.f)
MUSICREL


#ACMUSIC BY GEOGRAPHICAL AREA
#theater frequency according to the area of geographical area
library(cowplot)
music_byrip_2019 <- ggplot(data = sixplus_2019) + 
  geom_bar(mapping = aes(x = MUSIC.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "BuGn") +
  labs(x="Classical music concerts 2019", fill = "Area")


music_byrip_2020 <- ggplot(data = sixplus_2020) + 
  geom_bar(mapping = aes(x = MUSIC.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "BuGn")  +
  labs(x="Classical music concerts 2020", fill = "Area") 

# arrange the two plots in a single row
library(rlang)
prow <- plot_grid(
  music_byrip_2019 + theme(legend.position="none"),
  music_byrip_2020 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1)
prow

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  music_byrip_2019 + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(2, .4))

#------------------------

#FOCUS ON ACMUSIC
# frequency
ACMUSASS <- table(sixplus_2019$ACMUS.f)
ACMUSASS
ACMUSASS <- table(sixplus_2020$ACMUS.f)
ACMUSASS
ACMUSREL <- table(sixplus_2019$ACMUS.f)/length(sixplus_2019$ACMUS.f)
ACMUSREL
ACMUSREL <- table(sixplus_2020$ACMUS.f)/length(sixplus_2020$ACMUS.f)
ACMUSREL


#ACMUSIC BY GEOGRAPHICAL AREA
#theater frequency according to the area of geographical area
library(cowplot)
acmusic_byrip_2019 <- ggplot(data = sixplus_2019) + 
  geom_bar(mapping = aes(x = ACMUS.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Greens") +
  labs(x="Music concerts 2019", fill = "Area")


acmusic_byrip_2020 <- ggplot(data = sixplus_2020) + 
  geom_bar(mapping = aes(x = ACMUS.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Greens")  +
  labs(x="Music concerts 2020", fill = "Area") 

# arrange the two plots in a single row
library(rlang)
prow <- plot_grid(
  acmusic_byrip_2019 + theme(legend.position="none"),
  acmusic_byrip_2020 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1)
prow

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  acmusic_byrip_2019 + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(2, .4))


#FOCUS ON MONUMENT
# frequency
MONUMASS <- table(sixplus_2019$MONUM.f)
MONUMASS
MONUMASS <- table(sixplus_2020$MONUM.f)
MONUMASS
MONUMREL <- table(sixplus_2019$MONUM.f)/length(sixplus_2019$MONUM.f)
MONUMREL
MONUMREL <- table(sixplus_2020$MONUM.f)/length(sixplus_2020$MONUM.f)
MONUMREL                
#or
MONUMASS <- sixplus_2019 %>%     
  count(MONUM.f)
MONUMASS


#MONUM BY GEOGRAPHICAL AREA
#theater frequency according to the area of geographical area
library(cowplot)
monum_byrip_2019 <- ggplot(data = sixplus_2019) + 
  geom_bar(mapping = aes(x = MONUM.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Oranges") +
  labs(x="Monuments 2019", fill = "Area")


monum_byrip_2020 <- ggplot(data = sixplus_2020) + 
  geom_bar(mapping = aes(x = MONUM.f, fill = RIPMF.f), position="fill") +
  scale_fill_brewer(palette = "Oranges")  +
  labs(x="Monuments 2020", fill = "Area") 

# arrange the two plots in a single row
library(rlang)
prow <- plot_grid(
  monum_byrip_2019 + theme(legend.position="none"),
  monum_byrip_2020 + theme(legend.position="none"),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 1)
prow

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  monum_byrip_2019 + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(prow, legend, rel_widths = c(2, .4))

###############################################################

#MOST REPRESENTATIVES: CINEMA & / OR MUSEUM

########
#CINEMA 
########
# a.1) cinema frequencies by age and sex in 2019
#we can focus on the medium frequence
sixplus_2019 %>% 
  filter(CINE == 3) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = ETAMI.f, fill = SESSO.f))  +
  scale_fill_manual(values=c("lightblue", "pink")) +
  labs(x="Age", title = "Cinema attendance by age and sex in 2019", 
       fill = "Gender") +
  theme(plot.title = element_text(face = "bold"))


# a.2) cinema frequencies by age and sex in 2020
#we can focus on the medium frequence
sixplus_2020 %>% 
  filter(CINE == 3) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = ETAMI.f, fill = SESSO.f))  +
  scale_fill_manual(values=c("lightblue", "pink")) +
  labs(x="Age", title = "Cinema attendance by age and sex in 2020", 
       fill = "Gender") +
  theme(plot.title = element_text(face = "bold"))


# b) education level
cine_4_2019 <- sixplus_2019 %>% 
  filter(CINE == c(4)) %>%
  count(CINE.f, ISTRMI.f) %>%  
  ggplot(mapping = aes(x = CINE.f, y = ISTRMI.f)) +
  geom_tile(mapping = aes(fill = n)) + 
  scale_fill_gradient2(low = "mediumvioletred",  high = "darkorchid") +
  labs(y= NULL, x="Cinema attendance in 2019")
cine_4_2019

cine_4_2020 <- sixplus_2020 %>% 
  filter(CINE == 4) %>%
  count(CINE.f, ISTRMI.f) %>%  
  ggplot(mapping = aes(x = CINE.f, y = ISTRMI.f)) +
  geom_tile(mapping = aes(fill = n)) + 
  scale_fill_gradient2(low = "mediumvioletred",  high = "darkorchid") +
  labs(y= NULL, x="Cinema attendance in 2020")
cine_4_2020
#-------------------

########
#MUSEUM
########

# a) education level
museo_4_2019 <- sixplus_2019 %>% 
  filter(MUSEO == c(4)) %>%
  count(MUSEO.f, ISTRMI.f) %>%  
  ggplot(mapping = aes(x = MUSEO.f, y = ISTRMI.f)) +
  geom_tile(mapping = aes(fill = n)) + 
  scale_fill_gradient(low = "slategray1",  high = "steelblue") +
  labs(y= "Education", x="Museum attendance in 2019")
museo_4_2019

museo_4_2020 <- sixplus_2020 %>% 
  filter(MUSEO == c(4)) %>%
  count(MUSEO.f, ISTRMI.f) %>%  
  ggplot(mapping = aes(x = MUSEO.f, y = ISTRMI.f)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_gradient(low = "slategray1",  high = "steelblue") +
  labs(y= "Education", x="Museum attendance in 2020")
museo_4_2020

#----end---------------


