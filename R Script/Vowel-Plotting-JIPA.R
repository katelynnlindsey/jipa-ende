## To make vowel plot graphs

##load libraries first
library(tidyverse)
library(scales)
library(rcompanion)
library(gmodels)
library(vowels)
library(graphics)
library(gmodels)
library(ggplot2)
library(ggpubr)
library(phonR)

##set working directory
getwd()
setwd("C:/Users/Kate Lindsey/Desktop/R/")

##load prenorm data
Ende.prenorm <- read.csv(file = "Ende_Vowels-prenorm.csv")
Ende.norm <- norm.lobanov(Ende.prenorm)
Ende <- scalevowels(Ende.norm)

##change column names
names(Ende)[names(Ende) == "Speaker"] <- "speaker"
names(Ende)[names(Ende) == "Context"] <- "word.vowel"
names(Ende)[names(Ende) == "Vowel"] <- "vowel_id"
names(Ende)[names(Ende) == "F*1"] <- "F1"
names(Ende)[names(Ende) == "F*2"] <- "F2"

##change vowels to IPA
Ende$vowel_id <- str_replace(Ende$vowel_id,"uh","ʌ")
Ende$word.vowel <- str_replace(Ende$word.vowel,"uh","ʌ")
Ende$vowel_id <- str_replace(Ende$vowel_id,"ih","ɘ")
Ende$word.vowel <- str_replace(Ende$word.vowel,"ih","ɘ")

##plot vowels with vowel symbols as tokens
with(Ende, plotVowels(F1, F2, vowel_id, plot.tokens = TRUE, pch.tokens = vowel_id, 
                      cex.tokens = 1.2, alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel_id, 
                      cex.means = 2, var.col.by = vowel_id, family = "Charis SIL", pretty = TRUE, 
                      ellipse.line=TRUE, xlim = c(2400, 800), ylim = c(800, 200),
                      main=paste("Ende Vowels","( N =",nrow(Ende),")"), xlab="F2 (Hz.)", ylab="F1 (Hz.)"))

##plot vowels with words as tokens
with(Ende, plotVowels(F1, F2, vowel_id, plot.tokens = TRUE, pch.tokens = word.vowel, 
                      cex.tokens = 1.2, alpha.tokens = 0.4, plot.means = TRUE, pch.means = vowel_id, 
                      cex.means = 2, var.col.by = vowel_id, family = "Charis SIL", pretty = TRUE, main=paste("Ende Vowels","( N =",nrow(Ende),")"), xlab="F2 (Hz.)", ylab="F1 (Hz.)"))


### Plot vowel durations
library(tidyverse)
library(hrbrthemes)
library(viridis)

setwd("C:/Users/Kate Lindsey/Desktop/R/")

Ende.duration <- read.csv(file = "Ende_Vowels-prenorm.csv")

Ende.duration$vowel_id <- str_replace(Ende.duration$vowel_id,"uh","ʌ")
Ende.duration$vowel_id <- str_replace(Ende.duration$vowel_id,"ih","ɘ")

Ende.duration$vowel_id <- factor(Ende.duration$vowel_id, levels=c("ʌ","ɘ","u","o","i","e","a"))

Ende.duration %>% ggplot( aes(x=duration.ms, y=vowel_id, fill=vowel_id)) + 
  geom_boxplot(notch=TRUE) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal() +
  theme(legend.position="none",plot.title=element_text(size=11)) +
  ggtitle("Ende vowel duration") +
  xlab("Duration (ms.)") +
  ylab("Vowel")

###OLD

## import dataset with headings subj, vowel, gender, f1, f2

data <- `norm_all_5_5500_for_R`

Encoding(data$vowel)

with(data, plotVowels(f1,f2))

with(data, plotVowels(f1, f2, vowel = vowel, group = gender))

par(mfrow = c(2, 2))
rounded <- ifelse(data$vowel %in% c("o", "u"), "round", "unround")
with(data, plotVowels(f1, f2, var.sty.by = vowel, var.col.by = vowel))
with(data, plotVowels(f1, f2, var.sty.by = vowel, var.col.by = gender))
with(data, plotVowels(f1, f2, var.sty.by = gender, var.col.by = subj))
with(data, plotVowels(f1, f2, var.sty.by = subj, var.col.by = rounded))


par(mfrow = c(1, 2))
f2col <- plotrix::color.scale(data$f2, cs1 = c(180, 0), cs2 = 100, cs3 = 60, color.spec = "hcl")
pts <- with(data, cbind(f2, f1))
cdist <- apply(pts, 1, function(i) dist(rbind(i, colMeans(pts))))
cdistcol <- plotrix::color.scale(cdist, cs1 = c(180, 360), cs2 = 100, cs3 = 60, color.spec = "hcl")
with(data, plotVowels(f1, f2, var.col.by = NA, col = f2col))
with(data, plotVowels(f1, f2, var.col.by = NA, col = cdistcol))

par(mfrow = c(2, 2))
with(data, plotVowels(f1, f2, var.sty.by = vowel, var.col.by = vowel, pretty = TRUE))
with(data, plotVowels(f1, f2, var.sty.by = vowel, var.col.by = gender, pretty = TRUE))
with(data, plotVowels(f1, f2, var.sty.by = gender, var.col.by = subj, pretty = TRUE))
with(data, plotVowels(f1, f2, var.sty.by = subj, var.col.by = rounded, pretty = TRUE))

## As we have seen, the default for plotVowels is to plot each individual vowel 
## token, potentially colored and styled in different ways. But sometimes a 
## summary view is helpful to look at group differences. This is easily achieved 
## by turning off the plot.tokens argument (which defaults to TRUE) and turning 
## on the plot.means argument (which defaults to FALSE). Now that weâre calculating 
## means for each vowel, the vowel and group arguments become important. These next 
## two plots show the mean F1 and F2 values for each vowel, but with different 
## grouping factors: on the left, the mean for each vowel is calculated within-speaker; 
## on the right, the means are calculated within gender groups.

par(mfrow = c(1, 2))
with(data, plotVowels(f1, f2, vowel, group = subj, plot.tokens = FALSE, plot.means = TRUE, 
                      var.col.by = gender, var.sty.by = vowel, pretty = TRUE, xlim = c(2900, 600), 
                      ylim = c(1000, 250)))
with(data, plotVowels(f1, f2, vowel, group = gender, plot.tokens = FALSE, plot.means = TRUE, 
                      var.col.by = gender, var.sty.by = vowel, pretty = TRUE, xlim = c(2900, 600), 
                      ylim = c(1000, 250)))

par(mfrow = c(1,1))
with(data, plotVowels(f1, f2, vowel, plot.tokens = TRUE, pch.tokens = vowel, 
                      cex.tokens = 1.2, alpha.tokens = 0.4, plot.means = TRUE, pch.means = vowel, 
                      cex.means = 4, var.col.by = vowel, family = "Charis SIL", pretty = TRUE))


par(mfrow = c(2, 2))
with(data, plotVowels(f1, f2, vowel, plot.tokens = TRUE, pch.tokens = vowel, cex.tokens = 1.2, 
                      alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel, cex.means = 2, var.col.by = vowel, 
                      ellipse.line = TRUE, pretty = TRUE))
with(data, plotVowels(f1, f2, vowel, plot.tokens = TRUE, pch.tokens = vowel, cex.tokens = 1.2, 
                      alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel, cex.means = 2, var.col.by = vowel, 
                      ellipse.line = TRUE, ellipse.conf = 0.95, pretty = TRUE))
with(data, plotVowels(f1, f2, vowel, group = gender, plot.tokens = FALSE, plot.means = TRUE, 
                      pch.means = vowel, cex.means = 2, var.col.by = vowel, ellipse.fill = TRUE, pretty = TRUE))
with(data, plotVowels(f1, f2, vowel, group = subj, plot.tokens = FALSE, plot.means = TRUE, 
                      pch.means = vowel, cex.means = 2, var.col.by = vowel, var.sty.by = subj, ellipse.line = TRUE, 
                      ellipse.fill = TRUE, fill.opacity = 0.1, pretty = TRUE))


## grayscale
with(data, plotVowels(f1, f2, vowel, plot.tokens = TRUE, pch.tokens = vowel, cex.tokens = 1.2, 
                      alpha.tokens = 0.2, plot.means = TRUE, pch.means = vowel, cex.means = 2, var.col.by = vowel, 
                      ellipse.line = TRUE, pretty = TRUE, col = c("gray65", "gray55", 
                                                                  "gray45","gray35","gray25","gray15","gray5")))


with(data,table(subj,vowel))

with(data, plotVowels(f1, f2, vowel, group = gender, plot.tokens = FALSE, plot.means = TRUE, 
                      var.col.by = gender, var.sty.by = vowel, pretty = TRUE, xlim = c(2400, 800), 
                      ylim = c(800, 200),legend.kwd="bottomright"))

with(data, plotVowels(f1, f2, vowel, group = age, plot.tokens = FALSE, plot.means = TRUE, 
                      var.col.by = age, var.sty.by = vowel, pretty = TRUE, xlim = c(2400, 800), 
                      ylim = c(800, 200),legend.kwd="bottomright"))

#PLOTTING AVERAGE DURATIONS
#Boxplot
library(readxl)
vowel_duration <- read_excel("C:/Users/Kate Lindsey/Desktop/vowel-duration.xlsx")
boxplot(vowel_duration, horizontal=TRUE, main="Vowel Duration")

library(ggplot2)
library(RColorBrewer)
library(readxl)
vowel_duration_data <- read_excel("C:/Users/Kate Lindsey/Desktop/vowel-duration-data.xlsx")
p <- ggplot(vowel_duration_data, aes(x=TextGridLabel, y=durationms)) + geom_boxplot(notch=TRUE)
p
p + coord_flip()
p + coord_flip() + scale_x_discrete(limits=c("a","o","e","u","i","ə","ɪ"))
p + coord_flip() + scale_x_discrete(limits=c("a","o","e","u","i","ə","ɪ")) + theme_minimal() + labs(title="Vowel Duration",y="Duration (ms)", x="Vowel")
