library(xfun)
library(dplyr)
library(summarytools)
library(qgraph)
library(bootnet)
library(EGAnet)
library(networktools)

letter = read.csv("C:/Users/Brenna/Documents/R/Letter Data2.csv", stringsAsFactors = FALSE)

letterdata_f <- data.frame(letter)
letterdata <- na.omit(letterdata_f)

TCS <- letterdata$TCS
TCS <- letterdata$Anxious


#original dataframe with all variables
letterdata1<- letterdata %>% select(ï..Interest,	Sleep,	Fatigue,	Appetite,	Concentrating,	Slow,	Suicide,	Relax,	Restless,	Irritable,	Awful,	Appear,	Acceptance)


#new dataframe with variables removed based on goldbricker
#letterdata1<- letterdata %>% select(i..Interest,	Sleep,	Fatigue,	Appetite,	Bad, Concentrating,	Slow,	Suicide,	Anxious,	Control,	Worry,	Relax, Restless,	Irritable)

#dataframes of anxious variables
letterdataa <- letterdata %>% select(Anxious,	Control,	Worry,	Relax,	Restless,	Irritable,	Awful, Appear, Acceptance)

#dataframes of depression variables
letterdatad <- letterdata %>% select(ï..Interest,	Down,	Sleep,	Fatigue,	Appetite,	Bad,	Concentrating,	Slow,	Suicide, Appear, Acceptance)


#creating groups

Groupd <- structure(list(Depression = c(1:9), 
                         Gender = c(10, 11)), 
                    Names = c("Depressive Symptoms", "Gender Items"))

Groupa<- structure(list(Anxiety = c(1:7), 
                         Gender = c(8:9)), 
                    Names = c("Anxiety Symptoms", "Gender Items"))

#creating node names

Namesa <- c("Feeling anxious", "Uncontrollable worry", "Worrying about different things", "Trouble Relaxing", "Restlessness", "Irritability", "Fearful", "Appearance Congruence", "Identity Acceptance")
Namesd <- c("Low Interest", "Feeling depressed", "Trouble Sleeping", "Low energy", "Eating problems", "Feeling guilty", "Trouble concentrating", "Moving slow or restless", "Suicidal Ideation", "Appearance Congruence", "Identity Acceptance")


#running correlation as Spearman
lettercora <- cor(letterdataa, method = "spearman", use = "pairwise.complete.obs")

lettercord <- cor(letterdatad, method = "spearman", use = "pairwise.complete.obs")

#creating the graphs

#regularized partial correlation network - spearman correlation
g_lassoa <- qgraph(lettercora, graph = "glasso", sampleSize = nrow(letterdataa), tuning = 0.5, layout = "spring", theme = "colorblind", groups = Groupa, nodeNames = Namesa)
g_lassod <- qgraph(lettercord, graph = "glasso", sampleSize = nrow(letterdatad), tuning = 0.5, layout = "spring", theme = "colorblind", groups = Groupd, nodeNames = Namesd)

#unregularized partial correlation network - spearman correlation
#g_pcor <- qgraph(lettercor, graph = "pcor", sampleSize = nrow(letterdata1),  threshold = "bonferroni", alpha = 0.05, layout = "spring", theme = "colorblind", groups = Groups)

#bootstrapping anxiety
boot1a <- bootnet(letterdataa, default ="EBICglasso", nBoots = 10000, nCores = 8)
plot(boot1a, labels = TRUE, order = "sample") 

#bootstrapping depression
boot1d <- bootnet(letterdatad, default ="EBICglasso", nBoots = 10000, nCores = 8)
plot(boot1d, labels = TRUE, order = "sample") 

#bootstrapping for Stability
boot2a <- bootnet(letterdataa, default ="EBICglasso", nBoots = 10000, type = "case",  nCores = 8)

boot2d <- bootnet(letterdatad, default ="EBICglasso", nBoots = 10000, type = "case",  nCores = 8)

#stability plots
corStability(boot2a)
corStability(boot2d)

#bootstrapping for centrality
Networka <-estimateNetwork(letterdataa,default = "EBICglasso" ,  threshold = FALSE, corMethod = "spearman")
Networkd <- estimateNetwork(letterdatad,default = "EBICglasso" ,  threshold = FALSE, corMethod = "spearman")

boot3a <- bootnet(Networka, nBoots = 10000, nCores = 8, type = "case", statistics = c("expectedInfluence", "edge", "strength"))

boot3d <- bootnet(Networkd, nBoots = 10000, nCores = 8, type = "case", statistics = c("expectedInfluence", "edge", "strength"))

#centrality plots
centralityPlot(Networka, include = c("ExpectedInfluence", "Strength","Betweenness", "Closeness"), orderBy = "ExpectedInfluence")

centralityPlot(Networkd, include = c("ExpectedInfluence", "Strength", "Betweenness", "Closeness"), orderBy = "ExpectedInfluence")

#plot(boots, statistics = "ExpectedInfluence") + theme(legend.position = "none")

#goldbricker to remove redundant variables
goldbricker(letterdata1, p = 0.05, method = "hittner2003", threshold = 0.25,
           corMin = 0.5, progressbar = TRUE)

#EGA graph
ega<-EGA(letterdataa, plot.EGA = TRUE)
ega<-EGA(letterdatad, plot.EGA = TRUE)


#Relative Importance
net_relimp <- estimateNetwork(letterdata1,
                              default = "relimp",
                              normalize = FALSE)

plot(net_relimp, layout = "spring")

plot(boot1a, plot = "interval", split0 = TRUE, order="sample", labels=TRUE)



boot_relimp<- bootnet(net_relimp, nBoots = 100, nCores = 8)

plot(boot_relimp, order = "sample")


resultsa <- estimateNetwork(letterdataa, default = "EBICglasso", corMethod = "cor_auto", tuning = .5)

plot(resultsa)
centralityPlot(resultsa)
boot1 <- bootnet(resultsa, nCores = 8,
                 nBoots = 10000, type = 'nonparametric')
boot2 <- bootnet(resultsa, nCores = 8,
                 nBoots = 10000, type = 'case')
plot(boot1)
plot(boot2)
