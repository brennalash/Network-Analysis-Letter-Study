library(qgraph)
library(bootnet)
library(EGAnet)
library(networktools)
library(summarytools)

#data preparation
letter = read.csv("C:/Users/Brenna/Documents/R/Letter Data2.csv", stringsAsFactors = FALSE)

letterdata_og <- data.frame(letter)
letterdata <- na.omit(letterdata_og)

#dataframe of anxious variables
letterdataa <- letterdata %>% select(Anxious,	Control,	Worry,	Relax,	Restless,	Irritable,	Awful, Appear, Acceptance)

#dataframe of depression variables
letterdatad <- letterdata %>% select(ï..Interest,	Down,	Sleep,	Fatigue,	Appetite,	Bad,	Concentrating,	Slow,	Suicide, Appear, Acceptance)


#creating groups for networks
Groupd <- structure(list(Depression = c(1:9), 
                         Gender = c(10, 11)), 
                    Names = c("Depressive Symptoms", "Gender Items"))

Groupa<- structure(list(Anxiety = c(1:7), 
                        Gender = c(8:9)), 
                   Names = c("Anxiety Symptoms", "Gender Items"))

#creating node names for networks

Namesa <- c("Feeling anxious", "Uncontrollable worry", "Worrying about different things", "Trouble Relaxing", "Restlessness", "Irritability", "Fearful", "Appearance Congruence", "Identity Acceptance")
Namesd <- c("Low Interest", "Feeling depressed", "Trouble Sleeping", "Low energy", "Eating problems", "Feeling guilty", "Trouble concentrating", "Moving slow or restless", "Suicidal Ideation", "Appearance Congruence", "Identity Acceptance")

#creating the network for anxiety symptoms
Networka <- estimateNetwork(letterdataa, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs"))

#plotting the network for anxiety symptoms
plot(Networka,layout = "spring", groups = Groupa, nodeNames = Namesa)

#creating the network for depressive symptoms
Networkd <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs"))

#plotting the network for depressive symptoms
plot(Networkd, sampleSize = nrow(letterdatad),layout = "spring", groups = Groupd, nodeNames = Namesd)

#centrality plot for network for anxiety symptoms
centralityPlot(Networka, include = c("ExpectedInfluence", "Strength"))

#centrality plot for network for depressive symptoms
centralityPlot(Networkd, include = c("ExpectedInfluence", "Strength"))

#edge weight accuracy for network for anxiety symptoms
boots1a <- bootnet(Networka, nBoots = 10000, nCores = 8)
plot(boots1a, order = "sample")

#edge weight accuracy for network for depression symptoms
boots1d <- bootnet(Networkd, nBoots = 10000, nCores = 8)
plot(boots1d, order = "sample")

#centrality stability for anxiety network
boots2a <- bootnet(Networka, nBoots = 10000, nCores = 8, type = "case")
plot(boots2a)
corStability(boots2a)

#centrality stability for depression network
boots2d <- bootnet(Networkd, nBoots = 10000, nCores = 8, type = "case")
plot(boots2d)
corStability(boots2d)

#significant differences btwn edges for anxiety network
plot(boots1a, "edge", plot = "difference",
     onlyNonZero = TRUE, order = "sample")
plot(boots1a, "strength")

#significant differences btwn edges for depression network
plot(boots1d, "edge", plot = "difference",
     onlyNonZero = TRUE, order = "sample")
plot(boots1d, "strength")