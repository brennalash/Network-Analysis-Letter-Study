#loading packages that are needed
library(qgraph)
library(bootnet)
library(EGAnet)
library(networktools)
library(summarytools)
library(dplyr)


#--------------------------------------data preparation------------------------------------------------------------------------------------
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

#--------------------------------------setting up networks---------------------------------------------------------------------------------

#creating the network for anxiety symptoms- spearman correlations

  #threshold = false
Networka <- estimateNetwork(letterdataa, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs")) 
   #warning a dense regularized network was selected

  #threshold = true
Networkat <- estimateNetwork(letterdataa, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs"), threshold = TRUE)
   #note: network with lowest lambda was selected as best network

#creating network for anxiety symptoms- poly correlations

  #threshold = false
na1 <- qgraph(cor_auto(letterdataa), graph = "glasso", sampleSize = nrow(letterdataa),  
             theme = "colorblind", layout="spring", groups = Groupa, nodeNames = Namesa,
             title = "EBICglasso Anxiety", cut = 0)
   #warning: a dense regularized network was selected

  #threshold = true
na2 <- qgraph(cor_auto(letterdataa), graph = "glasso", sampleSize = nrow(letterdataa), 
             threshold = TRUE, theme = "colorblind", layout=na1$layout, 
             nodeNames = Namesa, groups = Groupa, title = "Thresholded EBICglasso Anxiety", cut = 0)
    #note: network with lowest lambda selected as best network

#plotting the network for anxiety symptoms

  #threshold = false
Networka_plot <- plot(Networka,layout = na1$layout, groups = Groupa, nodeNames = Namesa, title = "Spearman network Anxiety")

  #threshold = true
Networkat_plot <- plot(Networkat,layout = na1$layout, groups = Groupa, nodeNames = Namesa, title = "Thresholded spearman network Anxiety")

#------------------------------------------------------------------------------------------------------------------------------------------

#creating the network for depressive symptoms- spearman correlations

  #threshold = false
Networkd <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs")) 
    #warning: a dense regularized network was selected

  #threshold = true
Networkdt <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs"), threshold = TRUE)
    #note: network with lowest lambda was selected as best network

#creating network for depression symptoms- poly correlations

  #threshold = false
nd1 <- qgraph(cor_auto(letterdatad), graph = "glasso", sampleSize = nrow(letterdatad),  
              theme = "colorblind", layout="spring", nodeNames = Namesd, groups = Groupd, title = "EBICglasso Depression", cut = 0)
    #no warning message

  #threshold = true
nd2 <- qgraph(cor_auto(letterdatad), graph = "glasso", sampleSize = nrow(letterdatad), threshold = TRUE,
              theme = "colorblind", layout= nd1$layout, nodeNames = Namesd, groups = Groupd, title = "Threshold EBICglasso Depression", cut = 0)
    #no warning message

#plotting the network for depression symptoms
  #threshold = false
Networka_plot <- plot(Networkd,layout = nd1$layout, groups = Groupd, nodeNames = Namesd, title = "Spearman network Depression")
  #threshold = true
Networkat_plot <- plot(Networkdt,layout = nd1$layout, groups = Groupd, nodeNames = Namesd, title = "Thresholded spearman network Depression")

#--------------------------------------centrality inferences---------------------------------------------------------------------------------

#centrality plot for network for anxiety symptoms - 
centralityPlot(Networka, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")
centralityPlot(na1, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")

#centrality plot for network for depressive symptoms
centralityPlot(Networkd, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")
centralityPlot (nd1, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence") 

#numerical centrality values
centrality(Networka)$InExpectedInfluence
centrality(na1)$InExpectedInfluence
centrality(Networkd)$InExpectedInfluence
centrality(nd1)$InExpectedInfluence

#--------------------------------------network stability---------------------------------------------------------------------------------

#adjacency matrix
Networka$graph
na1$graph
Networkd$graph
nd1$graph

#bootstrap routines, non-parametric bootstrap when handling ordinal data

    #anxiety network
boot_spearmana <- bootnet(Networka, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))
boot_polya <- bootnet(na1, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))

boot2_spearmana <- bootnet(Networka, nCores=8, nBoots=2500, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boot2_polya <- bootnet(na1, nCores=8, type = "case", nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))

    #depression network
boot_spearmand <- bootnet(Networkd, nCores=8, nBoots=2500, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boot_polyd <- bootnet(nd1, nCores=8, nBoots=2000, type="case", statistics = c("edge", "strength", "expectedInfluence"))

boot2_spearmand <- bootnet(Networkd, nCores=8, nBoots=2500, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boo2t_polyd <- bootnet(nd1, nCores=8, nBoots=2000, type="case", statistics = c("edge", "strength", "expectedInfluence"))

#plot edge weights CI
  
  #anxiety network
plot(boot_spearmana, labels = FALSE, order = "sample")
plot(boot_polya, labels = FALSE, order = "sample") 
  #
plot(boot_spearmand, labels = FALSE, order = "sample") 
plot(boot_polyd, labels = FALSE, order = "sample") 


#diff test edge weights
plot(b1EtreshSP, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(b1EtreshSP, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(b1EtreshSP, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(b1EtreshSP, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

#expected influence plot stability
plot(b2EtreshSP, statistics = "expectedInfluence")
plot(b2EtreshSP, statistics = "expectedInfluence")
plot(b2EtreshSP, statistics = "expectedInfluence")
plot(b2EtreshSP, statistics = "expectedInfluence")

#stability coefficients
corStability(b2EtreshSP, statistics = "expectedInfluence")
corStability(b2EtreshSP, statistics = "expectedInfluence")
corStability(b2EtreshSP, statistics = "expectedInfluence")
corStability(b2EtreshSP, statistics = "expectedInfluence")

#diff test expected influence
plot(b1EtreshSP, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(b1EtreshSP, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(b1EtreshSP, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(b1EtreshSP, statistics = "expectedInfluence", order="sample", labels=TRUE)

