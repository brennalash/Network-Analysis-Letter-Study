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
Network2a <- estimateNetwork(letterdataa, default = "EBICglasso", corMethod = "cor_auto") 
#warning a dense regularized network was selected

  #threshold = true
Network2at <- estimateNetwork(letterdataa, default = "EBICglasso", corMethod = "cor_auto", threshold = TRUE)
#note: network with lowest lambda was selected as best network

#plotting the network for anxiety symptoms

  #threshold = false
Networka_plot <- plot(Networka,layout = "spring", groups = Groupa, nodeNames = Namesa, title = "Spearman network Anxiety")

  #threshold = true
Networkat_plot <- plot(Networkat,layout = Networka_plot$layout, groups = Groupa, nodeNames = Namesa, title = "Thresholded spearman network Anxiety")

  #threshold = false
Network2a_plot <- plot(Network2a,layout = Networka_plot$layout, groups = Groupa, nodeNames = Namesa, title = "Poly network Anxiety")

  #threshold = true
Network2at_plot <- plot(Network2at,layout = Networka_plot$layout, groups = Groupa, nodeNames = Namesa, title = "Thresholded Poly network Anxiety")

#creating the network for depression symptoms- spearman correlations

  #threshold = false
Networkd <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs")) 
#warning a dense regularized network was selected

  #threshold = true
Networkdt <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman", use = "pairwise.complete.obs"), threshold = TRUE)
#note: network with lowest lambda was selected as best network

#creating network for depression symptoms- poly correlations

  #threshold = false
Network2d <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor_auto") 
#warning a dense regularized network was selected

  #threshold = true
Network2dt <- estimateNetwork(letterdatad, default = "EBICglasso", corMethod = "cor_auto", threshold = TRUE)
#note: network with lowest lambda was selected as best network

#plotting the network for depression symptoms

  #threshold = false
Networkd_plot <- plot(Networkd,layout = "spring", groups = Groupd, nodeNames = Namesd, title = "Spearman network Depression")

  #threshold = true
Networkdt_plot <- plot(Networkdt,layout = Networkd_plot$layout, groups = Groupd, nodeNames = Namesd, title = "Thresholded spearman network Depression")

  #threshold = false
Network2d_plot <- plot(Network2d,layout = Networkd_plot$layout, groups = Groupd, nodeNames = Namesd, title = "Poly network Depression")

  #threshold = true
Network2dt_plot <- plot(Network2dt,layout = Networkd_plot$layout, groups = Groupd, nodeNames = Namesd, title = "Thresholded Poly network Depression")

#--------------------------------------centrality inferences---------------------------------------------------------------------------------

#centrality plot for network for anxiety symptoms - 
centralityPlot(Networka, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")
centralityPlot(Network2a, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")

#centrality plot for network for depressive symptoms
centralityPlot(Networkd, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")
centralityPlot (Network2d, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence") 

#numerical centrality values
centrality(Networka)$InExpectedInfluence
centrality(Network2a)$InExpectedInfluence
centrality(Networkd)$InExpectedInfluence
centrality(Network2d)$InExpectedInfluence

#--------------------------------------network stability---------------------------------------------------------------------------------

#adjacency matrix
Networka$graph
Network2a$graph
Networkd$graph
Network2d$graph

#bootstrap routines, non-parametric bootstrap when handling ordinal data

    #anxiety network
boot_spearmana <- bootnet(Networka, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))
boot_polya <- bootnet(Network2a, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))

boot2_spearmana <- bootnet(Networka, nCores=8, nBoots=2500, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boot2_polya <- bootnet(Network2a, nCores=8, type = "case", nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))

    #depression network
boot_spearmand <- bootnet(Networkd, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))
boot_polyd <- bootnet(Network2d, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))

boot2_spearmand <- bootnet(Networkd, nCores=8, nBoots=2500, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boot2_polyd <- bootnet(Network2d, nCores=8, nBoots=2000, type="case", statistics = c("edge", "strength", "expectedInfluence"))

#plot edge weights CI
  
  #anxiety network
plot(boot_spearmana, labels = FALSE, order = "sample")
plot(boot_polya, labels = FALSE, order = "sample") 

  #depression network
plot(boot_spearmand, labels = FALSE, order = "sample") 
plot(boot_polyd, labels = FALSE, order = "sample") 


#diff test edge weights

  #anxiety network
plot(boot_spearmana, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(boot_polya, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

  #depression network
plot(boot_spearmand, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(boot_polyd, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

#expected influence plot stability

  #anxiety network
plot(boot2_spearmana, statistics = "expectedInfluence")
plot(boot2_polya, statistics = "expectedInfluence")

  #depression network
plot(boot2_spearmand, statistics = "expectedInfluence")
plot(boot2_polyd, statistics = "expectedInfluence")

#stability coefficients

  #anxiety network
corStability(boot2_spearmana, statistics = "expectedInfluence")
corStability(boot2_polya, statistics = "expectedInfluence")

  #depression network
corStability(boot2_spearmand, statistics = "expectedInfluence")
corStability(boot2_polyd, statistics = "expectedInfluence")

#diff test expected influence

  #anxiety network
plot(boot_spearmana, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(boot_polya, statistics = "expectedInfluence", order="sample", labels=TRUE)

  #depression network
plot(boot_spearmand, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(boot_polyd, statistics = "expectedInfluence", order="sample", labels=TRUE)

