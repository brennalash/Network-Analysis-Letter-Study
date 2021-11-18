#loading packages that are needed
library(qgraph)
library(bootnet)
library(EGAnet)
library(networktools)
library(summarytools)
library(dplyr)


#--------------------------------------data preparation------------------------------------------------------------------------------------
letter = read.csv("C:/Users/Brenna/Documents/R/Letter Data2.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

letterdata_og <- data.frame(letter)
letterdata <- na.omit(letterdata_og)

#dataframe of all variables
letterdataall <- letterdata %>% select(Interest,	Down,	Sleep,	Fatigue,	Appetite,	Guilt,	Concentrating,	Slow,	Suicide,	Anxious,	Control,	Worry,	Relax, Restless,	Irritable,	Awful,	Appearance,	Acceptance)

#dataframe of anxious variables
letterdataa <- letterdata %>% select(Anxious,	Control,	Worry,	Relax,	Restless,	Irritable,	Awful, Appearance, Acceptance)

#dataframe of depression variables
letterdatad <- letterdata %>% select(Interest,	Down,	Sleep,	Fatigue,	Appetite,	Guilt,	Concentrating,	Slow,	Suicide, Appearance, Acceptance)

#dataframe of anxiety variables only 
letterdatanx <- letterdata %>% select(Anxious,	Control,	Worry,	Relax,	Restless,	Irritable,	Awful)

#dataframe of depressiom variables only
letterdatadep <- letterdata %>% select(Interest,	Down,	Sleep,	Fatigue,	Appetite,	Guilt,	Concentrating,	Slow,	Suicide)

#creating groups for networks
Groupd <- structure(list(Depression = c(1:9), 
                         Gender = c(10, 11)), 
                    Names = c("Depressive Symptoms", "Gender Items"))

Groupa<- structure(list(Anxiety = c(1:7), 
                        Gender = c(8:9)), 
                   Names = c("Anxiety Symptoms", "Gender Items"))


Groupsall <- structure(list(Depression = c(1:9), 
                         Anxiety = c(10:16), 
                         Gender = c(17, 18)), 
                    Names = c("Depressive Symptoms", "Anxiety Symptoms", "Minority Stress"))

#creating node names for networks
Namesa <- c("Feeling anxious", "Uncontrollable worry", "Worrying about different things", "Trouble Relaxing", "Restlessness", "Irritability", "Fearful", "Appearance Congruence", "Identity Acceptance")
Namesd <- c("Low Interest", "Feeling depressed", "Trouble Sleeping", "Low energy", "Eating problems", "Feeling guilty", "Trouble concentrating", "Moving slow or restless", "Suicidal Ideation", "Appearance Congruence", "Identity Acceptance")
Namesall <- c("Low Interest", "Feeling depressed", "Trouble Sleeping", "Low energy", "Eating problems", "Feeling guilty", "Trouble concentrating", "Moving slow or restless", "Suicidal Ideation","Feeling anxious", "Uncontrollable worry", "Worrying about different things", "Trouble Relaxing", "Restlessness", "Irritability", "Fearful", "Appearance Congruence", "Identity Acceptance")
#--------------------------------------setting up networks---------------------------------------------------------------------------------
#creating network for all symptoms
Network <- estimateNetwork(letterdataall, default ="EBICglasso", corMethod = "cor_auto") 

#creating network for anxiety symptoms only
Networkanx <- estimateNetwork(letterdatanx, default = "EBICglasso", corMethod = "cor_auto") 

#plotting network for anxiety symptoms only
Networkanx_plot <- plot(Networkanx)

#creating network for depression symptoms only
Networkdep <- estimateNetwork (letterdatadep, default = "EBICglasso", corMethod = "cor_auto")

#------------------------------------------------------------------------------------------------------------------------------------------

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
Network2at_plot <- plot(Network2at,layout = "spring", groups = Groupa, nodeNames = Namesa, title = "Network of Anxiety Symptoms and Protective Factors")

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

#-----------------------------------------------------------------------------------------------
#plotting network for all symptoms

Network_plot <- plot(Network, layout = "spring", groups = Groupsall, nodeNames = Namesall, title = "Depression & Anxiety Symptom Network")

#plotting the network for depression symptoms

  #threshold = false
#Networkd_plot <- plot(Networkd,layout = "spring", groups = Groupd, nodeNames = Namesd, title = "Spearman network Depression")

  #threshold = true
Networkdt_plot <- plot(Networkdt,layout = Networkd_plot$layout, groups = Groupd, nodeNames = Namesd, title = "Thresholded spearman network Depression")

  #threshold = false
#Network2d_plot <- plot(Network2d,layout = Networkd_plot$layout, groups = Groupd, nodeNames = Namesd, title = "Poly network Depression")

  #threshold = true
Network2dt_plot <- plot(Network2dt,layout = "spring", groups = Groupd, nodeNames = Namesd, title = "Network of Depression Symptoms and Protective Factors")

#--------------------------------------centrality inferences---------------------------------------------------------------------------------
#centrality plot for all symptoms
centralityPlot(Network, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")

#centrality plot for network for anxiety symptoms - 
#centralityPlot(Networkat, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")
centralityPlot(Network2at, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")

#centrality plot for network for depressive symptoms
#centralityPlot(Networkd, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence")
centralityPlot (Network2dt, include = c("ExpectedInfluence", "Strength"), orderBy = "ExpectedInfluence") 

#numerical centrality values
#centrality(Networka)$InExpectedInfluence
centrality(Network2at)$InExpectedInfluence
#centrality(Networkd)$InExpectedInfluence
centrality(Network2dt)$InExpectedInfluence
#centrality(Network)$InExpectedInfluence


#--------------------------------------network stability-------------------------------------------------------------------------------------

#adjacency matrix

  #anxiety network

#Networka$graph
Network2at$graph

  #depression network

#Networkd$graph
Network2dt$graph

#bootstrap routines

    #total network
#boot1 <- bootnet(Network, nCores=8, nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))
#boot2 <- bootnet(Network, nCores=8, type="case", nBoots=2500, statistics = c("edge", "strength", "expectedInfluence"))


    #anxiety network
#boot_spearmana <- bootnet(Networka, nCores=8, nBoots=10000, statistics = c("edge", "strength", "expectedInfluence"))
boot_polya <- bootnet(Network2at, nCores=8, nBoots=10000, statistics = c("edge", "strength", "expectedInfluence"))

#boot2_spearmana <- bootnet(Networka, nCores=8, nBoots=10000, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boot2_polya <- bootnet(Network2at, nCores=8, type = "case",  nBoots=10000, statistics = c("edge", "strength", "expectedInfluence"))

    #depression network
#boot_spearmand <- bootnet(Networkd, nCores=8, nBoots=10000, statistics = c("edge", "strength", "expectedInfluence"))
boot_polyd <- bootnet(Network2dt, nCores=8, nBoots=10000, statistics = c("edge", "strength", "expectedInfluence"))

#boot2_spearmand <- bootnet(Networkd, nCores=8, nBoots=10000, type="case", statistics = c("edge", "strength", "expectedInfluence"))
boot2_polyd <-bootnet(Networkdt, nCores=8, nBoots=10000, type="case", statistics = c("edge", "strength", "expectedInfluence"))


#plot edge weights CI
  
  #anxiety network
plot(boot_spearmana, labels = FALSE, order = "sample")
plot(boot_polya, labels = FALSE, order = "sample") 

  #depression network
plot(boot_spearmand, labels = FALSE, order = "sample") 
plot(boot_polyd, labels = FALSE, order = "sample") 


#diff test edge weights

  #total network
#plot(boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

  #anxiety network
#plot(boot_spearmana, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(boot_polya, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

  #depression network
#plot(boot_spearmand, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
plot(boot_polyd, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

#expected influence plot stability

  #anxiety network
#plot(boot2_spearmana, statistics = "expectedInfluence")
plot(boot2_polya, statistics = "expectedInfluence", title = "Anxiety Network")

  #depression network
#plot(boot2_spearmand, statistics = "expectedInfluence")
plot(boot2_polyd, statistics = "expectedInfluence", title = "Depression Network")

#stability coefficients
  
  #total network
corStability(boot2, statistics = "expectedInfluence")

  #anxiety network
#corStability(boot2_spearmana, statistics = "expectedInfluence")
corStability(boot2_polya, statistics = "expectedInfluence")
corStability(boot2_polya, statistics = "all")


  #depression network
#corStability(boot2_spearmand, statistics = "expectedInfluence")
corStability(boot2_polyd, statistics = "expectedInfluence")
corStability(boot2_polyd, statistics = "all")

#diff test expected influence

  #total network
#plot(boot1, statistics = "expectedInfluence", order="sample", labels=TRUE)

  #anxiety network
#plot(boot_spearmana, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(boot_polya, statistics = "expectedInfluence", order="sample", labels=TRUE)

  #depression network
#plot(boot_spearmand, statistics = "expectedInfluence", order="sample", labels=TRUE)
plot(boot_polyd, statistics = "expectedInfluence", order="sample", labels=TRUE)

#--------------------------------Bridging Symptoms---------------------------------------------------------------------------

#bridge expected influence

#bootstrap routines for bridge 
  
  #total network
boot_bridgeall <- bootnet(Network, nCores=8, nBoots=10000, type="case", statistics = "all", communities = Groupsall)
boot_bridgeall2 <- bootnet(Network, nCores = 8, nboots=10000,type="nonparametric", statistics="all", communities= Groupsall)


  #anxiety network
boot_bridgea <- bootnet(Network2a, nCores=8, nBoots=2500, type="case", statistics = "all", communities = Groupa)
boot_bridgea2 <- bootnet(Network2a, nCores = 8, nboots=2500,type="nonparametric", statistics="all", communities= Groupa)

  #depression network
boot_bridged <- bootnet(Network2d, nCores=8, nBoots=2500, type="case", statistics = "all", communities = Groupd)
boot_bridged2 <- bootnet(Network2d, nCores = 8, nboots=2500,type="nonparametric", statistics="all", communities= Groupd)

#Creating bridge networks 
  
  #anxiety network
bridge_centralitya <- bridge(network = Network2a$graph, communities = Groupa)

  #depression network
bridge_centralityd <- bridge(network = Network2d$graph, communities = Groupd)

#Bridge Stability

  #total network
corStability(boot_bridgeall)

  #anxiety network
corStability(boot_bridgea)

  #depression network
corStability(boot_bridged)

#Plots
  
  #anxiety network
plot(boot_bridgea2, statistics="bridgeStrength")

plot(boot_bridgea2, statistics="bridgeStrength", plot="difference")

plot(bridge_centralitya, include ="Bridge Strength", order = "value")

  
  #depression network
plot(boot_bridged2, statistics="bridgeStrength")

plot(boot_bridged2, statistics="bridgeStrength", plot="difference")

plot(bridge_centralityd, include="Bridge Strength", order = "value")

#Numeric Values for Bridge Strength
bridge_centralityd$`Bridge Strength`
bridge_centralitya$`Bridge Strength`

