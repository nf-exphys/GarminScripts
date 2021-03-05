library(pwr2)

(0.055-0.038) #difference in means for post HIIT vs. MOD
(0.038-0.024) #difference in means for pre vs. post MOD

pwr2::ss.2way(a=3, b=3, alpha=0.05, 
              beta = 0.2, delta.A = 0.017,
              delta.B = 0.014,
              sigma.A = 0.01,
              sigma.B = 0.005, B=100)
#time is Factor A (0,15,30) and test is Factor B (pre/post)
#Using data from Peake et al. (2014) for succinate
#Total sample is 24


pwr2::pwr.plot(n=seq(4,12,2), k=4, alpha = 0.05, f=seq(0.2,0.8,0.2))
#n is sample size per group
#Note that this plot is for balanced one-way ANOVAs

library(tidyverse)

#I thought this was pre vs. post exercise but it's actually pre vs. post intervention
#All measurements were taken fasted :/

# data_path <- "./Koay 2021 effect of chronic exercise metabolomic supp_data.xlsx"
# koay_data <- readxl::read_xlsx(path = data_path, sheet = 2, skip=2)
# koay_data_clean <- koay_data %>%
#   separate(., col=2, into=c("pre_mean", "pre_SEM"), sep = "?") %>%
#   separate(., col=4, into=c("post_mean", "post_SEM"), sep = "?")
# 
# n.sqrt <- sqrt(56)
# 
# koay_data_clean <- koay_data_clean %>%
#   mutate(pre_SEM = as.numeric(pre_SEM),
#          post_SEM = as.numeric(post_SEM),
#          post_mean = as.numeric(post_mean),
#          pre_mean = as.numeric(pre_mean),
#          pre_sd = (pre_SEM/n.sqrt),
#          post_sd = (post_SEM/n.sqrt)) %>%
#   rename(biochem_name=`Biochemical Name`) %>%
#   select(biochem_name, contains("mean"), contains("sd"))


#Pub with metabolomics R stuff: https://www.mdpi.com/2218-1989/9/10/200/htm
install.packages("MetaboAnalystR")
av <- available.packages(filters = list())
av[av[, "Package"] == "MetaboAnalystR"]


#Using data from Nieman et al. (2015) on palmitoylcarnitine

(3.401451347-2.145186857) #difference in means for 1.5 hr post:pre ratio water-banana
(3.15-1.26) #difference in means for 
  #21 hour post:pre fold change vs. 1.5 hour post:pre fold changes
  #Used 21 hr since there wasn't a pre-measure reported & 21 hr is close to pre

0.74 #palmitoylcarnitine banana post:pre ratio SD
1.02 #palmitoylcarnitine water post:pre ratio SD

log(3.4) - log(2.16)
ss.2way(a=2, b=2, alpha=0.05, 
              beta = 0.2, delta.A = 1.256,
              delta.B = 1.89,
              sigma.A = 0.74,
              sigma.B = 1.02,
        f.A = NULL,
        f.B = NULL,
        B=100)


library(pwr)

cohen.ES(test = "anov", size = "large")
pwr.t.test(n=NULL, d=0.4, 
           sig.level = 0.05, power = 0.8, type = "paired",
           alternative = "two.sided")
#n=51, not possible...

pwr.t.test(n=NULL, d=(1.25/1.02), #based on banana/pear data
           sig.level = 0.05, power = 0.8, type = "paired",
             alternative = "two.sided")
#n=7.4 pairs, far more reasonable

#Look into Lakens pre-print on sample size justification

#One-way ANOVA for 3 times and 1 post measurement
cohen.ES(test = "anov", size = "large")
pwr.anova.test(k=3, n=NULL, f=0.4, sig.level = 0.05, power = 0.45)
#n=21 for 80% power and large effect size (0.4)
#n=10 for 45% power and large effect size


#Fingertip blood lactate from Seiler et al., (2007)?


#### Power analysis using raw data from Peake 2014 ####
library(pwr2); library(tidyverse)

data <- read_csv(file = "peake_sum_data.csv")

#Factor A is Protocol (MOD vs. HIIT)
#Factor B is Timepoint (POST vs. 1 H)

metabs <- colnames(data)[4:51]
all_metabs <- list()

for (i in 1:length(metabs)){
  #Prepare data for factor A
  f_A <- data %>% 
    drop_na() %>% #get rid of rows with calculations 
    filter(Timepoint == "POST" | Timepoint == "1 H") %>%
    select(1:3, contains(metabs[i])) %>%
    filter(Timepoint=="POST") %>% 
    pull(metabs[i])
  
  #Factor A effect size
  sd_pool_A <- sqrt(((f_A[2])^2 + (f_A[4])^2)/2)
  mean_diff_A <- (f_A[1] - f_A[3])
  effect_size_A <- mean_diff_A/sd_pool_A
  
  f_B <- data %>% 
    drop_na() %>% #get rid of rows with calculations 
    filter(Timepoint == "POST" | Timepoint == "1 H") %>%
    select(1:3, contains(metabs[i])) %>%
    #filter(Timepoint=="POST") %>% 
    filter(Protocol == "MOD") %>%
    pull(metabs[i])
  
  #Factor B effect size
  sd_pool_B <- sqrt(((f_B[3])^2 + (f_B[4])^2)/2)
  mean_diff_B <- abs(f_B[1]-f_B[2])
  effect_size_B <- mean_diff_B/sd_pool_B
  
  results <- ss.2way(a=2, b=2, alpha=0.05, beta=0.2, #80% power
                     delta.A = NULL, delta.B = NULL,
                     sigma.A = NULL, sigma.B = NULL,
                     f.A = effect_size_A, f.B = effect_size_B,
                     B=100) #B is the number of iterations, 100 is the default, doesn't seem to change result
  
  all_metabs[[i]] <- metabs[i]
  names(all_metabs[[i]]) <- metabs[i]
  all_metabs[[i]][[1]] <- as.numeric(results$n)
  
}


suff_pwr <- which(all_metabs < 10)
all_metabs[suff_pwr]

insuff_pwr <- which(all_metabs > 50)
all_metabs[insuff_pwr]

hist(all_metabs)
hist(as.numeric(unlist(all_metabs)),breaks = 15)
