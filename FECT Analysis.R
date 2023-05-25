# Ben and Connor Analysis
# BDS high Tech blog post????

# Content:
# 3. Data Load 
# 4. Descriptive Figures 
# 5. Run the FECT
# 6. Counter factual Plots

##################
###  Options   ###
##################
options(scipen=100000)
set.seed(42)
# file_date <- "" # define the file date we want to use

##################
###  Library   ###
##################
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(panelView)
library(fect)

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox"
}
if(Sys.info()[["user"]]=="Benjamin Glasner"){
  # Root folder
  path_project <- "C:/Users/Benjamin Glasner/Dropbox"
}
# Path to saved cohort data 
path_bds <- paste0(path_project,"/GitHub/BDS-hightech")
setwd(path_bds)


##################
###  Data Load ###
##################
using <- read_csv("bdsht_analysis.csv")
############################
###  Descriptive Figures ###
############################

panelview(share_emp_54 ~ treated, data = using, index = c("MSA","year"), 
          axis.lab = "time", 
          xlab = "Time", 
          ylab = "Unit", 
          gridOff = TRUE, 
          by.timing = TRUE,
          background = "white", 
          main = "Treatment Timing among High Cost of Living and Low Cost of Living Dynamic MSAs")


panelview(share_emp_54 ~ treated, data = using, index = c("MSA","year"), 
          type = "outcome",
          axis.lab = "time", 
          xlab = "Time", 
          ylab = "Unit", 
          gridOff = TRUE, 
          by.timing = TRUE,
          background = "white", 
          main = "Share of Employmnet in Professional Services")

############################
###         FECT         ###
############################
using <- using %>% mutate(hightechshare = (emp_HT/(emp_HT + emp_nonHT))*100,
                          share_emp_51 = share_emp_51*100)

dependent_list <- c("share_emp_51",
                    "hightechshare", 
                    "estabs_entry_rate_HT",
                    "estabs_entry_rate_nonHT",
                    "job_creation_rate_HT",
                    "job_creation_rate_nonHT", 
                    "reallocation_rate_HT",
                    "reallocation_rate_nonHT")

dependent_title <- c("Share of Employment NAICS 51",
                     "Share of Employment in High Tech",
                     "High Tech Estab. Entry Rate",
                     "Non-High Tech Estab. Entry Rate", 
                     "High Tech Job Creation Rate",
                     "Non-High Tech Job Creation Rate", 
                     "High Tech Reallocation Rate",
                     "Non-High Tech Reallocation Rate")
data_list <- list()
fect_list <- list()

for(j in seq_along(dependent_list)){
  data_list[[j]] <- using
  names(data_list[[j]])[which(colnames(data_list[[j]])==dependent_list[[j]])] <- "dependent"
  
}

sapply(using, function(x) sum(is.na(x)))

for(i in seq_along(data_list)){
  
  fect_list[[i]] <- fect(data = data_list[[i]],
                         formula = dependent ~ treated,
                         force = "two-way",
                         index = c("MSA","year"),
                         method = "both",
                         # method = "fe",
                         # method = "ife",
                         # method = "mc",
                         nlambda = 5,
                         vartype = "bootstrap",
                         nboots = 1000,
                         parallel = TRUE,
                         cores = 8,
                         # force = "time",
                         min.T0 = 3,
                         se = TRUE,
                         na.rm = FALSE)
}

for(i in seq_along(fect_list)){
  print("***********************************")
  print(i)
  print(dependent_title[[i]])
  print("***********************************")
  print(fect_list[[i]])
  print("***********************************")
  print("***********************************")
}

plot(fect_list[[i]], 
     id = c(48660,20700))
#############################################

###    Counter Factual Plot               ###

#############################################

Outcome_paths <- list()
counterfactual_plots <- list()

for (i in seq_along(fect_list)) {
  ###############################
  
  # Define who is and is not treated via the FEct
  IDs_treatmentstatus <- as.data.frame(cbind(fect_list[[i]][["id"]],fect_list[[i]][["unit.type"]]))
  IDs_treated <- IDs_treatmentstatus %>% filter(V2!=1)
  IDs_treated_list <- as.vector(IDs_treated$V1)
  
  IDs_control <- IDs_treatmentstatus %>% filter(V2==1)
  IDs_control_list <- as.vector(IDs_control$V1)
  
  idx <- match(IDs_treated_list, names(as.data.frame(fect_list[[i]][["eff"]])))
  idx_control <- match(IDs_control_list, names(as.data.frame(fect_list[[i]][["eff"]])))
  
  ###########################
  # capture the average predicted path for each age from FEct 
  eff <- as.data.frame(fect_list[[i]][["eff"]])[,idx] 
  Y.ct <- as.data.frame(fect_list[[i]][["Y.ct"]])[,idx] 
  
  eff <- as.data.frame(rowMeans(eff, na.rm = TRUE))
  Y.ct <- as.data.frame(rowMeans(Y.ct, na.rm = TRUE))
  
  effect_df <- cbind(eff,Y.ct,fect_list[[i]][["rawtime"]])
  names(effect_df) <- c("Effect","Predicted","Year")
  
  Outcome_paths[[i]] <- effect_df
  #############################
  # Calculate the observed average path for treated and control units
  
  treat <- data_list[[i]] %>% filter(MSA %in% IDs_treated_list) %>% group_by(year) %>% summarise(Treated = mean(dependent,na.rm=TRUE)) %>% mutate(Year = year)
  control <- data_list[[i]] %>% filter(MSA %in% IDs_control_list) %>% group_by(year) %>% summarise(Control = mean(dependent,na.rm=TRUE)) %>% mutate(Year = year)
  
  Outcome_paths[[i]] <- left_join(Outcome_paths[[i]],treat)
  Outcome_paths[[i]] <- left_join(Outcome_paths[[i]],control)
  
  counterfactual_plots[[i]] <- Outcome_paths[[i]] %>% 
    ggplot(aes(x = Year)) +
    geom_vline(xintercept = 2008) +
    geom_line(aes(y = Predicted,color = 'Predicted')) + 
    geom_line(aes(y = Treated,color = 'Treated')) + 
    geom_point(aes(y = Predicted,color = 'Predicted')) + 
    geom_point(aes(y = Treated,color = 'Treated')) + 
    ggtitle(dependent_title[[i]]) +
    ylab("Predicted or Observed") +
    theme(plot.title = element_text(size=20),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18),
          strip.text.x = element_text(size = 20),
          strip.text.y = element_text(size = 15),
          strip.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(colour = "grey"),
          panel.spacing = unit(2, "lines"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_rect(fill="white"),
          legend.key.width = unit(2,"cm"),
          legend.text = element_text(size = 18),
          legend.background = element_rect(fill=NA),
          plot.caption = element_text(hjust = 0.5,size = 15))    
  
}

for(i in seq_along(counterfactual_plots)){
png(filename = paste0(dependent_title[[i]],".png"), width = 1000, height = 1000)
plot(counterfactual_plots[[i]])
dev.off()
}


ATT <- list()
se <- list()

for (i in seq_along(fect_list)) {
  ATT[[i]] <- fect_list[[i]]$att.avg
  
  weighted_boots <- fect_list[[i]][["att.avg.boot"]]
  weighted_boots <- as.data.frame(t(weighted_boots))
  
  se[[i]] <- sd(weighted_boots$V1)
}

results_fect <- as.data.frame(cbind(dependent_list,
                                    dependent_title,
                                    do.call(rbind,ATT),
                                    do.call(rbind,se)))
results_fect <- results_fect %>% rename("ATT" = "V3",
                                        "SE" = "V4") %>%
  mutate(ATT = as.numeric(ATT),
         SE = as.numeric(SE))


results_fect <- results_fect %>% mutate(Significant = if_else((ATT - SE*1.95)>0,"Positive, Sig. (95%)",
                                                    if_else((ATT + SE*1.95)<0, "Negative, Sig. (95%)","Insignificant")))

group.colors <- c("Positive, Sig. (95%)" = "darkgreen", "Negative, Sig. (95%)" = "darkred","Insignificant" ="grey50")

P <- results_fect %>% 
  ggplot(aes(x = dependent_title,
             y = ATT,
             color = Significant)) + 
  geom_hline(yintercept = 0) +
  geom_point(size = 4) + 
  geom_errorbar(aes(ymin = ATT - 1.95*SE, ymax = ATT + 1.95*SE)) + 
  scale_color_manual(values=group.colors)+ 
  theme(plot.title = element_text(size=20),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_text(size = 15, angle = 90),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30),
        strip.background = element_rect(fill = "white"),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.background = element_rect(fill=NA)) +
  guides(col = guide_legend(title = "")) 
P




