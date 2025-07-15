####################################################################
### Cochlear Tuning in Early Aging Estimated with Three Methods ####
####################################################################

# For plotting and analyzing data in "Cochlear Tuning in Early Aging Estimated with Three Methods" 
# Manuscript submitted to Trends in Hearing 

# Author: Courtney SC Glavin
# Last Updated: June 17, 2025

##############
## Set up R ##
##############

## Install packages, as required
if (!require(pacman)) install.packages('pacman')       # for pkg loading
if (!require(dplyr)) install.packages('dplyr')         # for data wrangling
if (!require(ggplot2)) install.packages('ggplot2')     # for plotting
if (!require(car)) install.packages('car')             # for Levene's test
if (!require(rstatix)) install.packages('rstatix')     # for effect size (eta_squared)
if (!require(cowplot)) install.packages('cowplot')     # for combining figures
if (!require(sm)) install.packages('sm')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(tidyr)) install.packages('tidyr')
if (!require(knitr)) install.packages('knitr')
if (!require(ggeffects)) install.packages('ggeffects')

## Open packages 
pacman::p_load(pacman, dplyr, ggplot2, car, rstatix, sm, ggpubr, tidyr, cowplot, knitr, ggeffects)#, plotrix, RColorBrewer, digitize, sm, ggpubr)

# Define color palette
my.pal <- c("#F79044FF","#B6308BFF","#8305A7FF")
# Define shapes
my.shapes <- c(15,16,17)

#########################################
## Loads, analyzes, and plots LRF data ##
#########################################

source('D:/Analysis/Tuning Manuscript/Scripts/LRFplot_TuningManuscript.R', echo=FALSE)

##########################################
## Loads, analyzes, and plots fPTC data ##
##########################################

source('D:/Analysis/Tuning Manuscript/Scripts/fptcPlot_TuningManuscript.R', echo=FALSE)

###########################################
## Loads, analyzes, and plots SFOAE data ##
###########################################

source('D:/Analysis/Tuning Manuscript/Scripts/sfoaePhasePlot_TuningManuscript.R', echo=FALSE)

###############################################
## Loads, analyzes, and plots audiogram data ##
###############################################

source('D:/Analysis/Tuning Manuscript/Scripts/audiogram_TuningManuscript.R', echo=FALSE)

#########################################################################
## Bind rows across data frames for cross-method, group-level analysis ##
#########################################################################

# Bind rows across all tuning dataframes 
idLRF <- as.data.frame(dfLRF$id)
colnames(idLRF)[1] <- "id"
idPTC <- as.data.frame(dfPTC$id)
colnames(idPTC)[1] <- "id"
idSF <- as.data.frame(dfSF$id)
colnames(idSF)[1] <- "id"

# Bind rows & find unique participants across the three tuning dataframes
idAll <- bind_rows(idLRF, idPTC, idSF)  
idAll <- unique(idAll)
colnames(idAll)[1] <- "ID"

# Filter audio data by idAll (only use audio data of participants with tuning data)
audiodf <- semi_join(newdf, idAll, by = "ID")

# Filter tracking data by idAll (only use tracking data of participants with tuning data)
#trkdf <- semi_join(dftrk, idAll, by = "ID")

###############################################
## Figure 1: Average and plot audiogram data ##
###############################################

dfavg <- audiodf %>% 
  group_by(Group,Frequency) %>%
  summarize(avgL = mean(Left, na.rm=T),
            avgR = mean(Right, na.rm=T),
            avgTE = mean(testEarThreshold, na.rm=T),
            sdL = sd(Left, na.rm=T),
            sdR = sd(Right,na.rm=T),
            sdTE = sd(testEarThreshold,na.rm=T)
  )

# Now create different dataframe without the participants with elevated thresholds 
cleandf <- audiodf %>%
  filter(ID != 'CCGC074') %>%
  filter(ID != 'CCGC069') %>%
  filter(ID != 'CCGC070')

cleanavg <- cleandf %>% 
  group_by(Group,Frequency) %>%
  summarize(avgL = mean(Left, na.rm=T),
            avgR = mean(Right, na.rm=T),
            avgTE = mean(testEarThreshold, na.rm=T),
            sdL = sd(Left, na.rm=T),
            sdR = sd(Right,na.rm=T),
            sdTE = sd(testEarThreshold,na.rm=T)) 

cleanavg <- cleanavg %>% 
  filter(Group == 'C')

# Plot average data - test ear
p1 <- ggplot(dfavg, aes(x=Frequency, y=avgTE, colour=Group, shape=Group)) +
  geom_line(size=1.5) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=(avgTE-sdTE), ymax=(avgTE+sdTE), colour=Group), alpha=0.5) +
  geom_hline(yintercept=25, linetype="dotted") +
  xlab("Frequency (Hz)") + 
  ylab("Average Threshold (dB HL)") +
  scale_color_manual(name="",
                     labels=c("18-23 years","30-39 years","40+ years"),
                     values=my.pal) +
  scale_shape_manual(name="", 
                     labels=c("18-23 years","30-39 years","40+ years"),
                     values=my.shapes) +  # Use appropriate shapes here
  scale_y_reverse(limits=c(100,-10),
                  breaks=c(0,10,20,30,40,50,60,70,80,90,100),
                  labels=c(0,10,20,30,40,50,60,70,80,90,100)) +
  theme_bw(base_size=15) + 
  theme(legend.position = c(.8,.2), 
        legend.background = element_blank())

# Same plot, different y scale for inset
inset <- ggplot(dfavg, aes(x=Frequency, y=avgTE, colour=Group, shape=Group)) +
  geom_line(size=1.2) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=(avgTE-sdTE), ymax=(avgTE+sdTE), colour=Group), alpha=0.5) +
  xlab("Frequency (Hz)") + 
  ylab("Average Threshold (dB HL)") + 
  #geom_hline(yintercept=25, linetype="dotted") +
  scale_color_manual(name="",
                     labels=c("18-23 years","30-39 years","40+ years"),
                     values=my.pal) +
  scale_shape_manual(name="", 
                     labels=c("18-23 years","30-39 years","40+ years"),
                     values=my.shapes) +  # Use appropriate shapes here
  scale_y_reverse(limits=c(25,-8),
                  breaks=c(0,10,20),
                  labels=c(0,10,20)) +
  theme_bw(base_size=12) + 
  theme(#axis.title = element_blank(), 
        #axis.text = element_blank(),
        #axis.ticks = element_blank(),
        legend.position = "none")

# Add average audiogram for Group C without participants with elevated thresholds 
p <- p1 + 
  geom_line(data=cleanavg, aes(x=Frequency, y=avgTE, colour=Group), linetype="dashed", alpha=0.5) +
  geom_point(data=cleanavg, aes(x=Frequency, y=avgTE, colour=Group, shape=Group), size=4, alpha=0.5)

pinset <- inset + 
  geom_line(data=cleanavg, aes(x=Frequency, y=avgTE, colour=Group), linetype="dashed", alpha=0.5) +
  geom_point(data=cleanavg, aes(x=Frequency, y=avgTE, colour=Group, shape=Group), size=3, alpha=0.5)

pnew <- ggdraw() + 
  draw_plot(p) + 
  draw_plot(pinset, x=0.15, y=0.15, width=0.45, height=0.45)

pnew

# Save 
#ggsave("Fig1_audiogram.tiff", plot=pnew, width=10, height=8, units="in", dpi=800, path = 'C:/Users/glavi/OneDrive - UW/Documents/Manuscripts/Tuning Manuscript/Submission 2/Figures')

######################################
## Analysis: ANOVA (audiogram data) ## 
######################################

## Analyze data using two-way ANOVA
audiodf$Group <- as.factor(audiodf$Group)
audiodf$Frequency <- as.factor(audiodf$Frequency)

# Two-way ANOVA to evaluate differences in thresholds between groups
audio.aov <- aov(testEarThreshold ~ Frequency + Group + Frequency:Group, data=audiodf)
summary(audio.aov)

# Check normality - distribution of residuals
hist(audio.aov[[2]])
ggqqplot(audio.aov[[2]])

# Check homogeneity of variance
#leveneTest(testEarThreshold ~ Frequency*Group, data=audiodf)
# Variance not different between groups

# Calculate effect sizes (partial ns) 
#eta_squared(audio.aov)

# Perform bonferroni-corrected t-tests
#audio.nal <- pairwise.t.test(audiodf$testEarThreshold, audiodf$Group, p.adjust.method = "bonferroni")

# Evaluate effect size (Cohen's d)
#audiodf %>% cohens_d(testEarThreshold ~ Group, var.equal = T)

########################################################################
## Data cleaning/processing: Qerb comparisons across measurement type ##
########################################################################

# Three main dataframes from source files (in addition to behavioral threshold data): 
# 1) dfLRF 
# 2) dfSF
# 3) dfPTC

# Combine into one df 
# Note: This method will only include participants with data estimations from all three methods

# First, clean up dfPTC - two participants have 2 data points at the same frequency
dfPTC <- dfPTC %>%
  group_by(id, frequency, group, age, pta, ptahigh, ptalow) %>%
  summarize(Qerb = mean(Qerb))

dfLRF <- dfLRF %>%
  group_by(id, frequency, group, age, pta, ptahigh, ptalow) %>%
  summarize(Qerb = mean(Qerb))

common_df <- dfLRF %>%
  inner_join(dfPTC, by = c("id", "frequency")) %>%
  inner_join(dfSF, by = c("id", "frequency")) %>%
  dplyr::select(id, frequency)  # Keep only the id and frequency columns

# Filter the original data frames based on the common id and frequency
dfLRF_filtered <- dfLRF %>%
  inner_join(common_df, by = c("id", "frequency")) %>%
  mutate(type = 'LRF')

dfPTC_filtered <- dfPTC %>%
  inner_join(common_df, by = c("id", "frequency")) %>%
  mutate(type = 'fPTC') 

dfSF_filtered <- dfSF %>%
  inner_join(common_df, by = c("id", "frequency")) %>%
  mutate(type = 'SFOAE')

# Combine the filtered data frames into one long data frame
dfQcombined <- bind_rows(dfLRF_filtered, dfPTC_filtered, dfSF_filtered)

# Add test ear thresholds to column to (later) examine the relationship between audiometric thresholds and tuning
audiodf <- unique(audiodf)

audiodf <- audiodf %>%
  rename(group = Group) %>%
  rename(id = ID) %>%
  rename(frequency = Frequency) %>%
  select(id, group, frequency, testEarThreshold, age, pta)


#########################################################
## Figure 12: Qerb comparisons across measurement type ##
#########################################################

# Reorder factors for plotting
dfQcombined$type <- factor(dfQcombined$type, levels = c("fPTC", "LRF","SFOAE"))

# Add weights for plotting (based on number of data points for each condition)
dfQcombined <- dfQcombined %>%
  mutate(weight = n()) %>%
  ungroup()

p12 <- ggplot() + 
  geom_point(data=dfQcombined, aes(x=frequency, y=Qerb, colour=group, shape=group), size=3, alpha=0.3) +
  geom_smooth(data=dfQcombined, aes(x=frequency, y=Qerb, colour=group, linetype=group, weight=weight), method="lm", span=1,se=F, size=1.5) + 
  facet_wrap(~type, scales="free_y") +
  scale_colour_manual(name="",
                      labels= c("18-23 years", "30-39 years", "40+ years"),
                      aesthetics= c("colour","fill"),
                      values=my.pal) + 
  scale_shape_manual(name="", 
                      labels=c("18-23 years", "30-39 years", "40+ years"),
                      values=my.shapes) + 
  scale_linetype_manual(name="", 
                         labels=c("18-23 years", "30-39 years", "40+ years"),
                         values=c("solid", "dashed", "dotted")) +
  scale_x_log10(limits=c(1000,20000)) + 
  scale_y_log10(limits=c(5,100)) + 
  labs(x = 'Frequency (Hz)',
       y = bquote(italic(Q)[italic(erb)]~'')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "top", legend.title = element_blank())#, axis.text.x=element_text(size=15))

p12

# Save
#ggsave("Fig12_QerbbyType.tiff", plot=p12, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures/')

######################################################################
## Analysis (MLR): Predicting Q by frequency, age group, and method ##
######################################################################

# Ensure df has thresholds for a given individual across all Qerb types at a given frequency
dfQcombined <- dfQcombined %>%
  group_by(id, frequency) %>%
  tidyr::fill(thresholdsFPL, .direction = "downup") %>%
  ungroup()

# (Re) Define variable types
dfQcombined$frequency <- as.numeric(dfQcombined$frequency)
dfQcombined$group <- as.factor(dfQcombined$group)
dfQcombined$type <- as.factor(dfQcombined$type)
dfQcombined$age <- as.numeric(dfQcombined$age)

# Center frequency variable
dfQcombined$frequency_c <- dfQcombined$frequency - mean(dfQcombined$frequency, na.rm=T)

# Run model 
Qmodel <- lm(Qerb ~ frequency_c * age * type + thresholdsFPL, data=dfQcombined)
summary(Qmodel)

      # Extract and round model summary
       tidy_model <- tidy(Qmodel) %>%
         mutate(across(where(is.numeric), ~ round(.x, 4)))  
                      
      # Display table
      knitr::kable(tidy_model, caption = "MLR Summary", align = "c")

      # Look at estimated marginal means and contrasts                
      library(emmeans)
      emm <- emtrends(Qmodel, ~ type, var = "age")
      summary(emm, infer=T)
      contrast_emtr <- contrast(emm, method = "pairwise")
      summary(contrast_emtr, infer = TRUE)

# Check for normality
# resid_vals <- residuals(Qmodel)
# hist(resid_vals)
# sm.density(resid_vals,model="normal")
# 
# #Check for heteroskedasticity
# fitted_vals <- fitted(Qmodel) 
# plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
# abline(h=0)

# Visualize effects                      
#eff <- ggpredict(Qmodel, terms = c("frequency_c", "type", "age [20, 30, 40]")) 
                      
#plot(eff) +
  #scale_x_log10() + 
  #scale_y_log10() +
  #theme_bw(base_size=15)

################################
## Figure 11: Qerb across age ##
################################

# Create labels for plot
labs <- c('A' = "18-23 years",
          'B' = "30-39 years",
          'C' = "40+ years")

p11 <- 
  ggplot() + 
  geom_point(data=dfQcombined, aes(x=age, y=Qerb), size=3, alpha=0.7) +
  geom_smooth(data=dfQcombined, aes(x=age, y=Qerb), colour="black", linetype="dashed", method="lm", se=T, size=1) + 
  scale_y_log10() + 
  labs(x = bquote('Age (years)'),
       y = bquote(italic(Q)[italic(erb)]~'')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "none", 
        legend.title = element_blank())

p11

# Save
#ggsave("Fig11_Qerb_age.tiff", plot=p11, width=10, height=7, units="in", dpi=800, path = 'C:/Users/glavi/OneDrive - UW/Documents/Manuscripts/Tuning Manuscript/Submission 3/Figures')


####################################
## Analysis: Slopes from Figure 4 ##
####################################

# Estimate the same slopes from lm; control for PTA 
model <- lm(Qerb ~ age + thresholdsFPL, data = dfQcombined)
summary(model)

##############################################
## Figure 2: Behavioral Tracking Thresholds ##
##############################################

#dftrk <- rename(dftrk, "ID" = "id")
trkplot <- dftrk %>%
  semi_join(dfQcombined, by="id")

trkplot <- trkplot %>%
  mutate(group = case_when(
    grepl("A", id) ~ "A",
    grepl("B", id) ~ "B",
    TRUE ~ "C"
  ))

trkavg <- trkplot %>% 
  group_by(group,frequency) %>%
  summarize(avgTrk = mean(thresholdsFPL, na.rm=T),
            sdTrk = sd(thresholdsFPL, na.rm=T)
  )

# Clean df without individuals with elevated thresholds
cleandf <- trkplot %>%
  filter(id != 'CCGC074') %>%
  filter(id != 'CCGC069') %>%
  filter(id != 'CCGC070')

cleanavg <- cleandf %>% 
  group_by(group,frequency) %>%
  summarize(avgTrk = mean(thresholdsFPL, na.rm=T),
            sdTrk = sd(thresholdsFPL, na.rm=T)
  )

cleanavg <- cleanavg %>% filter(group == 'C')

# Plot average tracking data 
p2 <- ggplot(trkavg, aes(x=frequency, y=avgTrk, colour=group, shape=group)) +
  geom_line(size=1.5) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=(avgTrk-sdTrk), ymax=(avgTrk+sdTrk), colour=group), alpha=0.5) +
  xlab("Frequency (kHz)") + 
  ylab("Average Tracking Threshold (dB FPL)") +
  scale_color_manual(name="",
                     labels=c("18-23 years","30-39 years","40+ years"),
                     aesthetics=c("colour","fill"), 
                     values=my.pal) +
  scale_shape_manual(name="", 
                     labels=c("18-23 years","30-39 years","40+ years"),
                     values=my.shapes) +  # Use appropriate shapes here
  scale_y_reverse(limits=c(110,-3),
                  breaks=c(0,10,20,30,40,50,60,70,80,90,100,110),
                  label=c(0,10,20,30,40,50,60,70,80,90,100,110)) +
  scale_x_log10(limits=c(100,18000),
                breaks=c(125,250,500,750,1000,1500,2000,3000,4000,6000,8000,10000,11250,12500,14000,16000),
                label=c("",0.25,0.5,0.75,1,1.5,2,3,4,6,8,10,"",12.5,"",16)) +
  theme_bw(base_size=15) + 
  theme(legend.position = c(.2,.3), 
        legend.background = element_blank())

p2

p02 <- p2 + 
  geom_line(data=cleanavg, aes(x=frequency, y=avgTrk, colour=group, shape=group), linetype="dashed", alpha = 0.5) + 
  geom_point(data=cleanavg, aes(x=frequency, y=avgTrk, colour=group, shape=group), size=4, alpha=0.5) 

p02

# Save 
#ggsave("Fig2_tracking.tiff", plot=p02, width=10, height=8, units="in", dpi=800, path = 'C:/Users/glavi/OneDrive - UW/Documents/Manuscripts/Tuning Manuscript/Submission 3/Figures')
