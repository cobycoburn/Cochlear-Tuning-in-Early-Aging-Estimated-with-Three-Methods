####################################################################
### Cochlear Tuning in Early Aging Estimated with Three Methods ####
####################################################################

# For plotting LRF data & associated (estimated) Qerbs for Tuning Manuscript.

# Author: Courtney SC Glavin
# Last Updated: October 7, 2024

################################################################################
## Set up R and data
################################################################################

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

## Open packages 
pacman::p_load(pacman, dplyr, ggplot2, car, rstatix, sm, ggpubr, tidyr, cowplot)#, plotrix, RColorBrewer, digitize, sm, ggpubr)

################################################################################
## LRF data 
################################################################################

source('D:/Analysis/Scripts/Tuning Manuscript/LRFplot_TuningManuscript.R', echo=FALSE)

################################################################################
## fPTC data
################################################################################

source('D:/Analysis/Scripts/Tuning Manuscript/fptcPlot_TuningManuscript.R', echo=FALSE)

################################################################################
## SFOAE data 
################################################################################

source('D:/Analysis/Scripts/Tuning Manuscript/fptcPlot_TuningManuscript.R', echo=FALSE)

################################################################################
## Audiogram data
################################################################################

## Load in data
df <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/audioData_2024-05-13.csv') # data
te <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/participantTestEar.csv') # test ear of each participant 

# Remove participants who are not included in other data analysis (i.e., did not meet inclusion criteria for study)
df <- df %>%
  filter(ID != 'CCG_B_007') %>%
  filter(ID != 'CCG_B_039') %>% 
  filter(ID != 'CCG_C_081') %>% 
  filter(ID != 'CCG_C_061')

# Rename mislabeled participants
df$ID[df$ID == "CCG_A_015"] <- "CCG_A_015_discontinued"
df$ID[df$ID == "CCG_B_29"] <- "CCG_B_029"

# Merge dfs
df <- df %>% 
  left_join(te, by="ID")

# Filter by test ear
newdf <- df %>% 
  mutate(testEarThreshold = ifelse(testEar == 'R', Right, Left)
  )

# Remove _ from participantIDs 
newdf$ID <- gsub("_", "", newdf$ID)

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

################################################################################
## Figure 1: Average and plot audiogram data
################################################################################

# Define color palette
my.pal <- c("#F79044FF","#B6308BFF","#8305A7FF")
# Define shapes
my.shapes <- c(15,16,17)

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

# Add average audiogram for Group C without participants with elevated thresholds 
p <- p1 + 
  geom_line(data=cleanavg, aes(x=Frequency, y=avgTE, colour=Group), linetype="dashed", alpha=0.5) +
  geom_point(data=cleanavg, aes(x=Frequency, y=avgTE, colour=Group, shape=Group), size=4, alpha=0.5)

#p

# Save 
#ggsave("Fig1_audiogram.tiff", plot=p, width=10, height=8, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures/')

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
leveneTest(testEarThreshold ~ Frequency*Group, data=audiodf)
# Variance not different between groups

# Calculate effect sizes (partial ns) 
eta_squared(audio.aov)

# Perform bonferroni-corrected t-tests
audio.nal <- pairwise.t.test(audiodf$testEarThreshold, audiodf$Group, p.adjust.method = "bonferroni")

# Evaluate effect size (Cohen's d)
audiodf %>% cohens_d(testEarThreshold ~ Group, var.equal = T)

################################################################################
## Data cleaning/processing: Qerb comparisons across measurement type
################################################################################

# Three main dataframes from source files (in addition to audiogram data): 
# 1) dfLRF 
# 2) dfSF
# 3) dfPTC

# Combine into one df - Method 1 (not used in manuscript)
#dfQ <- bind_rows(dfLRF, dfPTC, dfSF) # Note: this method of merging saves all data regardless of whether there are corollary data points

# Combine into one df - Method 2 (used in manuscript)
# Note: This method will only include participants with data estimations from all three methods

# First, clean up dfPTC - two participants have 2 data points at the same frequency
dfPTC <- dfPTC %>%
  group_by(id, frequency, group) %>%
  summarize(Qerb = mean(Qerb))

# Change frequency back to integer class
dfPTC$frequency <- as.integer(as.character(dfPTC$frequency))

dfLRF <- dfLRF %>%
  group_by(id, frequency, group) %>%
  summarize(Qerb = mean(Qerb))

# Change frequency back to integer class
dfSF$frequency <- as.integer(as.character(dfSF$frequency))

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
  select(id, group, frequency, testEarThreshold)

dfQcombined <- dfQcombined %>%
  inner_join(audiodf, by = c("id", "group", "frequency"))

################################################################################
## Not used: Qerb comparisons across measurement type (BOXPLOTS)
################################################################################

# Labels for plotting
# labs <- c('A' = "18-23 years",
#           'B' = "30-39 years",
#           'C' = "40+ years",
#           '2000' = "2000",
#           '4000' = "4000",
#           '8000' = "8000",
#           '10000' = "10000",
#           '12500' = "12500",
#           '14000' = "14000",
#           '16000' = "16000") 

# Boxplots of data across type
# p20 <- ggplot(dfQcombined, aes(x=type, y=Qerb, colour=group, group=type)) + 
#   geom_boxplot() +
#   facet_grid(group ~ frequency , scales="free", labeller=as_labeller(labs)) +
#   scale_colour_manual(name="",
#                       labels= c("18-23 years", "30-39 years", "40+ years"),
#                       aesthetics= c("colour","fill"),
#                       values=my.pal) +
#   labs(x = ('Measurement Type'),
#        y = bquote(italic(Q)[italic(erb)]~'')) + 
#   theme_bw(base_size=15) +
#   theme(legend.position = "none", legend.title = element_blank())#, axis.text.x=element_text(angle=45, hjust=1))  
# 
# 
# p20 

################################################################################
## Figure 11: Qerb comparisons across measurement type 
################################################################################

# Reorder factors for plotting
dfQcombined$type <- factor(dfQcombined$type, levels = c("LRF", "SFOAE","fPTC"))

# Add weights for plotting (based on number of data points for each condition)
dfQcombined <- dfQcombined %>%
  group_by(frequency, group, type) %>%
  mutate(weight = n()) %>%
  ungroup()

# Get rid of conditions (for plotting) with only one data point so as to not significantly skew smoothed fit
#dfQcombined2 <- dfQcombined %>%
#  filter(weight > 1)

p21 <- ggplot() + 
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

p21

# Save
#ggsave("Fig11_QerbbyType.tiff", plot=p21, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures/')

################################################################################
## Figure 12: Correlations of Qerb across measurement type 
################################################################################

## Correlation plots - Qerb of different types
plotlabs <- c('A' = "18-23 years",
              'B' = "30-39 years",
              'C' = "40+ years")

# Correlation calculations

# Group A, SF vs. PTC
SFa <- dfQcombined %>% filter(group == 'A') %>% filter(type == 'SFOAE')
PTCa <- dfQcombined %>% filter(group == 'A') %>% filter(type == 'fPTC')
cor.test(SFa$Qerb,PTCa$Qerb)

# Group A, SF vs. LRF
LRFa <- dfQcombined %>% filter(group == 'A') %>% filter(type == 'LRF')
cor.test(SFa$Qerb,LRFa$Qerb)

# Reshape data for plotting
dfwide <- dfQcombined %>% tidyr::pivot_wider(names_from = type, values_from = Qerb)
#dfwide <- dfwide %>% filter(frequency <= 8000)

# SFOAE vs. fPTC
p1 <- ggscatter(data=dfwide, 
               x="SFOAE", 
               y="fPTC", 
               color="group",  # Map color to a variable
               palette=my.pal,
               add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' SFOAE')),
       y = (bquote(italic(Q)[italic(erb)]~' fPTC'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

p2 <- ggscatter(data=dfwide, 
                x="SFOAE", 
                y="fPTC", 
                color="black",  # Map color to a variable
                #palette=my.pal,
                add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  #facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' SFOAE')),
       y = (bquote(italic(Q)[italic(erb)]~' fPTC'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

# Combine plots
p3 <- cowplot::plot_grid(p1,p2)

#####
# SFOAE vs. LRF 
p4 <- ggscatter(data=dfwide, 
          x="SFOAE", 
          y="LRF", 
          color="group",  # Map color to a variable
          palette=my.pal,
          add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' SFOAE')),
       y = (bquote(italic(Q)[italic(erb)]~' LRF'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

p5 <- ggscatter(data=dfwide, 
                x="SFOAE", 
                y="LRF", 
                color="black",  # Map color to a variable
                #palette=my.pal,
                add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  #facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' SFOAE')),
       y = (bquote(italic(Q)[italic(erb)]~' LRF'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

# Combine plots
p6 <- cowplot::plot_grid(p4,p5)

#p6

#####
# fPTC vs. LRF 
p7 <- ggscatter(data=dfwide, 
                x="fPTC", 
                y="LRF", 
                color="group",  # Map color to a variable
                palette=my.pal,
                add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' fPTC')),
       y = (bquote(italic(Q)[italic(erb)]~' LRF'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

p8 <- ggscatter(data=dfwide, 
                x="fPTC", 
                y="LRF", 
                color="black",  # Map color to a variable
                #palette=my.pal,
                add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  #facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' fPTC')),
       y = (bquote(italic(Q)[italic(erb)]~' LRF'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

# Combine plots
p9 <- cowplot::plot_grid(p7,p8)

#p9

#####
# Combine all three plots 
p10 <- cowplot::plot_grid(p3,p6,p9, nrow=3)

#p10

# Save
#ggsave("Fig12_correlations.tiff", plot=p10, width=16, height=8, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures/')

################################################################################
## Analysis (for Discussion): Audiometric thresholds and Qerb
################################################################################
#test <- dfQcombined %>% filter(type == 'SFOAE' & frequency == 4000 & testEarThreshold < 30)

ggscatter(data=dfQcombined, 
          x="testEarThreshold", 
          y="Qerb", 
          #color="group",  # Map color to a variable
          palette=my.pal,
          add = "reg.line") + 
  stat_cor(label.x=Inf, hjust=1.1) + 
  facet_wrap(frequency ~ type) + #, labeller=as_labeller(plotlabs)) + 
  #labs(x = (bquote(italic(Q)[italic(erb)]~' SFOAE')),
  #     y = (bquote(italic(Q)[italic(erb)]~' fPTC'))) +
  theme_bw(base_size=15) +
  theme(legend.position="none")

# Figure not shown - no statistically significant relationships 

################################################################################
## Analysis (MLR): Predicting Q by frequency, age group, and method 
################################################################################

Qmodel <- lm(Qerb ~ frequency * group * type, data=dfQcombined)
summary(Qmodel)

# Check for normality
resid_vals <- residuals(Qmodel)
hist(resid_vals)
sm.density(resid_vals,model="normal")

#Check for heteroskedasticity
fitted_vals <- fitted(Qmodel) 
plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
abline(h=0)

################################################################################
## Figure 13 (NOT USED): Qerb across methods; collapsed age groups 
################################################################################
# 
# # Second color palette for Q type
# my.pal2 <- c('#33638DFF','#29AF7FFF','#FF964F')#,'FDE725FF')
# 
# ## By age group 
# avgQ <- dfQcombined %>%
#   group_by(frequency, group, type) %>%
#   summarize(avgQ = mean(Qerb), 
#             sdQ = sd(Qerb)) 
# 
# 
# ## Collapse across age groups
# avgQall <- dfQcombined %>%
#   group_by(frequency, type) %>%
#   summarize(avgQ = mean(Qerb), 
#             sdQ = sd(Qerb))
# 
# p17 <- ggplot(avgQall, aes(x=frequency, y=avgQ, colour=type)) + 
#   geom_point(size=2, alpha=0.4) +
#   #geom_errorbar(data=avgQall, aes(x=frequency, ymin=avgQ-sdQ, ymax=avgQ+sdQ), position=position_dodge(width=0.1), alpha=0.4, width=0.02) + 
#   geom_errorbar(aes(ymin=avgQ-sdQ, ymax=avgQ+sdQ), alpha=0.4, width=0.02) +
#   geom_smooth(data=avgQall, aes(x=frequency, y=avgQ, colour=type), method="lm", se=F, size=1.2) + 
#   scale_colour_manual(name="",
#                       #labels= c("fPTC", "DPOAE LRF", "SFOAE Phase Gradient Delay"),
#                       aesthetics= c("colour","fill"),
#                       values=my.pal2) + 
#   scale_x_log10() + 
#   scale_y_log10(limits=c(5,100)) + 
#   labs(x = 'Frequency (Hz)',
#        y = bquote(italic(Q)[italic(erb)]~'')) + 
#   theme_bw(base_size=15) +
#   theme(legend.position = "right", legend.title = element_blank())#, axis.text.x=element_text(size=15))
# 
# p17 