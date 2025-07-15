#############################################
### DPOAE Level Ratio Function (LRF) Data ###
#############################################

# For plotting LRF data & associated (estimated) Qerbs 
# Sub-analysis of Cochlear Tuning in Early Aging Estimated with Three Methods
# Manuscript submitted to Trends in Hearing

# Author: Courtney SC Glavin
# Last Updated: June 24, 2025
# Version: 3.0 

##############
## Set up R ##
##############

## Install packages, as required
if (!require(pacman)) install.packages('pacman')             # for pkg loading
if (!require(dplyr)) install.packages('dplyr')               # for data wrangling
if (!require(ggplot2)) install.packages('ggplot2')           # for plotting
if (!require(RColorBrewer)) install.packages('RColorBrewer') # for colors
if (!require(digitize)) install.packages('digitize')         # to grab data from Wilson et al. 2021
if (!require(sm)) install.packages('sm')                     # for sm.density
if (!require(ggpubr)) install.packages('ggpubr')             # for correlation plots
if (!require(tidyr)) install.packages('tidyr')               # for data pivoting
if (!require(cowplot)) install.packages('cowplot')           # for combining plots

## Open packages 
pacman::p_load(pacman, dplyr, ggplot2, plotrix, RColorBrewer, digitize, sm, ggpubr, tidyr, cowplot)

# Define color palette (for plotting)
my.pal <- c("#F79044FF","#B6308BFF","#8305A7FF", "black")
# Define shapes (for plotting)
my.shapes <- c(15,16,17,NA)

############################################################
## Initial data wrangling - LRF and behavioral thresholds ##
############################################################

## Read dataframe - specify path here 
df <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/indvLRFQ2024-06-28.csv') # LRF data
dfaudio <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/audioData_2024-05-13.csv') # audiogram data
te <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/participantTestEar.csv') # test ear of each participant 
dftrk <- read.csv('D:/Analysis/Behavioral Tracking/Analyzed Data/individualtrackingThresholds_2024-05-14.csv') # behavioral tracking thresholds of each participant

#########################
### LRF Data Cleaning ###
#########################

dfclean <- df %>%
  mutate(ratio = round(f2/f1, 3)) %>% # create f2/f1 ratio column
  filter (f2 > 1000) %>%              # filter out initial 2 participants who had data at additional frequencies 
  filter (f2 != 11250) %>%
  unique() 


dfclean <- dfclean %>%
  group_by(f2, group, id, type, ratio) %>%
  summarize(LRF = mean(LRF),
            NF = mean(NF),
            Qerb = mean(Qerb),
            highRatiom = mean(highRatiom),
            highRatiob = mean(highRatiob), 
            peak = mean(peak))# Create f2/f1 ratio column in LRF dataframe 


dfclean <- rename(dfclean, frequency = f2) # change variable names
dfclean$id <- gsub("_", "", dfclean$id) # Remove _ from participant IDs 

###########################
### Audiogram wrangling ###
###########################

# Rename mislabeled participants
dfaudio$ID[df$ID == "CCG_A_015"] <- "CCG_A_015_discontinued"
dfaudio$ID[df$ID == "CCG_B_29"] <- "CCG_B_029"

# Merge dfs (audiogram data & participant test ear)
dfaudio <- dfaudio %>% 
  left_join(te, by="ID")

# Filter by test ear
dfaudio <- dfaudio %>% 
  mutate(testEarThreshold = ifelse(testEar == 'R', Right, Left)
  )

# Remove _ from participant IDs 
dfaudio$ID <- gsub("_", "", dfaudio$ID)

# Create PTA column from audiogram data (0.5, 1, and 2 kHz)
pta <- dfaudio %>%
  filter(Frequency %in% c(500, 1000, 2000)) %>%
  mutate(selected_ear = ifelse(testEar == 'L', Left, Right)) %>% 
  group_by(ID) %>%
  summarise(pta = mean(selected_ear, na.rm = TRUE)) %>%
  ungroup() 

dfaudio <- dfaudio %>%
  left_join(pta, by = "ID")

# Reformat column name
dfaudio <- rename(dfaudio, id = ID)
dfaudio <- rename(dfaudio, frequency = Frequency)

#####################################
### Behavioral tracking wrangling ###
#####################################

# Reformat data
dftrk$frequency <- dftrk$frequency*1000
dftrk$id <- paste0("CCG_", dftrk$id)
dftrk$id <- gsub("_", "", dftrk$id)

dftrk <- dftrk %>% select(id, frequency, thresholdsFPL) %>%
  filter(frequency <= 16000) # No tuning data above 14 kHz

ptalow <- dftrk %>%
       filter(frequency %in% c(500, 1000, 2000)) %>%
       group_by(id) %>%
       summarise(ptalow = mean(thresholdsFPL, na.rm = TRUE)) %>%
       ungroup()

ptahigh <- dftrk %>%
       filter(frequency %in% c(8000, 10000, 12500, 14000, 16000)) %>%
       group_by(id) %>%
       summarise(ptahigh = mean(thresholdsFPL, na.rm = TRUE)) %>%
       ungroup() 

dftrk <- dftrk %>%
  left_join(ptalow, by = "id") %>%
  left_join(ptahigh, by = "id") 

##################################
### Merge all three dataframes ###
##################################

df <- left_join(dfclean, dfaudio, by = c("id", "frequency"))
df <- left_join(df, dftrk, by = c("id", "frequency"))
df <- unique(df) # Clear duplicate rows

df <- df %>%
  group_by(id) %>%
  tidyr::fill(age, pta, ptahigh, ptalow, .direction = "downup") %>%
  ungroup()

# Fix missing age in this dataframe (this person's audiogram was missing from our data set,
# therefore they inadvertently were filtered out when dfs were combined)
df <- df %>%
  mutate(
    age = case_when(
      grepl("CCGA002", id) ~ 20,      
      TRUE                  ~ age    # leave other rows unchanged
    )
  )

########################################################################
## Figure 3: Unmirrored vs. Mirrored comparisons within an individual ##
########################################################################

# Filter mirrored vs Unmirrored comparison across frequencies for one participant 
dfcleanCompare0 <- df %>% filter(id=='CCGA045') %>% filter(type==0) # unmirrored
dfcleanCompare1 <- df %>% filter(id=='CCGA045') %>% filter(type==1) # mirrored

# Additional cleaning (participant had multiple Qerbs estimated)
dfcleanCompare1$Qerb <- round(dfcleanCompare1$Qerb,3)
dfcleanCompare1 <- dfcleanCompare1 %>% filter(Qerb != 11.945) %>% filter(Qerb != 12.263) 

# Sort data to make labels for figure
dfunique0 <- dfcleanCompare0 %>% distinct(Qerb, .keep_all = TRUE)
dfunique0 <- dfunique0 %>% dplyr::select(frequency,type,Qerb)
dfunique1 <- dfcleanCompare1 %>% distinct(Qerb, .keep_all = TRUE)
dfunique1 <- dfunique1 %>% dplyr::select(frequency,type,Qerb)
dfunique <- rbind(dfunique0,dfunique1)

## Figure 3: Individual data (participant CCG_A_045) - unmirrored vs mirrored comparison 
p3 <- ggplot() + 
  geom_line(data=dfcleanCompare0, aes(x=ratio, y=LRF), colour='#008080', linewidth=1) + 
  geom_line(data=dfcleanCompare1, aes(x=ratio, y=LRF), linetype='dashed', linewidth=1, alpha=0.8) + 
  facet_wrap(~frequency, nrow=1) + 
  scale_x_log10(breaks=c(0.9,1.0,1.1,1.2,1.3,1.4,1.5),
                labels=c("","","1.1","1.2","1.3","1.4","")) + 
  labs(x = bquote(italic(f)[italic(2)]~'/'~italic(f)[italic(1)]~' Ratio'),
       y = ('Average DPOAE Level (dB SPL)')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x=element_text(size=10))

p3

# Save  
#ggsave("Fig2_LRFexample.tiff", plot=p3, width=10, height=5, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

##################################
## Additional LRF data cleaning ##
##################################

## Select mirrored data only if Qerb is lower than for unmirrored at same condition
filtered_df <- df %>%
  group_by(id, frequency) %>%
  summarize(
    max_Qerb_0 = max(Qerb[type == 0], na.rm = TRUE),
    max_Qerb_1 = max(Qerb[type == 1], na.rm = TRUE),
    best_type = ifelse(max_Qerb_0 > max_Qerb_1, 0, 1),
    .groups = 'drop'
  ) %>%
  inner_join(df, by = c("id", "frequency")) %>%
  filter(type == best_type) %>%
  dplyr::select(-max_Qerb_0, -max_Qerb_1, -best_type)

filtered_df$ratio <- round(filtered_df$ratio,5)

# Manually remove several problematic conditions - unless where otherwise noted, these conditions arise from 
# "extra data" where an LRF measure was initiated and then stopped partway through (e.g.,
# because participant moved, asked for a break, or some other artifact). It presents 
# as jitter. This occurred for 6 total conditions across all participants/frequencies.

dfrm <- tibble::tibble(
  id = c("CCGC074", "CCGA052", "CCGA065", "CCGA066", "CCGA057", "CCGA013"),
  frequency = c(2000, 8000, 8000, 8000, 8000, 8000)
)

filtered_df <- anti_join(filtered_df, dfrm, by = c("id", "frequency"))

#########################################
## Figure 4: LRFs by age and frequency ##
#########################################

# Create labels for plot
labs <- c('A' = "18-23 years",
          'B' = "30-39 years",
          'C' = "40+ years",
          '2000' = "2000",
          '4000' = "4000",
          '8000' = "8000",
          '10000' = "10000",
          '12500' = "12500",
          '14000' = "14000",
          '16000' = "16000") 

# Figure 4: Plot LRFs across age and frequency 
p4 <- ggplot(data=filtered_df, aes(x=ratio, y=LRF, colour=group, group=id)) + 
  geom_line(linewidth=0.7,alpha=0.4) + 
  geom_smooth(aes(group=group, color=group), method="loess", span=0.8, se=F, size=1.5) + 
  facet_grid(frequency~group, labeller = as_labeller(labs)) + 
  scale_colour_manual(name="",
                      labels= c("18-23 years", "30-39 years", "40+ years"),
                      aesthetics= c("colour","fill"),
                      values=my.pal) + 
  scale_x_log10(
    breaks=c(0.9,1.0,1.1,1.2,1.3,1.4,1.5),
    labels=c("","1.0","1.1","1.2","1.3","1.4","1.5")) + 
  labs(x = bquote(italic(f)[italic(2)]~'/'~italic(f)[italic(1)]~' Ratio'),
       y = ('Average DPOAE Level (dB SPL)')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "top", legend.title = element_blank())

p4

# Save
#ggsave("Fig3_LRFall.tiff", plot=p4, width=7, height=14, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

###############################################
## Figure 5: Plot LRF Qerb across age groups ##
###############################################

# Add loess fit data from Wilson et al. 2021; Figure 3, 62/52 panel.
source('D:/Analysis/Tuning Manuscript/Scripts/wilsonLRFdata_TuningManuscript.R', echo=FALSE)

# Grab unique Q values for plotting 
dfQ <- unique(filtered_df[, c("id","frequency","group","Qerb","age","pta","ptahigh","ptalow","thresholdsFPL","highRatiom","highRatiob","peak")])

# Figure 5: Qerb LRF across frequencies (& ages)
p5 <- ggplot() + 
  geom_point(data=dfQ, aes(x=frequency, y=Qerb, colour=group, shape=group), size=3, alpha=0.7) +
  geom_smooth(data=dfQ, aes(x=frequency, y=Qerb, colour=group, linetype=group), method="lm", se=F, size=1.5) + 
  scale_colour_manual(name="", 
                      labels=c("18-23 years", "30-39 years", "40+ years", "Wilson et al. (2021)"),
                      values=my.pal) + 
  scale_shape_manual(name="", 
                     labels=c("18-23 years", "30-39 years", "40+ years", "Wilson et al. (2021)"),
                     values=my.shapes) + 
  scale_linetype_manual(name="", 
                        labels=c("18-23 years", "30-39 years", "40+ years", "Wilson et al. (2021)"),
                        values=c("solid", "dashed", "dotted", "dashed")) + 
  scale_x_log10() + 
  scale_y_log10(limits=c(3,30)) + 
  labs(x = bquote(italic(f)[italic(2)]~' Frequency (Hz)'),
       y = bquote(italic(Q)[italic(erb)]~' LRF')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "top", 
        legend.title = element_blank())

# Add Wilson data 
p5 <- p5 + 
  geom_line(data=wilsonDF, aes(x=frequency, y=Q), colour="black", linetype="dashed", alpha=0.5) +
  geom_errorbar(data=wilsonDFavg, aes(x=frequency, ymin=(avgQ-sdQ), ymax=(avgQ+sdQ)), colour="black", alpha=0.5, linetype="dashed", width=0.02) + 
  annotate(geom="text", x=3000, y=7.72, label="Wilson et al. (2021)", color="black", size=5) 

p5

# Save 
#ggsave("Fig4_Qerb_byage.tiff", plot=p5, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

####################################
## Analysis: Slopes from Figure 4 ##
####################################

# Estimate the same slopes from lm; control for PTA 
models <- dfQ %>%
  group_by(group) %>%
  do(model = lm(Qerb ~ frequency + pta, data = .))

coefficients <- models %>%
  summarise(intercept = coef(model)[1], slope = coef(model)[2])

print(coefficients)

##########################################
#### Figure 6: Peak values of DPOAE LRF ##
##########################################

dfslopepeak <- unique(filtered_df[, c("id","frequency","group","peak","highRatiom","age","pta","ptalow","ptahigh")])

# Filter data (gets rid of one aberrant data point from CCG_B_035 at 4kHz, which is slightly below 0)
dfslopepeak <- dfslopepeak %>%
  filter(highRatiom > 0) 

# Pivot data into long format for plotting
df_long <- dfslopepeak %>%
  pivot_longer(cols = c(peak, highRatiom), 
               names_to = "type",         
               values_to = "value")       

df_long <- df_long %>% filter(type == "peak")

# Create labels for plotting
xlabs <- c("18-23", "30-39", "40+")

## Figure 6: LRF Peak boxplots
p6 <- ggplot(data=df_long, aes(x=group, y=value, colour=group)) + 
  geom_boxplot() +
  facet_wrap(~frequency,nrow = 1) + #, scales = "free", labeller = labeller(type = facet_labels)) +
  scale_colour_manual(name="",
                      labels= c("18-23 years", "30-39 years", "40+ years"),
                      aesthetics= c("colour","fill"),
                      values=my.pal) + 
  scale_x_discrete(labels= xlabs) +
  labs(x = 'Age Group (years)',
       y = 'DPOAE LRF Peak (dB SPL)') + 
  theme_bw(base_size=15) +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1))

p6

# Save 
#ggsave("Fig5_LRFPeakandSlope.tiff", plot=p6, width=14, height=7, units="in", dpi=800, path = 'C:/Users/glavi/OneDrive - UW/Documents/Manuscripts/Tuning Manuscript/Submission 2/Figures')

#########################
## Analysis: Peak Gain ##
#########################

dfpeak <- df_long %>% filter(type == 'peak') 

# Frequency as numeric variable 
dfpeak$frequency <- as.numeric(dfpeak$frequency)

# Linear regression
peakmodel <- lm(value ~ frequency * age + pta, data=dfpeak)
summary(peakmodel)

# Check interaction plot 
# interaction.plot(x.factor = dfpeak$group,
#                  trace.factor = dfpeak$frequency,
#                  response = dfpeak$value,
#                  fun = mean,
#                  type = "b",
#                  legend = TRUE,
#                  xlab = "age",
#                  ylab = "Mean of peak",
#                  trace.label = "Frequencies")
# 
# # Check for multicollinearity 
# car::vif(peakmodel)
# 
# # Check for normality
# resid_vals <- residuals(peakmodel)
# #hist(resid_vals)
# sm.density(peakmodel$residuals, model="normal")
# 
# # Check for heteroskedasticity
# fitted_vals <- fitted(peakmodel) 
# plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
# abline(h=0)

# Formatting function (for plotting - not used) 
# format_pval <- function(pval){
#   pval <- scales::pvalue(pval, accuracy= 0.001, add_p = TRUE)
#   gsub(pattern = "(=|<)", replacement = " \\1 ", x = pval)
# }

######################################################
## Save data for comparisons across tuning measures ##
######################################################

## Save data in new frame to compare across LRF, SFOAE, and fPTC
dfLRF <- dfQ %>%
  mutate(type = 'LRF') 

################################################
## Linear regression: Qerb (LRF) by age group ##
################################################

# Re(define) variable types
dfLRF$age <- as.numeric(dfLRF$age)
dfLRF$group <- as.factor(dfLRF$group)

# Control for PTA
Qmodel <- lm(Qerb ~ frequency * age + pta, data = dfLRF)
summary(Qmodel)

# Age effects not significant, though main effect of frequency is.

# # Check for normality
# resid_vals <- residuals(Qmodel)
# #hist(resid_vals)
# sm.density(Qmodel$residuals, model="normal")
# 
# # Check for heteroskedasticity
# fitted_vals <- fitted(Qmodel) 
# plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
# abline(h=0)