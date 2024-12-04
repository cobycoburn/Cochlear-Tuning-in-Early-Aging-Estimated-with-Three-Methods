################################################################################
### LRF Data ###
################

# For plotting LRF data & associated (estimated) Qerbs for Tuning Manuscript.

# Author: Courtney Glavin
# Last Updated: October 8, 2024

################################################################################
## Set up R and data
################################################################################

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

## Read dataframe - specify path here 
#df <- read.csv('D:/LRF/indvLRFQ2024-06-28.csv')
df <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/')

# Define color palette (for plotting)
my.pal <- c("#F79044FF","#B6308BFF","#8305A7FF", "black")
# Define shapes (for plotting)
my.shapes <- c(15,16,17,NA)

# Create f2/f1 ratio 
df$ratio <- (df$f2)/(df$f1)
dfclean <- df
dfclean$ratio <- round(dfclean$ratio,3)

# Remove duplicate rows
dfclean <- unique(dfclean)

# Remove conditions from which all participants don't have data (some pilot participants had data at 1000 & 11250 Hz)
dfclean <- dfclean %>%
  filter (f2 > 1000) %>% 
  filter(f2 != 11250) 

# Average over given participant if they have multiple trials at one f2
dfclean <- dfclean %>%
  group_by(f2, group, id, type, ratio) %>%
  summarize(LRF = mean(LRF),
            NF = mean(NF),
            Qerb = mean(Qerb))

# Aberrant estimation error for one participant (with level criterion set to -25 dB SPL) - remove from analysis
dfrm <- dfclean %>%
  filter(id == 'CCG_C_020') %>%
  filter(f2 == 4000) 

# Filter for new dataframe 
dfclean <- anti_join(dfclean, dfrm, by="id")

################################################################################
## Figure 2: Unmirrored vs. Mirrored comparisons within an individual 
################################################################################

# Filter mirrored vs Unmirrored comparison across frequencies for one participant 
dfcleanCompare0 <- dfclean %>% filter(id=='CCG_A_045') %>% filter(type==0) # unmirrored
dfcleanCompare1 <- dfclean %>% filter(id=='CCG_A_045') %>% filter(type==1) # mirrored

# Additional cleaning (participant had multiple Qerbs estimated)
dfcleanCompare1$Qerb <- round(dfcleanCompare1$Qerb,3)
dfcleanCompare1 <- dfcleanCompare1 %>% filter(Qerb != 11.945) %>% filter(Qerb != 12.263) 

# Sort data to make labels for figure
dfunique0 <- dfcleanCompare0 %>% distinct(Qerb, .keep_all = TRUE)
dfunique0 <- dfunique0 %>% dplyr::select(f2,type,Qerb)
dfunique1 <- dfcleanCompare1 %>% distinct(Qerb, .keep_all = TRUE)
dfunique1 <- dfunique1 %>% dplyr::select(f2,type,Qerb)
dfunique <- rbind(dfunique0,dfunique1)

## Figure 1: Individual data (participant CCG_A_045) - unmirrored vs mirrored comparison 
p1 <- ggplot() + 
  geom_line(data=dfcleanCompare0, aes(x=ratio, y=LRF), colour='#008080', linewidth=1) + 
  geom_line(data=dfcleanCompare1, aes(x=ratio, y=LRF), linetype='dashed', linewidth=1, alpha=0.8) + 
  facet_wrap(~f2, nrow=1) + 
  scale_x_log10(breaks=c(0.9,1.0,1.1,1.2,1.3,1.4,1.5),
                labels=c("","","1.1","1.2","1.3","1.4","")) + 
  labs(x = bquote(italic(f)[italic(2)]~'/'~italic(f)[italic(1)]~' Ratio'),
       y = ('Average DPOAE Level (dB SPL)')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x=element_text(size=10))

#p1 

# Save  
#ggsave("Fig2_LRFexample.tiff", plot=p1, width=10, height=5, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

################################################################################
## Extra analysis (not used in manuscript): Compare Unmirrored vs. Mirrored Q values 
################################################################################
# 
# Qavg <- dfclean %>%
#   group_by(type, group, f2) %>%
#   summarize(avg = mean(Qerb),
#             sd = sd(Qerb))
# 
# ## Plot: Unmirrored vs. Mirrored Qerbs across age groups (not currently used as figure) ##
# typenames <- c('0' = "Unmirrored",
#                '1' = "Mirrored",
#                '2000' = "2000",
#                '4000' = "4000",
#                '8000' = "8000",
#                '10000' = "10000",
#                '12500' = "12500",
#                '14000' = "14000",
#                '16000' = "16000")
# 
# # Plot mirrored & unmirrored Qerbs across groups 
# ggplot(dfclean, aes(x=f2, y=Qerb, colour=group)) + 
#   geom_point(size=3) + 
#   geom_smooth(method="lm", se=T) + 
#   facet_wrap(~type, labeller = as_labeller(typenames)) +
#   scale_x_log10() +
#   scale_colour_manual(name="",
#                       labels= c("18-23 years", "30-39 years", "40+ years"),
#                       aesthetics= c("colour","fill"),
#                       values=my.pal) + 
#   theme_bw(base_size = 18) 

################################################################################
## Additional data cleaning
################################################################################

## Select mirrored data only if Qerb is lower than for unmirrored at same condition
filtered_df <- dfclean %>%
  group_by(id, f2) %>%
  summarize(
    max_Qerb_0 = max(Qerb[type == 0], na.rm = TRUE),
    max_Qerb_1 = max(Qerb[type == 1], na.rm = TRUE),
    best_type = ifelse(max_Qerb_0 > max_Qerb_1, 0, 1),
    .groups = 'drop'
  ) %>%
  inner_join(df, by = c("id", "f2")) %>%
  filter(type == best_type) %>%
  dplyr::select(-max_Qerb_0, -max_Qerb_1, -best_type)

filtered_df$ratio <- round(filtered_df$ratio,5)

# Manually remove several problematic conditions - unless where otherwise noted, 
# these conditions generally arise from 
# "extra data" where an LRF measure was initiated and then stopped partway through (e.g.,
# because participant moved, asked for a break, or some other artifact). It presents 
# as jitter. This occurred for 6 total conditions across all participants/frequencies.

dfrm <- dfclean %>%
  filter(id == 'CCG_C_074') %>%
  filter(f2 == 2000) 

filtered_df <- anti_join(filtered_df, dfrm, by=c("id","f2"))

dfrm2 <- dfclean %>%
  filter(id == 'CCG_A_052') %>%
  filter(f2 == 8000) 

filtered_df <- anti_join(filtered_df, dfrm2, by=c("id","f2"))

dfrm3 <- dfclean %>% # artifact
  filter(id == 'CCG_A_065') %>%
  filter(f2 == 8000) 

filtered_df <- anti_join(filtered_df, dfrm3, by=c("id","f2"))

dfrm4 <- dfclean %>%
  filter(id == 'CCG_A_066') %>%
  filter(f2 == 8000) 

filtered_df <- anti_join(filtered_df, dfrm4, by=c("id","f2"))

dfrm5 <- dfclean %>%
  filter(id == 'CCG_A_057') %>%
  filter(f2 == 8000)

filtered_df <- anti_join(filtered_df, dfrm5, by=c("id","f2"))

dfrm6 <- dfclean %>%  # artifact
  filter(id == 'CCG_A_013') %>%
  filter(f2 == 8000) 

filtered_df <- anti_join(filtered_df, dfrm6, by=c("id","f2"))

################################################################################
## Figure 3: LRFs by age and frequency
################################################################################

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

# Figure 3: Plot LRFs across age and frequency 
p2 <- ggplot(data=filtered_df, aes(x=ratio, y=LRF, colour=group, group=id)) + 
  geom_line(linewidth=0.7,alpha=0.4) + 
  geom_smooth(aes(group=group, color=group), method="loess", span=0.8, se=F, size=1.5) + 
  facet_grid(f2~group, labeller = as_labeller(labs)) + 
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

#p2

# Save
#ggsave("Fig3_LRFall.tiff", plot=p2, width=7, height=14, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

################################################################################
## Figure 4: Plot Qerb across age group
################################################################################

# Note: adding loess fit data from Wilson et al. 2021; Figure 3, 62/52 panel.
# All data were extracted using Grabit in Matlab. The dataframe is built manually here.

# wilsonDF - the loess fit line only
wilsonDF <- data.frame(
  frequency = c(1000.33970726055, 2006.50588039036, 4023.53432658475, 
                6039.56067316453, 8035.55385518328, 9946.42905583272, 
                12462.8658889361, 14007.5006547176, 15999.7331643981),
  Q = c(5.75977100995213, 6.37716347919087, 7.66985558394774, 
            8.61118278733335, 9.29114150212901, 9.32773806667332, 
            9.03372550488849, 8.77902277398051, 8.44012567278906)
)

# Remove fvals < 2 kHz in Wilson df (not tested here)
wilsonDF <- wilsonDF %>% filter(frequency >= 2000)

# wilsonDF2 contains the individual data points from the figure
wilsonDF2 <- data.frame(
  frequency = c(1000, 1000, 1000, 1000, 1000, 
              1000, 2000, 2000, 2000, 2000, 
              2000, 4000, 4000, 4000, 4000, 
              4000, 4000, 4000, 6000, 6000, 
              6000, 6000, 6000, 6000, 6000, 
              8000, 8000, 8000, 8000, 8000, 
              8000, 8000, 10000, 10000, 10000, 
              10000, 10000, 10000, 10000, 12500, 
              12500, 12500, 12500, 12500, 12500, 
              12500, 12500, 14000, 14000, 14000, 
              14000, 14000, 14000, 14000, 16000, 
              16000, 16000, 16000, 16000, 16000, 
              16000),
  Q = c(4.98834675187882, 5.81294982859289, 5.92808627547304, 6.18947746147641, 6.33688046644726, 
              7.10034698897541, 9.93741966352706, 6.42951890525901, 6.23084050786102, 5.80606705209330, 
              4.79077996992862, 5.15540763953256, 6.44714649103038, 8.31962801763884, 8.48435266738027, 
              8.72054906369798, 9.14092247479924, 9.96473672284129, 10.6029128185992, 9.91884385813485, 
              9.06320205765849, 8.47853198657009, 7.77758667840091, 7.53725167361156, 7.13443597122382, 
              15.0617254933162, 12.7933441682340, 9.91404418964582, 9.83664076721457, 9.27448601977851, 
              8.37528693960134, 8.08470660311781, 11.6395133763626, 11.1042345841980, 10.8885659761621, 
              8.87940981117479, 8.47112405913460, 7.35554749512743, 7.24103507675156, 14.7224706863476, 
              13.9357537455064, 10.7994184091021, 10.6730760276988, 10.4657058601592, 9.30381098225026, 
              8.17414745821041, 7.26665968912244, 13.9331365057466, 11.0112528963710, 10.6291410120984, 
              8.97939369594330, 7.32246474634323, 5.26690979629485, 4.41463102798044, 5.18375263833250, 
              6.95686193645752, 8.39824916913407, 9.04802504672779, 11.0519759150190, 11.3150980503367, 
              17.4201954717043)
)

# Remove fvals < 2 kHz in Wilson df2 (not measured here)
wilsonDF2 <- wilsonDF2 %>% filter(frequency >= 2000)

# Create averages from wilsonDF2 data frame in order to plot error bars
wilsonDFavg <- wilsonDF2 %>%
  group_by(frequency) %>%
  summarize(avgQ = mean(Q), 
            sdQ = sd(Q))

# Grab unique Q values for plotting 
dfQ <- unique(filtered_df[, c("id","f2","group","Qerb")])

# Plot the data
p4 <- ggplot() + 
  geom_point(data=dfQ, aes(x=f2, y=Qerb, colour=group, shape=group), size=3, alpha=0.7) +
  geom_smooth(data=dfQ, aes(x=f2, y=Qerb, colour=group, linetype=group), method="lm", se=F, size=1.5) + 
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
p4 <- p4 + 
  geom_line(data=wilsonDF, aes(x=frequency, y=Q), colour="black", linetype="dashed", alpha=0.5) +
  geom_errorbar(data=wilsonDFavg, aes(x=frequency, ymin=(avgQ-sdQ), ymax=(avgQ+sdQ)), colour="black", alpha=0.5, linetype="dashed", width=0.02) + 
  annotate(geom="text", x=3000, y=7.72, label="Wilson et al. (2021)", color="black", size=5) 

#p4

# Save 
#ggsave("Fig4_Qerb_byage.tiff", plot=p4, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

####################################
## Analysis: Slopes from Figure 4 ##
####################################

# Estimate the same slopes from lm
models <- filtered_df %>%
  group_by(group) %>%
  do(model = lm(Qerb ~ f2, data = .))

coefficients <- models %>%
  summarise(intercept = coef(model)[1], slope = coef(model)[2])

print(coefficients)

###############################################################################
## Linear regression: Qerb (LRF) by age group
###############################################################################

# # Not currently used in tuning manuscript
# 
# Qmodel <- lm(Qerb ~ f2 * group, data=dfQ)
# summary(Qmodel)
# 
# # Check for normality
# resid_vals <- residuals(Qmodel)
# #hist(resid_vals)
# sm.density(Qmodel$residuals, model="normal")
# 
# # Check for heteroskedasticity
# fitted_vals <- fitted(Qmodel) 
# plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
# abline(h=0)

################################################################################
#### Figure 5: Slope and peak of DPOAE LRF 
################################################################################

dfslopepeak <- unique(filtered_df[, c("id","f2","group","peak","highRatiom")])

# Filter data (gets rid of one aberrant data point from CCG_B_035 at 4kHz, which is slightly below 0)
dfslopepeak <- dfslopepeak %>%
  filter(highRatiom > 0) 

# Pivot data into long format for plotting
df_long <- dfslopepeak %>%
  pivot_longer(cols = c(peak, highRatiom), 
               names_to = "type",         
               values_to = "value")       

# Reorder factors for plotting
df_long$type <- factor(df_long$type, levels = c("peak", "highRatiom"))

# Create labels for plotting
xlabs <- c("18-23", "30-39", "40+")
facet_labels <- c(peak = "DPOAE LRF Peak (dB SPL)", highRatiom = "High Ratio Slope (dB/Hz)")

## Figure 5: LRF Peak & LRF High Ratio Slope boxplots
p5 <- ggplot(data=df_long, aes(x=group, y=value, colour=group)) + 
  geom_boxplot() +
  facet_grid(type~f2, scales = "free", labeller = labeller(type = facet_labels)) +
  scale_colour_manual(name="",
                      labels= c("18-23 years", "30-39 years", "40+ years"),
                      aesthetics= c("colour","fill"),
                      values=my.pal) + 
  scale_x_discrete(labels= xlabs) +
  labs(x = 'Age Group (years)',
       y = '') + 
  theme_bw(base_size=15) +
  theme(legend.position = "top", legend.title = element_blank(), axis.text.x=element_text(angle=45, hjust=1))

#p5

# Save 
#ggsave("Fig5_LRFPeakandSlope.tiff", plot=p5, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

#########################
## Analysis: Peak Gain ##
#########################

dfpeak <- df_long %>% filter(type == 'peak') #%>% filter(f2 <= 8000)

# Change f2 to factor
dfpeak$f2 <- as.factor(dfpeak$f2)
# Linear regression
peakmodel <- lm(value ~ group + f2, data=dfpeak)
summary(peakmodel)

# Check for normality
resid_vals <- residuals(peakmodel)
#hist(resid_vals)
sm.density(peakmodel$residuals, model="normal")

# Check for heteroskedasticity
fitted_vals <- fitted(peakmodel) 
plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
abline(h=0)
# Violations of homogeneity of variance, but moving forward since OLS is robust

################################
## Analysis: High Ratio Slope ##
################################

dfslope <- df_long %>% filter(type == 'highRatiom') #%>% filter(f2 <= 8000)

# Change f2 to factor
dfslope$f2 <- as.factor(dfslope$f2)
# Linear regression
slopemodel <- lm(value ~ group + f2, data=dfslope)
summary(slopemodel)

# Check for normality
resid_vals <- residuals(slopemodel)
#hist(resid_vals)
sm.density(slopemodel$residuals, model="normal")

# Check for heteroskedasticity
fitted_vals <- fitted(peakmodel) 
plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
abline(h=0)
# Violations of homogeneity of variance, but moving forward since OLS is robust

################################################################################
## Figure 6: Qerb and peak values 
################################################################################

# Grab unique Q, slope, and peak values for plotting 
dfQMP <- unique(filtered_df[, c("id","f2","group","Qerb","highRatiom", "peak")])

plotlabs <- c('A' = "18-23 years",
              'B' = "30-39 years",
              'C' = "40+ years")

# Create formatting function (for plotting)
format_pval <- function(pval){
  pval <- scales::pvalue(pval, accuracy= 0.001, add_p = TRUE)
  gsub(pattern = "(=|<)", replacement = " \\1 ", x = pval)
}

# Examine peak LRF vs. high ratio slope
p10 <- ggscatter(dfQMP, x = "peak", y = "highRatiom", color = "group",
                 palette= c("#F79044FF","#B6308BFF","#8305A7FF"),
                 add = "reg.line", conf.int = F) +
  stat_cor(aes(label = paste(..r.label.., format_pval(..p..), sep = "*`,`~"))) +
  facet_wrap(~group, labeller = as_labeller(plotlabs)) + 
  labs(x = ('DPOAE LRF Peak (dB SPL)'),
       y = ('High Ratio Side Slope (dB/Hz)')) + 
  theme_bw(base_size = 15) +
  theme(legend.position = "none", legend.title = element_blank())

#p10

# Examine high ratio slope vs. Qerb
p11 <- 
  ggscatter(data=dfQMP, x="Qerb", y="highRatiom", color="group", 
            palette= c("#F79044FF","#B6308BFF","#8305A7FF"))+#, add = "reg.line") + 
  stat_cor() + #aes(label = paste(..r.label.., format_pval(..p..), sep = "*`,`~"))) +
  facet_wrap(~group, labeller=as_labeller(plotlabs)) + 
  labs(x = (bquote(italic(Q)[italic(erb)]~' LRF')),
       y =('High Ratio Side Slope (dB/Hz)')) + 
  theme_bw(base_size=15) +
  theme(legend.position="none")

#p11

## Figure 6: Combine into a single plot
combined_plot <- plot_grid(p10, p11, labels = c("A", "B"), ncol = 1)
#combined_plot

# Save 
#ggsave("Fig6_slopeVpeakVqerb.tiff", plot=combined_plot, width=11, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

################################################################################
## Prep data
################################################################################

## Save data in new frame to compare across LRF, SFOAE, and fPTC
dfLRF <- dfQ %>%
  mutate(type = 'LRF') %>%
  rename(frequency = f2)

# Get participant ids in consistent format
dfLRF$id <- gsub("_","",as.character(dfLRF$id)) 
