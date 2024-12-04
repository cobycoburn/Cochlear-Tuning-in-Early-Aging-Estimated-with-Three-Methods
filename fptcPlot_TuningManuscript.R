#################
### fPTC Data ###
#################

# For plotting fPTC data & associated (estimated) Qerbs for Tuning Manuscript.

# Author: Courtney Glavin
# Last Updated: October 9, 2024

################################################################################
## Set up R and data
################################################################################

## Install packages, as required
if (!require(pacman)) install.packages('pacman')             # for pkg loading
if (!require(dplyr)) install.packages('dplyr')               # for data wrangling
if (!require(ggplot2)) install.packages('ggplot2')           # for plotting
if (!require(plotrix)) install.packages('pacman')            # for pacman
if (!require(sm)) install.packages('sm')                     # sm density - normality check

## Open packages 
pacman::p_load(pacman, dplyr, ggplot2, plotrix, sm)

# Read .csv; update path as needed
#df <- read.csv('D:/Compiled fPTC Data/allData25-Jun-2024.csv')
df <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/allData25-Jun-2024.csv')

# Define color palette (for plotting)
my.pal <- c("#F79044FF","#B6308BFF","#8305A7FF")
# Define shapes (for plotting)
my.shapes <- c(15,16,17,NA)

################################################################################
## Prep data
################################################################################

# Change group variable (something was assigned incorrectly in matlab) 
df <- df %>%
  mutate(group = case_when(
    substr(id, 4, 4) == "A" ~ "A",
    substr(id, 4, 4) == "B" ~ "B",
    substr(id, 4, 4) == "C" ~ "C",
    TRUE ~ NA_character_
  ))

# Create ratio variable 
df$ratio <- (df$probeFreq)/(df$noiseFreq)

# Preliminary data cleaning
df <- df %>%
  filter(probeFreq > 1000) %>%       # one early pilot participant has data at additional frequencies
  filter(probeFreq != 11250) 

################################################################################
## Data processing and cleaning ##
################################################################################

# Now, look at raw data before additional data processing
#ggplot(df, aes(x=noiseFreq, y=noiseLevel, colour=group, group=id)) + 
#  geom_line() + 
#  facet_grid(group~probeFreq, scales="free") +
#  theme_bw()

# Based on various inspections, the first level of data cleaning will:
  # 1) Remove fPTCs where Q10 could not be estimated (=0)
  # 2) Remove fPTCs where Qerb could not be estimated (=0)
  # 3) Remove fPTCs where the low frequency slope is >=0 

df <- df %>%
  filter(Q10 != 0) %>%
  filter(Qerb != 0) %>%
  filter(sl_lfull < 0)  

# Based on visual inspection of each fPTC, applying additional cleaning criteria
  # 1) Remove B041 @ 12.5 kHz (curve is completely flat; derived slopes not representative)
  # 2) Remove A001 @ 16 kHz (very few points - 10 - curve does not follow typical trends)
  # 3) Remove A012 @ 14 kHz (very few points + artificially high Qerb)
  # 4) Remove C087 @ 8 kHz (curve is completely rising; slopes not representative. ftip artificially low outside of 2 SDs from mean)
  # 5) Remove C074 @ 4 kHz (odd shaped curve with ftip defined at artificially low f value [2.5 kHz] outside of 2 SDs from mean)
  # 6) Remove B044 @ 4 kHz (odd shape, artificially low f value outside of 2 SDs of mean)

rmdf <- df %>%
  filter((id == 'CCGB041' & probeFreq == 12500) | (id == 'CCGA001' & probeFreq == 16000) | (id == 'CCGA012' & probeFreq == 14000) | (id == 'CCGC087' & probeFreq == 8000) | (id == 'CCGC074' & probeFreq == 4000) | (id == 'CCGB044' & probeFreq == 4000) 
)

df <- anti_join(df, rmdf)

# Additionally, some participants had noise sweep parameters set incorrectly/differently - remove
rmdf2 <- df %>%
  filter((id == 'CCGB029' & probeFreq == 14000) | (id == 'CCGB029' & probeFreq == 10000) | (id == 'CCGB040' & probeFreq == 12500) | (id == 'CCGB036' & probeFreq == 12500) 
         | (id == 'CCGA001' & probeFreq == 10000) | (id == 'CCGA012' & probeFreq == 10000) | (id == 'CCGA014' & probeFreq == 14000) | (id == 'CCGB011' & probeFreq == 10000) 
         | (id == 'CCGB040' & probeFreq == 10000) | (id == 'CCGC071' & probeFreq == 10000)
  )

df <- anti_join(df, rmdf2) 

################################################################################
## Figure 9: fPTC Plotting 
################################################################################

# Create labels
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

# Create x ticks
label_function <- function(x) {
  return(x / 1000)
}

# # Plot individual example
# ex <- df %>% filter(id == 'CCGA001') %>%
#   filter(probeFreq==2000)
# 
# ggplot(data=ex, aes(x=noiseFreq,y=noiseLevel)) + 
#   geom_line(alpha=0.5) +
#   geom_smooth(se=F) + 
#   scale_x_log10(labels = label_function) + 
#   labs(x = 'Masker Frequency (kHz)',
#        y = 'Masker Level (dB SPL)') +
#   theme_bw(base_size=25) 

# fPTCs as a function of frequency

p12 <- ggplot(data=df, aes(x=noiseFreq, y=noiseLevel, colour=group, group=id)) + 
  geom_line(alpha = 0.2) +
  geom_smooth(data=df, aes(x=noiseFreq, y=noiseLevel, colour=group, group=group), se=F, size=1.2) + 
  facet_grid(group ~ probeFreq , scales="free_x", labeller=as_labeller(labs)) + 
  scale_colour_manual(name="",
                      labels= c("18-23 years", "30-39 years", "40+ years"),
                      aesthetics= c("colour","fill"),
                      values=my.pal) + 
  scale_x_log10(labels = label_function) + 
  labs(x = 'Masker Frequency (kHz)',
       y = 'Masker Level (dB SPL)') + 
  theme_bw(base_size=15) +
  theme(legend.position = "top", legend.title = element_blank()) 

#p12

# Save
#ggsave("Fig9_fPTCclean.tiff", plot=p12, width=18, height=9, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

################################################################################
## Not used in manuscript: ftip and probe vs. ftip Plotting
################################################################################
# 
# agelabs <- c('A' = "18-23 years",
#           'B' = "30-39 years",
#           'C' = "40+ years") 
# 
# p14 <- ggplot(df, aes(x=probeFreq, y=ftip, colour=group)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
#   facet_grid(~group, labeller = as_labeller(agelabs)) + 
#   scale_colour_manual(name="",
#                       labels= c("18-23 years", "30-39 years", "40+ years"),
#                       aesthetics= c("colour","fill"),
#                       values=my.pal) + 
#   labs(x = 'Probe Frequency (Hz)',
#        y = 'fPTC Tip Frequency (Hz)') + 
#   theme_bw(base_size=15) +
#   theme(legend.position = "none", legend.title = element_blank())#, axis.text.x=element_text(size=15))
# 
# p14
# 
# # Save 
# #ggsave("probeVtip.tiff", plot=p14, width=12, height=6, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')
# 
# # Probe vs. ftip plot
# 
# df <- df %>%
#   mutate(tipdiff = log2(ftip/probeFreq))
# 
# p15 <- ggplot(df, aes(x=group, y=tipdiff, colour=group)) + 
#   geom_boxplot() + 
#   facet_grid(~probeFreq, labeller = as_labeller(labs)) +
#   scale_colour_manual(name="",
#                       labels= c("18-23 years", "30-39 years", "40+ years"),
#                       aesthetics= c("colour","fill"),
#                       values=my.pal) + 
#   scale_x_discrete(labels = c("A" = "18-23",
#                               "B" = "30-39",
#                               "C" = "40+")) + 
#   labs(x = 'Age Group (years)',
#        y = 'Difference in fPTC tip to probe frequency (octaves)') + 
#   theme_bw(base_size=15) +
#   theme(legend.position = "none", legend.title = element_blank())#, axis.text.x=element_text(size=15))
# 
# p15

# Save 
#ggsave("probeVtip_boxplot.tiff", plot=p15, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

################################################################################
## Figure 10: Qerb (fPTC) Plot
################################################################################

# Pull data from other studies (used Grabit in Matlab to extract data)
# Wilson et al. (2020)
WilsonfPTCQ <- data.frame(
  frequency = c(1, 4),
  Qerb = c(7.46685329431035, 9.71255229745881),
  sdlow = c(6.90541022922701,	8.58966616729213),
  sdhigh = c(7.99313291064108, 10.8355116848104)
) 

# Oxenham and Shera (2003) - simultaneously masked at +10 dB SL
OxenhamfPTCQ <- data.frame(
  frequency = c(1, 2, 4, 6, 8),
  Qerb = c(7.3569, 8.3849, 10.0407, 9.8132, 10.9818),
  sdlow = c(6.5824, 7.3947, 8.1715, 8.8893, 9.74),
  sdhigh = c(8.0050, 9.2559, 11.9618, 10.5467, 11.9001)
)
  
Glasberg <- data.frame(
  frequency = c(1000.74465337443,
                1182.46939708793,
                1424.87807927537,
                1672.61212461580,
                2015.48592948901,
                2267.37358392505,
                2501.18163049221,
                2786.29531023697,
                3033.62943142995,
                3335.50000273499,
                3667.42291571087,
                3992.97305412143,
                4375.93703280852,
                4795.64891754632,
                5136.67476453173,
                5537.99794060123,
                6029.59545676352,
                6543.36974037997,
                7124.15862754435,
                7580.98614789115,
                7962.19290342263),
  Qerb = c(7.57174143889728,
           7.79906669981362,
           8.01739205498286,
           8.17325078048549,
           8.36751175778263,
           8.44117679488228,
           8.53229836918022,
           8.60703641504911,
           8.64585198512718,
           8.70313060504296,
           8.77885135599573,
           8.81844176825632,
           8.84024340719802,
           8.88037048464605,
           8.93815673683264,
           8.94116445691610,
           8.98148686044982,
           9.00329647956546,
           9.00672136495895,
           9.04641304204481,
           9.04839824024518)
)

# Charaziak et al. 2013 (fPTC, sim masked, converted from Q10 by equation Q10 = 0.56*Qerb)
CharaziakfPTCQ <- data.frame(
  frequency = c(1000, 4000),
  Qerb = c(6.2071, 8.8818) 
)

# Yasin & Plack (2005) - forward masked (averages)
YasinfPTCQ <- data.frame(
  frequency = c(12, 13, 14, 15, 16, 16.5, 17),
  Qerb = c(17.86, 15.49, 16.28, 13.14, 11.25, 8.77, 31.46), 
  sd = c(4.44, 4.19, 5.24, 4.39, 5.46, 3.08, 0.00)
)

# Pull unique values for plotting Qerb
dfQ <- unique(df[, c("id","probeFreq","group","Qerb")])

p16 <- ggplot(data=dfQ, aes(x=probeFreq, y=Qerb, colour=group)) + 
  geom_point(aes(shape=group), size=3, alpha=0.4) +
  geom_smooth(aes(group=group, color=group, linetype=group), method="lm", se=F, size=1.5) + 
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
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = 'Probe Frequency (Hz)',
       y = bquote(italic(Q)[italic(erb)]~' fPTC')) + 
  theme_bw(base_size=15) +
  theme(legend.position = "top", legend.title = element_blank())#, axis.text.x=element_text(size=15))

p16 <- p16 + 
  # Glasberg and Moore (1990) - predictions
  geom_line(data=Glasberg, aes(x=frequency, y=Qerb), colour="black", linetype = "dashed", alpha=0.7) +
  # Wilson et al. (2020)
  geom_point(data=WilsonfPTCQ, aes(x=frequency*1000, y=Qerb), colour="black", fill="black", shape=25, size=4, alpha=0.8) + 
  geom_errorbar(data=WilsonfPTCQ, aes(x=frequency*1000, ymin=sdlow, ymax=sdhigh), colour="black", alpha=0.8, width=0.01) + 
  # Oxenham and Shera (2003)
  geom_point(data=OxenhamfPTCQ, aes(x=frequency*1000, y=Qerb), colour="#008080", fill="#008080", shape=11, size=4, alpha=1) + 
  geom_errorbar(data=OxenhamfPTCQ, aes(x=frequency*1000, ymin=sdlow, ymax=sdhigh), colour="#008080", alpha=0.7, width=0.01) +
  # Charaziak et al. (2013) 
  geom_point(data=CharaziakfPTCQ, aes(x=frequency, y=Qerb), colour="#800000", shape=12, size=4, alpha=1) +
  # Yasin and Plack (2005) - forward masked, high frequencies
  geom_point(data=YasinfPTCQ, aes(x=frequency*1000, y=Qerb), colour="black", shape=8, size=4, alpha=1) + 
  geom_errorbar(data=YasinfPTCQ, aes(x=frequency*1000, ymin=(Qerb-sd), ymax=(Qerb+sd)), colour="black", alpha=1, width=0.01)

#p16

# Save 
#ggsave("Fig10_QerbPTC.tiff", plot=p16, width=14, height=7, units="in", dpi=800, path='D:/Analysis/Tuning Manuscript/Figures')

# # Get additional icons from this plot (additional legend create in illustrator)
# ggplot() + 
#   geom_line(data=Glasberg, aes(x=frequency, y=Qerb), colour="black", linetype = "dashed", alpha=1) +
#   #geom_point(data=YasinfPTCQ, aes(x=frequency*1000, y=Qerb), colour="black", shape=8, size=15, alpha=1) + 
#   #xlim(1000,20000) +
#   #ylim(10,20) + 
#   scale_x_log10(limits=c(1000,10000)) + 
#   scale_y_log10(limits=c(5,8)) +
#   theme_void() 
# 
# # Save
# ggsave("Glasberg_symbol.tiff", width=6, height=3, units="in", dpi=800, path='D:/Analysis/Tuning Manuscript/Figures')

###############################################################################
## Linear regression - effects of frequency & age group on SFOAE Qerb
###############################################################################

Qptcmodel <- lm(Qerb ~ probeFreq * group, data=dfQ)
summary(Qptcmodel)

# Check for normality
resid_vals <- residuals(Qptcmodel)
hist(resid_vals)
sm.density(resid_vals,model="normal")

#Check for heteroskedasticity
fitted_vals <- fitted(Qptcmodel) 
plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
abline(h=0)

# Three-way ANOVA
#dfQ$probeFreq <- as.factor(dfQ$probeFreq)
#dfQ$group <- as.factor(dfQ$group)
#model <- aov(Qerb ~ probeFreq * group, data=dfQ)
#summary(model) 

# Calculate effect size
#eta_squared <- eta_squared(model)
#print(eta_squared)
#cohens_d()
# Post-hoc tests
#tukeyresult <- TukeyHSD(model)
#print(tukeyresult)

###############################################################################

## Save data in new frame to compare across LRF, SFOAE, and fPTC

dfPTC <- dfQ %>%
  mutate(type = 'fPTC') %>%
  rename(frequency = probeFreq)

