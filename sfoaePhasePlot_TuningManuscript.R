###########################################
### SFOAE sweeps (phase-gradient delay) ###
###########################################

# For plotting SFOAE phase & associated (estimated) Qerbs
# Used specifically for Tuning Manuscript (updated since dissertation was published in 08/2024)

# Author: Courtney Glavin
# Last Updated: October 8, 2024

################################################################################
## Set up R environment and data
################################################################################

## Install packages, as required
if (!require(pacman)) install.packages('pacman')             # for pkg loading
if (!require(dplyr)) install.packages('dplyr')               # for data wrangling
if (!require(ggplot2)) install.packages('ggplot2')           # for plotting
if (!require(plotrix)) install.packages('plotrix')           # for plotting
if (!require(RColorBrewer)) install.packages('RColorBrewer') # for colors
if (!require(sm)) install.packages('sm')                     # sm density - normality check

## Open packages 
pacman::p_load(pacman, dplyr, ggplot2, plotrix, RColorBrewer, sm)

## Read in data, update path as needed
#df <- read.csv('D:/Compiled SFOAE Phase Data/sfPhase.csv')
df <- read.csv('D:/Analysis/Tuning Manuscript/Data (Final)/sfPhase.csv')

## Data clean-up
# Filter out CCGC081 (did not qualify for study)
df <- df %>%
  filter(id != 'CCG_C_081')

# Rename participant (participant is CCGA025, not B025 - labeling issue)
df <- df %>%
  mutate(id = ifelse(id == 'CCG_B_025', 'CCG_A_025', id))

# Also fix the same participant's age group, which is mislabeled (redefine for all participants)
df <- df %>%
  mutate(group = case_when(
    grepl("A", id) ~ "A",  # If "A" is found in participant_id
    grepl("B", id) ~ "B",  # If "B" is found in participant_id
    TRUE ~ "C"                         # Else assign "C"
  ))

# Remove CCG_A_025 because there were errors in measurement that resulted in multiple measures
df <- df %>%
  filter(id != 'CCG_A_025')

# Define color palette (for plotting)
my.pal <- c("#F79044FF","#B6308BFF","#8305A7FF", "black")
# Define shapes (for plotting)
my.shapes <- c(15,16,17,NA)

################################################################################
## Figure 6: Plot phase in cycles across center frequencies by age group
################################################################################

 p7 <- ggplot(data=df, aes(x=(f/1000), y=phi, colour=group, group=id)) +
    geom_line(alpha=0.3, scales="free_x") +
    geom_smooth(aes(group=group, color=group, linetype=group),method="lm", se=F, linewidth=1.2) +
    facet_wrap(~cf, scales="free_x", nrow=1) +
    scale_colour_manual(name="",
                        labels= c("18-23 years", "30-39 years", "40+ years"),
                        aesthetics= c("colour","fill"),
                        values=my.pal) +
    scale_linetype_manual(name="", 
                        labels=c("18-23 years", "30-39 years", "40+ years"),
                        values=c("solid", "dashed", "dotted")) + 
    labs(x = bquote(italic(f)[italic(p)]~' Frequency (kHz)'),
         y = 'SFOAE Phase (cycles)') +
    theme_bw(base_size=15) +
    theme(legend.position = "top", legend.title = element_blank(), axis.text.x=element_text(size=10))

# p7

# Save
#ggsave("Fig7_SFphase.tiff", plot=p7, width=12, height=6, units="in", dpi=800, path='D:/Analysis/Tuning Manuscript/Figures')

####################################
## Analysis: Slopes from Figure 7 ##
####################################

# Not used in manuscript

models <- df %>%
  group_by(group) %>%
  do(model = lm(phi ~ f, data = .))

coefficients <- models %>%
  summarise(intercept = coef(model)[1], slope = coef(model)[2])

print(coefficients)


################################################################################
## Figure 8: Plot estimated Qerb across center frequencies by age group
################################################################################

# Prep OAE Qerb estimations from Shera et al. 2002 (pulled in Matlab using Grabit; saving values here)

# Lower bound of CI 
SheraLower <- data.frame(
  Frequency = c(1010.21088971841, 1108.55558921698, 1232.72595200841, 1334.89878081531, 1439.14724261356, 
                1544.73446913607, 1717.78355155299, 1893.36016073777, 2041.24750750333, 2239.96430492726, 
                2479.91237363956, 2479.91237363956, 2745.52899213601, 2946.92513096990, 3121.43342839452, 
                3380.01989459308, 3660.21497187057, 3894.08469510981, 4292.10351769524, 4709.94263917030, 
                5168.39275432001, 5523.05302423108, 6196.30153768587, 6860.05791266752, 7594.72304773126, 
                8483.03868590580, 9474.77262550640, 10351.6953008787, 11562.1848357493, 12857.2761236212, 
                14552.4291039346, 15482.6526528673, 13739.3800027535, 16691.7650940610, 18075.0090006269, 
                19400.6397440613, 20824.0241502446),
  Qerb = c(11.8513182256925, 12.1269401110995, 12.4497942296487, 12.6975630778879, 12.9074704150764, 
           13.2084971802395, 13.6052752956176, 13.9215107699837, 14.1987499979666, 14.5289652677226, 
           14.9656006109880, 14.9656006109880, 15.3642240231041, 15.6703939615516, 15.9832766623050, 
           16.1396854370792, 16.5156722955578, 16.7893403917546, 17.1795849776021, 17.5791244644223, 
           17.9282883544825, 18.2857868068512, 18.8346042616373, 19.4006358919363, 19.8513231671076, 
           20.5154398067639, 20.9218585803062, 21.6231684166400, 22.1985563812688, 22.7895459703886, 
           23.3950757884958, 23.9413041825629, 23.1668779590904, 24.3370852768526, 24.7390936091711, 
           25.1483839646900, 25.7348921208604)
)

# Upper bound of CI
SheraUpper <- data.frame(
  Frequency = c(997.872233644659, 1164.99673370086, 1330.35202775035, 1532.70386212711, 1734.82571730929, 
                1998.75059474318, 2302.70976800523, 2606.44089828711, 2924.19784282255, 3413.90146938749, 
                3830.29312052398, 4511.55283983928, 5314.04986464543, 6149.37710229384, 7147.80358296116, 
                8344.81743930521, 9403.97992103883, 1239.43429154354, 1085.37956207485, 1427.95717665343, 
                1652.42091979404, 2174.02887070583, 1895.34143287850, 2471.65477285992, 2785.26194910706, 
                3180.67278451881, 3648.30639236929, 4203.17469383029, 4972.81063499338, 5780.05973156145, 
                6659.14486879189, 7706.10713714262, 8957.05926082498),
  Qerb = c(15.2563334505918, 15.9226411706908, 16.5090380814283, 17.2307159428457, 17.8064800442287, 
           18.7087857628482, 19.3972956166615, 20.1791054508903, 20.8539219050105, 21.6925043448707, 
           22.7178625667114, 23.7883494982454, 24.9921799124427, 25.9978340628529, 27.3145225376369, 
           28.4129000542289, 29.6568358731746, 16.1864826188394, 15.6115427975986, 16.8940602561685, 
           17.5738561712674, 19.1443776824687, 18.2814725692657, 19.8496774517248, 20.5132157486515, 
           21.4104788231773, 22.3467038143648, 23.2462038021597, 24.5035673230917, 25.5740665063433, 
           26.6034743644962, 27.8584762267282, 29.0755429360005)
)

# Data are in long format, so take only one row from each participant at each cf
# so as to not get duplicate Qerbs. Additionally, filter out Qerbs == 0 (those 
# were set in Matlab when Q could not be estimated due to noisy data)

df2 <- df %>% 
  distinct(id, cf, .keep_all = T) %>% 
  filter(Qerb_S > 0)

## Plot Qerb data (estimated using Shera Method)
p9 <- ggplot(data=df2, aes(x=cf, y=Qerb_S, colour=group)) + 
  geom_point(aes(shape=group), size=3, alpha=0.7) +
  geom_smooth(aes(group=group, color=group, linetype=group), method="lm", formula=y~x, se=F, size=1.5) + 
  scale_x_log10() + 
  scale_y_log10(limits=c(0.1,100)) + 
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
  labs(x = bquote(italic(f)[italic(p)]~' Frequency (Hz)'),
       y = bquote(italic(Q)[italic(erb)]~' SFOAE')) + 
  theme_bw(base_size=15) + 
  theme(legend.position = "top")

#p9

# Add Shera (2002) OAE tuning estimates to figure
p9 <- p9 + 
  geom_line(data=SheraLower, aes(x=Frequency, y=Qerb), colour="black", alpha=0.5, linetype="dashed") + 
  geom_line(data=SheraUpper, aes(x=Frequency, y=Qerb), colour="black", alpha=0.5, linetype="dashed") + 
  annotate(geom="text", x=1250, y=21, label="Shera et al. (2002)", color="black", size=5) 


#p9 

# Save 
#ggsave("Fig8_QerbSF-s.tiff", plot=p9, width=14, height=7, units="in", dpi=800, path = 'D:/Analysis/Tuning Manuscript/Figures')

###############################################################################
## Linear regression - Qerb (SFOAE) by age group 
###############################################################################

# Not currently used in manuscript
# 
# #df2$cf <- as.factor(df2$cf)
# 
# Qsfmodel <- lm(Qerb_S ~ cf * group, data=df2)
# summary(Qsfmodel)
# 
# # Check for normality
# resid_vals <- residuals(Qsfmodel)
# hist(resid_vals)
# sm.density(resid_vals,model="normal")
# 
# #Check for heteroskedasticity
# fitted_vals <- fitted(Qsfmodel) 
# plot(fitted_vals, resid_vals, xlab="Fitted values", ylab="Residuals")
# abline(h=0)

################################################################################
## Prep data
################################################################################

# Save data in new frame to compare across LRF, SFOAE, and fPTC
dfQ <- df2 %>%
  dplyr::select(id, cf, group, Qerb_S)

dfSF <- dfQ %>%
  mutate(type = 'SFOAE') %>%
  rename(frequency = cf,
         Qerb = Qerb_S)

# Get participant ids in consistent format
dfSF$id <- gsub("_","",as.character(dfSF$id)) 
