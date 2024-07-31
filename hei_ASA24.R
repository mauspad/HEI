library(rstudioapi)
library(stringr)
library(dplyr)

# Set working directory as source
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
prefix <- basename(getwd())
prefix <- str_sub(prefix, 1, -2)
prefix

# Read CSV file using base R
Totals <- read.csv("Totals.csv")

# Perform column-wise calculations
Totals <- transform(Totals,
                    FWHOLEFRT = F_CITMLB + F_OTHER,
                    MONOPOLY = MFAT + PFAT,
                    VTOTALLEG = V_TOTAL + V_LEGUMES,
                    VDRKGRLEG = V_DRKGR + V_LEGUMES,
                    PFALLPROTLEG = PF_MPS_TOTAL + PF_EGGS + PF_NUTSDS + PF_SOY + PF_LEGUMES,
                    PFSEAPLANTLEG = PF_SEAFD_HI + PF_SEAFD_LOW + PF_NUTSDS + PF_SOY + PF_LEGUMES
)

# Sort data by UserName and UserID using dplyr
Totals <- arrange(Totals, UserName, UserID)

# Calculate sum per person of variables of interest using dplyr
idtot <- Totals %>%
  group_by(UserName, UserID) %>%
  summarize(across(c(KCAL, VTOTALLEG, VDRKGRLEG, F_TOTAL, FWHOLEFRT, G_WHOLE, D_TOTAL,
                     PFALLPROTLEG, PFSEAPLANTLEG, MONOPOLY, SFAT, SODI, G_REFINED, ADD_SUGARS), sum, na.rm = TRUE))
indat = Totals
kcal = Totals$KCAL
vtotalleg = Totals$VTOTALLEG
vdrkgrleg = Totals$VDRKGRLEG
f_total = Totals$F_TOTAL
fwholefrt = Totals$FWHOLEFRT
g_whole = Totals$G_WHOLE
d_total = Totals$D_TOTAL
pfallprotleg = Totals$PFALLPROTLEG
pfseaplantleg = Totals$PFSEAPLANTLEG
monopoly = Totals$MONOPOLY
satfat = Totals$SFAT
sodium = Totals$SODI
g_refined = Totals$G_REFINED
add_sugars = Totals$ADD_SUGARS

# Perform calculations without subsetting
Totals <- Totals %>%
  mutate(
                    VEGDEN = ifelse(kcal > 0, vtotalleg / (kcal / 1000), 0),
                    HEI2015C1_TOTALVEG = 5 * (VEGDEN / 1.1),
                    HEI2015C1_TOTALVEG = ifelse(HEI2015C1_TOTALVEG > 5, 5, HEI2015C1_TOTALVEG),
                    HEI2015C1_TOTALVEG = ifelse(VEGDEN == 0, 0, HEI2015C1_TOTALVEG),
                    GRBNDEN = ifelse(kcal > 0, vdrkgrleg / (kcal / 1000), 0),
                    HEI2015C2_GREEN_AND_BEAN = 5 * (GRBNDEN / 0.2),
                    HEI2015C2_GREEN_AND_BEAN = ifelse(HEI2015C2_GREEN_AND_BEAN > 5, 5, HEI2015C2_GREEN_AND_BEAN),
                    HEI2015C2_GREEN_AND_BEAN = ifelse(GRBNDEN == 0, 0, HEI2015C2_GREEN_AND_BEAN),
                    FRTDEN = ifelse(kcal > 0, f_total / (kcal / 1000), 0),
                    HEI2015C3_TOTALFRUIT = 5 * (FRTDEN / 0.8),
                    HEI2015C3_TOTALFRUIT = ifelse(HEI2015C3_TOTALFRUIT > 5, 5, HEI2015C3_TOTALFRUIT),
                    HEI2015C3_TOTALFRUIT = ifelse(FRTDEN == 0, 0, HEI2015C3_TOTALFRUIT),
                    WHFRDEN = ifelse(kcal > 0, fwholefrt / (kcal / 1000), 0),
                    HEI2015C4_WHOLEFRUIT = 5 * (WHFRDEN / 0.4),
                    HEI2015C4_WHOLEFRUIT = ifelse(HEI2015C4_WHOLEFRUIT > 5, 5, HEI2015C4_WHOLEFRUIT),
                    HEI2015C4_WHOLEFRUIT = ifelse(WHFRDEN == 0, 0, HEI2015C4_WHOLEFRUIT),
                    WGRNDEN = ifelse(kcal > 0, g_whole / (kcal / 1000), 0),
                    HEI2015C5_WHOLEGRAIN = 10 * (WGRNDEN / 1.5),
                    HEI2015C5_WHOLEGRAIN = ifelse(HEI2015C5_WHOLEGRAIN > 10, 10, HEI2015C5_WHOLEGRAIN),
                    HEI2015C5_WHOLEGRAIN = ifelse(WGRNDEN == 0, 0, HEI2015C5_WHOLEGRAIN),
                    DAIRYDEN = ifelse(kcal > 0, d_total / (kcal / 1000), 0),
                    HEI2015C6_TOTALDAIRY = 10 * (DAIRYDEN / 1.3),
                    HEI2015C6_TOTALDAIRY = ifelse(HEI2015C6_TOTALDAIRY > 10, 10, HEI2015C6_TOTALDAIRY),
                    HEI2015C6_TOTALDAIRY = ifelse(DAIRYDEN == 0, 0, HEI2015C6_TOTALDAIRY),
                    PROTDEN = ifelse(kcal > 0, pfallprotleg / (kcal / 1000), 0),
                    HEI2015C7_TOTPROT = 5 * (PROTDEN / 2.5),
                    HEI2015C7_TOTPROT = ifelse(HEI2015C7_TOTPROT > 5, 5, HEI2015C7_TOTPROT),
                    HEI2015C7_TOTPROT = ifelse(PROTDEN == 0, 0, HEI2015C7_TOTPROT),
                    SEAPLDEN = ifelse(kcal > 0, pfseaplantleg / (kcal / 1000), 0),
                    HEI2015C8_SEAPLANT_PROT = 5 * (SEAPLDEN / 0.8),
                    HEI2015C8_SEAPLANT_PROT = ifelse(HEI2015C8_SEAPLANT_PROT > 5, 5, HEI2015C8_SEAPLANT_PROT),
                    HEI2015C8_SEAPLANT_PROT = ifelse(SEAPLDEN == 0, 0, HEI2015C8_SEAPLANT_PROT),
                    FARATIO = ifelse(satfat > 0, monopoly / satfat, 0),
                    HEI2015C9_FATTYACID = ifelse(satfat == 0 & monopoly == 0, 0,
                                                 ifelse(satfat == 0 & monopoly > 0, 10,
                                                        ifelse(FARATIO >= 2.5, 10,
                                                               ifelse(FARATIO <= 1.2, 0,
                                                                      10 * ((FARATIO - 1.2) / (2.5 - 1.2)))))),
                    HEI2015C9_FATTYACID = ifelse(is.na(HEI2015C9_FATTYACID), 0, HEI2015C9_FATTYACID),
                    SODDEN = ifelse(kcal > 0, sodium / kcal, 0),
                    HEI2015C10_SODIUM = ifelse(SODDEN <= 1.1, 10,
                                               ifelse(SODDEN >= 2.0, 0,
                                                      10 - (10 * (SODDEN - 1.1) / (2.0 - 1.1)))),
                    HEI2015C10_SODIUM = ifelse(is.na(HEI2015C10_SODIUM), 0, HEI2015C10_SODIUM),
                    RGDEN = ifelse(kcal > 0, g_refined / (kcal / 1000), 0),
                    HEI2015C11_REFINEDGRAIN = ifelse(RGDEN <= 1.8, 10,
                                                     ifelse(RGDEN >= 4.3, 0,
                                                            10 - (10 * (RGDEN - 1.8) / (4.3 - 1.8)))),
                    HEI2015C11_REFINEDGRAIN = ifelse(is.na(HEI2015C11_REFINEDGRAIN), 0, HEI2015C11_REFINEDGRAIN),
                    SFAT_PERC = ifelse(kcal > 0, 100 * (satfat * 9 / kcal), 0),
                    HEI2015C12_SFAT = ifelse(SFAT_PERC >= 16, 0,
                                             ifelse(SFAT_PERC <= 8, 10,
                                                    10 - (10 * (SFAT_PERC - 8) / (16 - 8)))),
                    HEI2015C12_SFAT = ifelse(is.na(HEI2015C12_SFAT), 0, HEI2015C12_SFAT),
                    ADDSUG_PERC = ifelse(kcal > 0, 100 * (add_sugars * 16 / kcal), 0),
                    HEI2015C13_ADDSUG = ifelse(ADDSUG_PERC >= 26, 0,
                                               ifelse(ADDSUG_PERC <= 6.5, 10,
                                                      10 - (10 * (ADDSUG_PERC - 6.5) / (26 - 6.5)))),
                    HEI2015C13_ADDSUG = ifelse(is.na(HEI2015C13_ADDSUG), 0, HEI2015C13_ADDSUG)
  )

# Calculate HEI2015_TOTAL_SCORE
Totals$HEI2015_TOTAL_SCORE <- rowSums(select(Totals, starts_with("HEI2015C")), na.rm = TRUE)

# Write the output to a file
write.csv(Totals, file = "output_data.csv")