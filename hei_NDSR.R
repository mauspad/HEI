# Load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(rstudioapi)
library(stringr)

# Set working directory as source
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
prefix <- basename(getwd())
prefix <- str_sub(prefix,1,-2)
prefix

# Read the data from the input files
ndsdata_record <- fread(paste0(prefix, "04.txt"), sep='\t', header=FALSE, fill=TRUE)
ndsdata_fgscrecord <- fread(paste0(prefix, "09.txt"), sep='\t', header=FALSE, fill=TRUE)

# Rename columns to match SAS labels
colnames(ndsdata_record) <- c(
  "cproject", "cpartid", "dintake", "cpjname", "irectype", "cpname", "igender",
  "dpdob", "dentry", "idow", "cvisit", "cintvw", "csite", "irdacat", "iamount",
  "ireliabl", "cdbvers", "cswvers", "rigrams", "rikcal", "rifat", "ritcho", "ripro",
  "riapro", "rivpro", "rialc", "richol", "risfa", "rimfa", "ripfa", "rifruc",
  "rigala", "rigluc", "rilact", "rimalt", "risucr", "ristar", "ridfib", "riwsdf",
  "riifib", "ripect", "riva", "ribceq", "rirl", "rivd", "rittc", "riatc", "ribtc",
  "rigtc", "ridtc", "rivk", "rivc", "rithi", "ririb", "rinia", "ripant", "rivb6",
  "rifol", "rivb12", "rica", "rip", "rimg", "rife", "rizn", "ricu", "rise", "rina",
  "rik", "ris04_0", "ris06_0", "ris08_0", "ris10_0", "ris12_0", "ris14_0", "ris16_0",
  "ris17_0", "ris18_0", "ris20_0", "ris22_0", "rim14_1", "rim16_1", "rim18_1",
  "rim20_1", "rim22_1", "rip18_2", "rip18_3", "rip18_4", "rip20_4", "rip20_5",
  "rip22_5", "rip22_6", "ritryp", "rithre", "riisol", "rileuc", "rilysi", "rimeth",
  "ricyst", "riphen", "rityro", "rivali", "riargi", "rihist", "rialan", "riaspa",
  "riglut", "riglyc", "riprol", "riseri", "riaspt", "risacc", "ricaf", "riphyt",
  "rioxal", "rimh3", "risp", "riash", "riw", "ripctfat", "ripctcho", "ripctpro",
  "ripctalc", "ripctsfa", "ripctmfa", "ripctpfa", "rips", "ricsi", "rivare",
  "rif181t", "rif182t", "rif161t", "rittfa", "riunut1", "riunut2", "riunut3",
  "riunut4", "riunut5", "riunut6", "riunut7", "riunut8", "riunut9", "riunut10",
  "cnote", "ribcar", "riacar", "ribcry", "rilz", "rilyco", "ridfe", "rinfol",
  "risfol", "cgdbvers", "cgswvers", "ctnote", "riunut11", "riunut12", "riunut13",
  "riunut14", "riunut15", "riunut16", "riunut17", "riunut18", "riunut19", "riunut20",
  "rivarae", "rikj", "riniaeq", "ritsugar", "riomega3", "rimn", "rivite", "rinatatoc",
  "risynatoc", "ridaid", "rigeni", "riglyt", "ricoum", "ribioa", "riformon",
  "cblank1", "rif", "rifnond", "riftotal", "riasugar", "riacesk", "risucl",
  "riacho", "rigig", "rigib", "riglg", "riglb", "richoline", "ribetaine", "rieryth",
  "riinos", "riisom", "rilactl", "rimaltitol", "rimani", "ripini", "risorb", "rixyli",
  "rinitrogen", "ricla_total", "ricla_c9t11", "ricla_t10c12", "ritag", "rivd2",
  "rivd3", "rias_ts", "ritotalgrains", "riwholegrains", "rirefinedgrains", "rip18_3n3",
  "risof", "rigluten", "chdrfield1dsc", "chdrfield1rsp", "chdrfield2dsc",
  "chdrfield2rsp", "chdrfield3dsc", "chdrfield3rsp", "chdrfield4dsc", "chdrfield4rsp",
  "chdrfield5dsc", "chdrfield5rsp", "ctlrfield1dsc", "ctlrfield1rsp", "ctlrfield2dsc",
  "ctlrfield2rsp", "ctlrfield3dsc", "ctlrfield3rsp", "ritotallignans", "riseco", "rimata", 
  "rilari", "ripino", "rip18_2n6", "rip18_3n6", "rip20_4n6", "riomega6"
)

colnames(ndsdata_fgscrecord) <- c(
  "cproject", "cpartid", "dintake", "FRU0100", "FRU0200", "FRU0300", "FRU0400", "FRU0500", 
  "FRU0600", "FRU0700", "VEG0100", "VEG0200", "VEG0300", "VEG0400", "VEG0800", "VEG0450", 
  "VEG0700", "VEG0600", "VEG0900", "VEG0500", "FMC0100", "GRW0100", "GRS0100", "GRR0100", 
  "GRW0200", "GRS0200", "GRR0200", "GRW0300", "GRS0300", "GRR0300", "GRW0400", "GRS0400", 
  "GRR0400", "GRW0500", "GRS0500", "GRR0500", "GRW0600", "GRS0600", "GRR0600", "GRW0700", 
  "GRS0700", "GRR0700", "GRW0800", "GRS0800", "GRR0800", "GRW1000", "GRS1000", "GRR1000", 
  "GRW0900", "GRS0900", "GRR0900", "GRW1100", "GRW1200", "GRR1300", "MRF0100", "MRL0100", 
  "MRF0200", "MRL0200", "MRF0300", "MRL0300", "MRF0400", "MRL0400", "MCF0200", "MCL0200", 
  "MRF0500", "MPF0100", "MPL0100", "MPF0200", "MFF0100", "MFL0100", "MFF0200", "MSL0100", 
  "MSF0100", "MCF0100", "MCL0100", "MOF0100", "MOF0200", "FMC0200", "MOF0300", "MOF0400", 
  "MOF0500", "MOF0600", "MOF0700", "DMF0100", "DMR0100", "DML0100", "DMN0100", "DMF0200", 
  "DMR0200", "DML0200", "DML0300", "DML0400", "SWT0600", "MSC1100", "DCF0100", "DCR0100", 
  "DCL0100", "DCN0100", "DYF0100", "DYR0100", "DYL0100", "DYF0200", "DYR0200", "DYL0200", 
  "DYN0100", "DOT0100", "DOT0200", "DOT0300", "DOT0400", "FCF0100", "FCR0100", "FCL0100", 
  "FCN0100", "DOT0500", "DOT0600", "DOT0700", "DOT0800", "FMF0100", "FMR0100", "FOF0100", 
  "FSF0100", "FAF0100", "FAR0100", "FDF0100", "FDR0100", "SWT0400", "MSC1200", "SWT0500", 
  "SWT0700", "SWT0800", "SWT0100", "SWT0200", "SWT0300", "BVS0400", "BVA0400", "BVU0300", 
  "BVS0300", "BVA0300", "BVS0500", "BVA0500", "BVU0400", "BVS0100", "BVA0100", "BVU0100", 
  "BVS0200", "BVA0200", "BVU0200", "BVS0600", "BVA0600", "BVU0500", "BVS0700", "BVA0700", 
  "BVU0600", "BVO0100", "BVO0200", "BVE0100", "BVE0400", "BVE0300", "BVE0200", "MSC0100", 
  "MSC0200", "MSC0300", "MSC0400", "MSC0500", "MSC0600", "MSC0700", "MSC0800", "MSC0900", 
  "MSC1000", "GRW1300", "GRS1300", "DML0500", "MSC1300", "DYF0300", "DYR0300", "DYL0300", 
  "DOT0900"
)

# Sort each dataset by cpartid and dintake
ndsdata_fgscrecord <- ndsdata_fgscrecord %>%
  arrange(cpartid, dintake)

ndsdata_record <- ndsdata_record %>%
  arrange(cpartid, dintake)

# Merge the datasets
record0409 <- merge(ndsdata_record, ndsdata_fgscrecord, by = c("cpartid", "dintake"))

# Calculate HEI components
hei0409 <- record0409 %>%
  mutate(
    hei_totveg = (VEG0100 + VEG0200 + VEG0300 + VEG0400 + VEG0800 + VEG0450 + VEG0700 +
                    VEG0600 + VEG0900 + VEG0500) / 2,
    hei_greensbeans = (VEG0100 + VEG0700) / 2,
    hei_totfruit = (FRU0100 + FRU0200 + FRU0300 + FRU0400 + FRU0500 + FRU0600 + FRU0700) / 2,
    hei_wholefruit = (FRU0300 + FRU0400 + FRU0500 + FRU0600 + FRU0700) / 2,
    hei_wholegrains = riwholegrains,
    hei_dairy = (DMF0100 + DMR0100 + DML0100 + DMN0100 + DMF0200 + DMR0200 +
                   DML0200 + DML0300 + DML0400 + DCF0100 + DCR0100 + DCL0100 + DCN0100 +
                   DYF0100 + DYR0100 + DYL0100 + DYF0200 + DYR0200 + DYL0200 + DYN0100 +
                   (DOT0100 / 3) + DOT0300 + DOT0400 + DOT0500 + DOT0600) +
      DML0500 + DYF0300 + DYR0300 + DYL0300 + DOT0900,
    hei_totproteins = (MRF0100 + MRL0100 + MRF0200 + MRL0200 +
                         MRF0300 + MRL0300 + MRF0400 + MRL0400 + MCF0200 + MCL0200 + MRF0500 +
                         MPF0100 + MPL0100 + MPF0200 + MFF0100 + MFL0100 + MFF0200 + MSL0100 +
                         MSF0100 + MCF0100 + MCL0100 + MOF0100 + MOF0200 + MOF0300 + MOF0400 +
                         MOF0500 + MOF0600 + MOF0700 + (VEG0700 * 2)),
    hei_seafoodplantprot = (MFF0100 +
                              MFL0100 + MFF0200 + MSL0100 + MSF0100 + MOF0500 + MOF0600 + MOF0700 +
                              (VEG0700 * 2)),
    hei_sodium = rina / 1000,
    hei_refinedgrains = rirefinedgrains,
    hei_addedsugars = rias_ts * 4
  )

hei0409togroup <- hei0409

hei0409togroup$ripctsfa <- hei0409togroup$ripctsfa * hei0409togroup$rikcal

dailyhei0409 <- hei0409togroup %>%
  group_by(cpartid) %>%
  summarize_at(
    vars(rikcal, hei_totveg, hei_greensbeans, hei_totfruit, hei_wholefruit, hei_wholegrains, hei_dairy,
         hei_totproteins, hei_seafoodplantprot, ripfa, rimfa, risfa, hei_sodium, hei_refinedgrains,
         hei_addedsugars, ripctsfa),
    funs(sum)
  )

hei0409 <- dailyhei0409

hei0409$ripctsfa <- hei0409$ripctsfa / hei0409$rikcal

energy <- hei0409$rikcal / 1000

hei0409$HEIX1_TOTALVEG <- 0
hei0409$HEIX2_GREEN_AND_BEAN <- 0
hei0409$HEIX3_TOTALFRUIT <- 0
hei0409$HEIX4_WHOLEFRUIT <- 0
hei0409$HEIX5_WHOLEGRAIN <- 0
hei0409$HEIX6_TOTALDAIRY <- 0
hei0409$HEIX7_TOTPROT <- 0
hei0409$HEIX8_SEAPLANT_PROT <- 0
hei0409$HEIX9_FATTYACID <- 0
hei0409$HEIX10_SODIUM <- 0
hei0409$HEIX11_REFINEDGRAIN <- 0
hei0409$HEIX12_ADDEDSUGARS <- 0
hei0409$HEIX13_SATFATS <- 0

if (hei0409$rikcal != 0) {

  hei0409$xhei_totveg <- hei0409$hei_totveg / energy
  hei0409$HEIX1_TOTALVEG <- ifelse(hei0409$xhei_totveg == 0, 0,
                                   ifelse(hei0409$xhei_totveg >= 1.1, 5, 5 * (hei0409$xhei_totveg / 1.1)))
  
  hei0409$xhei_greensbeans <- hei0409$hei_greensbeans / energy
  hei0409$HEIX2_GREEN_AND_BEAN <- ifelse(hei0409$xhei_greensbeans == 0, 0,
                                         ifelse(hei0409$xhei_greensbeans >= 0.2, 5, 5 * (hei0409$xhei_greensbeans / 0.2)))
  
  hei0409$xhei_totfruit <- hei0409$hei_totfruit / energy
  hei0409$HEIX3_TOTALFRUIT <- ifelse(hei0409$xhei_totfruit == 0, 0,
                                     ifelse(hei0409$xhei_totfruit >= 0.8, 5, 5 * (hei0409$xhei_totfruit / 0.8)))
  
  hei0409$xhei_wholefruit <- hei0409$hei_wholefruit / energy
  hei0409$HEIX4_WHOLEFRUIT <- ifelse(hei0409$xhei_wholefruit == 0, 0,
                                     ifelse(hei0409$xhei_wholefruit >= 0.4, 5, 5 * (hei0409$xhei_wholefruit / 0.4)))
  
  hei0409$xhei_wholegrains <- hei0409$hei_wholegrains / energy
  hei0409$HEIX5_WHOLEGRAIN <- ifelse(hei0409$xhei_wholegrains == 0, 0,
                                     ifelse(hei0409$xhei_wholegrains >= 1.5, 10, 10 * (hei0409$xhei_wholegrains / 1.5)))
  
  hei0409$xhei_dairy <- hei0409$hei_dairy / energy
  hei0409$HEIX6_TOTALDAIRY <- ifelse(hei0409$xhei_dairy == 0, 0,
                                     ifelse(hei0409$xhei_dairy >= 1.3, 10, 10 * (hei0409$xhei_dairy / 1.3)))
  
  hei0409$xhei_totproteins <- hei0409$hei_totproteins / energy
  hei0409$HEIX7_TOTPROT <- ifelse(hei0409$xhei_totproteins == 0, 0,
                                  ifelse(hei0409$xhei_totproteins >= 2.5, 5, 5 * (hei0409$xhei_totproteins / 2.5)))
  
  hei0409$xhei_seafoodplantprot <- hei0409$hei_seafoodplantprot / energy
  hei0409$HEIX8_SEAPLANT_PROT <- ifelse(hei0409$xhei_seafoodplantprot == 0, 0,
                                        ifelse(hei0409$xhei_seafoodplantprot >= 0.8, 5, 5 * (hei0409$xhei_seafoodplantprot / 0.8)))
  
  hei0409$xhei_fatacid <- (hei0409$ripfa + hei0409$rimfa) / hei0409$risfa
  FARMIN <- 1.2
  FARMAX <- 2.5
  hei0409$HEIX9_FATTYACID <- ifelse(hei0409$xhei_fatacid <= FARMIN, 0,
                                    ifelse(hei0409$xhei_fatacid > FARMAX, 10, 10 * ((hei0409$xhei_fatacid - FARMIN) / (FARMAX - FARMIN))))
  
  hei0409$xhei_sodium <- hei0409$hei_sodium / energy
  SODMIN <- 1.1
  SODMAX <- 2.0
  hei0409$HEIX10_SODIUM <- ifelse(hei0409$xhei_sodium >= SODMAX, 0,
                                  ifelse(hei0409$xhei_sodium <= SODMIN, 10, 10 - (10 * ((hei0409$xhei_sodium - SODMIN) / (SODMAX - SODMIN)))))
  
  hei0409$xhei_refinedgrains <- hei0409$hei_refinedgrains / energy
  RGMIN <- 1.8
  RGMAX <- 4.3
  hei0409$HEIX11_REFINEDGRAIN <- ifelse(hei0409$xhei_refinedgrains >= RGMAX, 0,
                                        ifelse(hei0409$xhei_refinedgrains <= RGMIN, 10, 10 - (10 * ((hei0409$xhei_refinedgrains - RGMIN) / (RGMAX - RGMIN)))))
  
  hei0409$xhei_addedsugars <- 100 * hei0409$hei_addedsugars / hei0409$rikcal
  ADDSUGMIN <- 6.5
  ADDSUGMAX <- 26
  hei0409$HEIX12_ADDEDSUGARS <- ifelse(hei0409$xhei_addedsugars >= ADDSUGMAX, 0,
                                       ifelse(hei0409$xhei_addedsugars < ADDSUGMIN, 10, 10 - (10 * ((hei0409$xhei_addedsugars - ADDSUGMIN) / (ADDSUGMAX - ADDSUGMIN)))))
  
  hei0409$xhei_satfats <- hei0409$ripctsfa
  SATFATSMIN <- 8
  SATFATSMAX <- 16
  hei0409$HEIX13_SATFATS <- ifelse(hei0409$xhei_satfats > SATFATSMAX, 0,
                                   ifelse(hei0409$xhei_satfats < SATFATSMIN, 10, 10 - (10 * ((hei0409$xhei_satfats - SATFATSMIN) / (SATFATSMAX - SATFATSMIN)))))
}

hei0409$HEI2015_TOTAL_SCORE <- rowSums(hei0409[, c("HEIX1_TOTALVEG", "HEIX2_GREEN_AND_BEAN", "HEIX3_TOTALFRUIT", "HEIX4_WHOLEFRUIT",
                                                   "HEIX5_WHOLEGRAIN", "HEIX6_TOTALDAIRY", "HEIX7_TOTPROT", "HEIX8_SEAPLANT_PROT",
                                                   "HEIX9_FATTYACID", "HEIX10_SODIUM", "HEIX11_REFINEDGRAIN", "HEIX12_ADDEDSUGARS", "HEIX13_SATFATS")])

# Keep only HEI-related variables
keephei <- hei0409[, c("cpartid", "rikcal", "HEI2015_TOTAL_SCORE", "HEIX1_TOTALVEG", "HEIX2_GREEN_AND_BEAN",
                       "HEIX3_TOTALFRUIT", "HEIX4_WHOLEFRUIT", "HEIX5_WHOLEGRAIN", "HEIX6_TOTALDAIRY",
                       "HEIX7_TOTPROT", "HEIX8_SEAPLANT_PROT", "HEIX9_FATTYACID", "HEIX10_SODIUM",
                       "HEIX11_REFINEDGRAIN", "HEIX12_ADDEDSUGARS", "HEIX13_SATFATS", "hei_totveg",
                       "hei_greensbeans", "hei_totfruit", "hei_wholefruit", "hei_dairy", "hei_wholegrains",
                       "hei_totproteins", "hei_seafoodplantprot", "hei_refinedgrains", "hei_sodium",
                       "hei_addedsugars", "xhei_totfruit", "xhei_wholefruit", "xhei_totveg",
                       "xhei_greensbeans", "xhei_wholegrains", "xhei_dairy", "xhei_totproteins",
                       "xhei_seafoodplantprot", "xhei_refinedgrains", "xhei_fatacid", "xhei_sodium",
                       "xhei_addedsugars", "xhei_satfats")]

# Write results to csv
write.csv(keephei, paste0(prefix, "_HEI", ".csv"), row.names=FALSE)

          