################################################################################
#                                                                              #
#                     Characteristics charts                                   #
#                     ######################                                   #
#                                                                              #
# This code creates the characteristics visualisations and the comparison      #
# on_road cycle lanes and off-road cycle tracks                                #
# road cycle lanes/tracks                                                      #
################################################################################

# load packages
library(tidyverse)
library(sf)
library(summarytools)
library(units)
library(ggpmisc) # adding tables to ggplot
library(scales)  # allows ggplot axis labels to be wrapped


# load datasets
# These datasets were downloaded from TFL 25th February 2021 and data cleansed
c_asl = readRDS(file = "data/cleansed_asl")
c_crossings = readRDS(file = "data/cleansed_crossings")
c_cyclelanetrack = readRDS(file = "data/cleansed_cycle_lane_track")
c_signals = readRDS(file = "data/cleansed_signals")
c_trafficcalming = readRDS(file = "data/cleansed_trafficcalming")
c_restrictedroutes = readRDS(file = "data/cleansed_restricted_route")
c_restrictedpoints = readRDS(file = "data/cleansed_restrictedpoints")
c_signage = readRDS(file = "data/cleansed_signage")
c_parking = readRDS(file = "data/cleansed_parking")


################################################################################
# Create visualisations of the characteristics:
# - bar charts for count/% 
# - density plots for length/width


# 1) ASL
asl_charac = c_asl %>%
  st_drop_geometry() %>%
  select(contains("ASL")) %>%
  mutate(ASL_COLOUR_F = case_when(ASL_COLOUR == "NONE" ~ "FALSE", 
                                  TRUE ~ "TRUE")) %>%
  select(-c(ASL_COLOUR)) # create df of just ASL characteristics 

ASL_FDR = asl_charac$ASL_FDR %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDR == "TRUE") %>%
  rename(charac = ASL_FDR)  # create df of feeder lane characteristic with freq and %
ASL_FDR[1] <- "ASL_FDR" # relabel 'TRUE' with characteristic

ASL_FDRLFT = asl_charac$ASL_FDRLFT %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDRLFT == "TRUE") %>%
  rename(charac = ASL_FDRLFT)
ASL_FDRLFT[1] <- "ASL_FDRLFT"

ASL_FDCENT = asl_charac$ASL_FDCENT %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDCENT == "TRUE") %>%
  rename(charac = ASL_FDCENT)
ASL_FDCENT[1] <- "ASL_FDCENT"

ASL_FDRIGH = asl_charac$ASL_FDRIGH %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_FDRIGH == "TRUE") %>%
  rename(charac = ASL_FDRIGH)
ASL_FDRIGH[1] <- "ASL_FDRIGH"

ASL_SHARED = asl_charac$ASL_SHARED %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_SHARED == "TRUE") %>%
  rename(charac = ASL_SHARED)
ASL_SHARED[1] <- "ASL_SHARED"

ASL_COLOUR_F = asl_charac$ASL_COLOUR_F %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(ASL_COLOUR_F == "TRUE") %>%
  rename(charac = ASL_COLOUR_F)
ASL_COLOUR_F[1] <- "ASL_COLOUR_F"

asl_nil = filter_all(asl_charac, all_vars(. == "FALSE")) #= 224 
ASL_NIL = c("ASL_NIL", nrow(asl_nil), round((nrow(asl_nil)/nrow(c_asl)*100), digits = 1))

ASL = rbind(ASL_FDR, ASL_NIL,ASL_FDRLFT, ASL_COLOUR_F, ASL_FDCENT, ASL_FDRIGH, ASL_SHARED) %>%
  mutate(pct = round(as.numeric(pct))) %>%  # convert pct from character to numeric
  mutate(Percentage = paste0(pct, "%")) %>%   # create new text version of %
  mutate(Count = as.numeric(freq)) %>%  # convert to numeric for plotting
  mutate(charac = factor(charac,
                         levels = c("ASL_SHARED", "ASL_FDRIGH","ASL_FDCENT", "ASL_COLOUR_F", 
                                    "ASL_NIL", "ASL_FDRLFT", "ASL_FDR"),
                         labels = c(".            Shared nearside lane", "Right feeder lane", 
                                    "Centre feeder lane", "Coloured tarmac", 
                                    "No characteristics", "Left feeder lane",  
                                    "Feeder lane present"))) # factor and order correctly
  
#manually recode
ASL[6,4] = "0.7%" # manually recode
ASL[7,4] = "0.2%"

# # create stacked bar chart of ASL count with % in text
asl_count = ggplot() +
  geom_bar(data = ASL,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 800, 1600), limits = c(0, 2100)) +
  geom_text(data = ASL, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))

asl_count_space = ggplot() +
  geom_bar(data = ASL,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20),
        plot.margin = unit(c(0, 0, 0, 1.75), "cm")) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 800, 1600), limits = c(0, 2100)) +
  geom_text(data = ASL, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))

# 2) Crossings
cross_charac = c_crossings %>%
  st_drop_geometry() %>%
  select(contains("CRS"))  # create df of just crossing characteristics 

CRS_SIGNAL = cross_charac$CRS_SIGNAL %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CRS_SIGNAL == "TRUE") %>%
  rename(charac = CRS_SIGNAL)  # create df of crossing signal characteristic with freq and %
CRS_SIGNAL[1] <- "CRS_SIGNAL" # relabel 'TRUE' with characteristic

CRS_SEGREG = cross_charac$CRS_SEGREG %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CRS_SEGREG == "TRUE") %>%
  rename(charac = CRS_SEGREG)
CRS_SEGREG[1] <- "CRS_SEGREG"

CRS_CYGAP = cross_charac$CRS_CYGAP %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CRS_CYGAP == "TRUE") %>%
  rename(charac = CRS_CYGAP)
CRS_CYGAP[1] <- "CRS_CYGAP"

CRS_PEDEST = cross_charac$CRS_PEDEST %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CRS_PEDEST == "TRUE") %>%
  rename(charac = CRS_PEDEST)
CRS_PEDEST[1] <- "CRS_PEDEST"

CRS_LEVEL = cross_charac$CRS_LEVEL %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CRS_LEVEL == "TRUE") %>%
  rename(charac = CRS_LEVEL)
CRS_LEVEL[1] <- "CRS_LEVEL"

crs_nil = filter_all(cross_charac, all_vars(. == "FALSE")) 
CRS_NIL = c("CRS_NIL", nrow(crs_nil), round((nrow(crs_nil)/nrow(c_crossings)*100), digits = 1))

CRS = rbind(CRS_SIGNAL, CRS_SEGREG, CRS_NIL, CRS_CYGAP, CRS_PEDEST, CRS_LEVEL) %>%
  mutate(pct = round(as.numeric(pct))) %>%  # convert pct from character to numeric
  mutate(Percentage = paste0(pct, "%")) %>%   # create new text version of %
  mutate(Count = as.numeric(freq)) %>%  # convert to numeric for plotting
  mutate(charac = factor(charac,
                         levels = c("CRS_LEVEL", "CRS_PEDEST", "CRS_CYGAP", "CRS_NIL", 
                                    "CRS_SEGREG", "CRS_SIGNAL"),
                         labels = c("Crossing over rail or tram tracks", "Cyclists must dismount",
                                    "Gap in island or kerb", "No characteristics", 
                                    "Cyclists segregated", "Signal controlled crossing")))

# Create stacked bar chart of CRS count with % in text
cross_count = ggplot() +
  geom_bar(data = CRS,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 800, 1600), limits = c(0, 2100)) +
  scale_y_discrete(labels = wrap_format(30)) +
  geom_text(data = CRS, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5)

cross_count_space = ggplot() +
  geom_bar(data = CRS,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20),
        plot.margin = unit(c(0, 0, 0, 1.15), "cm")) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 800, 1600), limits = c(0, 2100)) +
  scale_y_discrete(labels = wrap_format(30)) +
  geom_text(data = CRS, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5)


# 3) CLT
clt_charac = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  select(contains("CLT")) %>%
  mutate(CLT_COLOUR_F = case_when(CLT_COLOUR == "NONE" ~ "FALSE", 
                                  TRUE ~ "TRUE")) %>%
  select(-c(CLT_COLOUR)) # create df of just clt characteristics 

CLT_CARR = clt_charac$CLT_CARR %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_CARR == "TRUE") %>%
  rename(charac = CLT_CARR)  # create df of on/off characteristic with freq and %
CLT_CARR[1] <- "CLT_CARR" # relabel 'TRUE' with characteristic

CLT_SEGREG = clt_charac$CLT_SEGREG %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_SEGREG == "TRUE") %>%
  rename(charac = CLT_SEGREG)
CLT_SEGREG[1] <- "CLT_SEGREG"

CLT_STEPP = clt_charac$CLT_STEPP %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_STEPP == "TRUE") %>%
  rename(charac =CLT_STEPP)
CLT_STEPP[1] <- "CLT_STEPP"

CLT_PARSEG = clt_charac$CLT_PARSEG %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_PARSEG== "TRUE") %>%
  rename(charac = CLT_PARSEG)
CLT_PARSEG[1] <- "CLT_PARSEG"

CLT_SHARED = clt_charac$CLT_SHARED %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_SHARED == "TRUE") %>%
  rename(charac = CLT_SHARED)
CLT_SHARED[1] <- "CLT_SHARED"

CLT_MANDAT = clt_charac$CLT_MANDAT %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_MANDAT == "TRUE") %>%
  rename(charac = CLT_MANDAT)
CLT_MANDAT[1] <- "CLT_MANDAT"

CLT_ADVIS = clt_charac$CLT_ADVIS %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_ADVIS == "TRUE") %>%
  rename(charac = CLT_ADVIS)
CLT_ADVIS[1] <- "CLT_ADVIS"

CLT_PRIORI = clt_charac$CLT_PRIORI %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_PRIORI == "TRUE") %>%
  rename(charac = CLT_PRIORI)
CLT_PRIORI[1] <- "CLT_PRIORI"

CLT_CONTRA = clt_charac$CLT_CONTRA %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_CONTRA == "TRUE") %>%
  rename(charac = CLT_CONTRA)
CLT_CONTRA[1] <- "CLT_CONTRA"

CLT_SHARED = clt_charac$CLT_SHARED %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_SHARED == "TRUE") %>%
  rename(charac = CLT_SHARED)
CLT_SHARED[1] <- "CLT_SHARED"

CLT_BIDIRE = clt_charac$CLT_BIDIRE %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_BIDIRE == "TRUE") %>%
  rename(charac = CLT_BIDIRE)
CLT_BIDIRE[1] <- "CLT_BIDIRE"

CLT_CBYPAS = clt_charac$CLT_CBYPAS %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_CBYPAS == "TRUE") %>%
  rename(charac = CLT_CBYPAS)
CLT_CBYPAS[1] <- "CLT_CBYPAS"

CLT_BBYPAS = clt_charac$CLT_BBYPAS %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_BBYPAS == "TRUE") %>%
  rename(charac = CLT_BBYPAS)
CLT_BBYPAS[1] <- "CLT_BBYPAS"

CLT_PARKR = clt_charac$CLT_PARKR %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_PARKR == "TRUE") %>%
  rename(charac = CLT_PARKR)
CLT_PARKR[1] <- "CLT_PARKR"

CLT_WATERR = clt_charac$CLT_WATERR %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_WATERR == "TRUE") %>%
  rename(charac = CLT_WATERR)
CLT_WATERR[1] <- "CLT_WATERR"

CLT_PTIME = clt_charac$CLT_PTIME %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_PTIME == "TRUE") %>%
  rename(charac = CLT_PTIME)
CLT_PTIME[1] <- "CLT_PTIME"

CLT_COLOUR_F = clt_charac$CLT_COLOUR_F %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(CLT_COLOUR_F == "TRUE") %>%
  rename(charac = CLT_COLOUR_F)
CLT_COLOUR_F[1] <- "CLT_COLOUR_F"

clt_nil = filter_all(clt_charac, all_vars(. == "FALSE")) #= nil so no obs have no characteristics 

CLT = rbind(CLT_CARR, CLT_BIDIRE, CLT_SHARED, CLT_ADVIS, CLT_COLOUR_F, CLT_PARKR, 
            CLT_PARSEG, CLT_PTIME, CLT_PRIORI, CLT_SEGREG, CLT_MANDAT, CLT_CONTRA, 
            CLT_WATERR, CLT_BBYPAS, CLT_STEPP, CLT_CBYPAS) %>%
  mutate(pct = round(as.numeric(pct))) %>%  # convert pct from character to numeric
  mutate(Percentage = paste0(pct, "%")) %>%   # create new text version of %
  mutate(Count = as.numeric(freq)) %>%  # convert to numeric for plotting
  mutate(charac = factor(charac,
                         levels = c("CLT_CBYPAS", "CLT_STEPP", "CLT_BBYPAS", "CLT_WATERR", 
                                    "CLT_CONTRA", "CLT_MANDAT", "CLT_SEGREG", "CLT_PRIORI", 
                                    "CLT_PTIME", "CLT_PARSEG", "CLT_PARKR", "CLT_COLOUR_F",
                                    "CLT_ADVIS", "CLT_SHARED", "CLT_BIDIRE","CLT_CARR"),
                         labels = c("Cycle bypass at traffic signals", "Stepped segregation", 
                                    "Continuous cycle facilities at bus stop", 
                                    "Route by water", 
                                    "Contraflow cycle lane/track", "Mandatory cycle lane", 
                                    "Fully segregated", "Cycle lane/track has priority", 
                                    "Part-time cycle lane/track", "Partially segregated", 
                                    "Route by or through a Park", "Coloured tarmac", 
                                    "Advisory cycle lane", "Shared with other users", 
                                    "Bi-directional (two-way flow)", "On-carriageway"))) 

CLT[14,4] = "0.5%" # manually recode
CLT[15,4] = "0.4%"
CLT[16,4] = "0.2%"

# # create stacked bar chart of CLT count with % in text
clt_count = ggplot() +
  geom_bar(data = CLT,
           aes(x = Count, y = charac), stat = "identity", 
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 5000, 10000), limits = c(0, 16100)) +
  geom_text(data = CLT, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))

clt_count_space = ggplot() +
  geom_bar(data = CLT,
           aes(x = Count, y = charac), stat = "identity", 
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20),
        plot.margin = unit(c(0, 0, 0, 0.6), "cm")) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 5000, 10000), limits = c(0, 16100)) +
  geom_text(data = CLT, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))



# 4) Signals
sig_charac = c_signals %>%
  st_drop_geometry() %>%
  select(contains("SIG")) # create df of just SIG characteristics 

SIG_HEAD = sig_charac$SIG_HEAD %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(SIG_HEAD == "TRUE") %>%
  rename(charac = SIG_HEAD)  # create df of characteristic with freq and %
SIG_HEAD[1] <- "SIG_HEAD" # relabel 'TRUE' with characteristic

SIG_SEPARA = sig_charac$SIG_SEPARA %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(SIG_SEPARA == "TRUE") %>%
  rename(charac = SIG_SEPARA)  
SIG_SEPARA[1] <- "SIG_SEPARA" 

SIG_EARLY = sig_charac$SIG_EARLY %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(SIG_EARLY == "TRUE") %>%
  rename(charac = SIG_EARLY)  
SIG_EARLY[1] <- "SIG_EARLY" 

SIG_TWOSTG = sig_charac$SIG_TWOSTG%>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(SIG_TWOSTG == "TRUE") %>%
  rename(charac = SIG_TWOSTG)  
SIG_TWOSTG[1] <- "SIG_TWOSTG" 

SIG_GATE= sig_charac$SIG_GATE %>%freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(SIG_GATE == "TRUE") %>%
  rename(charac = SIG_GATE)  
SIG_GATE[1] <- "SIG_GATE" 

sig_nil = filter_all(sig_charac, all_vars(. == "FALSE")) #= 4
SIG_NIL = c("SIG_NIL", nrow(sig_nil), round((nrow(sig_nil)/nrow(c_signals)*100), digits = 1))

SIG = rbind(SIG_EARLY, SIG_GATE, SIG_HEAD, SIG_NIL, SIG_SEPARA, SIG_TWOSTG) %>%
  mutate(pct = round(as.numeric(pct))) %>%  # convert pct from character to numeric
  mutate(Percentage = paste0(pct, "%")) %>%   # create new text version of %
  mutate(Count = as.numeric(freq)) %>%  # convert to numeric for plotting
  mutate(charac = factor(charac,
                         levels = c("SIG_NIL", "SIG_TWOSTG", "SIG_GATE", "SIG_EARLY", "SIG_SEPARA", "SIG_HEAD"),
                         labels = c("No characteristics", "Two-stage right turn", 
                                    "Cycle/bus gate allowing cycles through",
                                    "Early release for cyclists", "Separate stage for cyclists", 
                                    "Cycle symbol on signal lights"))) 
SIG[4,4] = "0.9%"

# # create stacked bar chart of Signal count with % in text
sig_count = ggplot() +
  geom_bar(data = SIG,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 300), limits = c(0, 525)) +
  geom_text(data = SIG, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))

sig_count_space = ggplot() +
  geom_bar(data = SIG,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        plot.margin = unit(c(0, 0.0, 0, 0.35), "cm")) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 300), limits = c(0, 525)) +
  geom_text(data = SIG, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))


# did 0.1 then 0.5

#5) Traffic calming
tc_charac = c_trafficcalming %>%
  st_drop_geometry() %>%
  select(contains("TRF"))

TRF_RAISED = tc_charac$TRF_RAISED %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_RAISED == "TRUE") %>%
  rename(charac = TRF_RAISED)  # create df of characteristic with freq and %
TRF_RAISED[1] <- "TRF_RAISED" # relabel 'TRUE' with characteristic

TRF_ENTRY = tc_charac$TRF_ENTRY %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_ENTRY == "TRUE") %>%
  rename(charac = TRF_ENTRY)  
TRF_ENTRY[1] <- "TRF_ENTRY" 

TRF_CUSHI = tc_charac$TRF_CUSHI %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_CUSHI == "TRUE") %>%
  rename(charac = TRF_CUSHI)  
TRF_CUSHI[1] <- "TRF_CUSHI" 

TRF_HUMP = tc_charac$TRF_HUMP %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_HUMP == "TRUE") %>%
  rename(charac = TRF_HUMP)  
TRF_HUMP[1] <- "TRF_HUMP" 

TRF_SINUSO = tc_charac$TRF_SINUSO %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_SINUSO == "TRUE") %>%
  rename(charac = TRF_SINUSO)  
TRF_SINUSO[1] <- "TRF_SINUSO" 

TRF_BARIER = tc_charac$TRF_BARIER %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_BARIER == "TRUE") %>%
  rename(charac = TRF_BARIER)  
TRF_BARIER[1] <- "TRF_BARIER"

TRF_NAROW = tc_charac$TRF_NAROW %>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_NAROW == "TRUE") %>%
  rename(charac = TRF_NAROW)  
TRF_NAROW[1] <- "TRF_NAROW"

TRF_CALM = tc_charac$TRF_CALM%>% freq(cumul = FALSE, report.nas = FALSE) %>% tb() %>%
  filter(TRF_CALM == "TRUE") %>%
  rename(charac = TRF_CALM)  
TRF_CALM[1] <- "TRF_CALM"

tc_nil = filter_all(tc_charac, all_vars(. == "FALSE")) 
TRF_NIL = c("TRF_NIL", nrow(tc_nil), round((nrow(tc_nil)/nrow(c_trafficcalming)*100), digits = 2))


TC = rbind(TRF_BARIER, TRF_CALM, TRF_CUSHI, TRF_ENTRY, TRF_HUMP, TRF_NAROW, TRF_NIL, TRF_RAISED, TRF_SINUSO) %>%
  mutate(pct = round(as.numeric(pct))) %>%  # convert pct from character to numeric
  mutate(Percentage = paste0(pct, "%")) %>%   # create new text version of %
  mutate(Count = as.numeric(freq)) %>%
  mutate(charac = factor(charac,
                         levels = c("TRF_NIL", "TRF_NAROW", "TRF_CALM", "TRF_BARIER", 
                                    "TRF_RAISED", "TRF_SINUSO", "TRF_ENTRY", 
                                    "TRF_CUSHI", "TRF_HUMP"),
                         labels = c("No characteristics", "Carriageway narrowing e.g. chicane", 
                                    "Other traffic calming measure", "Barrier cyclists can pass", 
                                    "Raised table at junction", "Hump/cushion is sinusoidal", 
                                    "Side road entry treatment e.g. raised", "Speed cushion", "Speed hump"))) 
TC[7,4] = "0.02%" # manually recode

# # create stacked bar chart of traffic count with % in text
tc_count = ggplot() +
  geom_bar(data = TC,
           aes(x = Count, y = charac), stat = "identity",
           colour = "black", fill = "grey", width = 0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"), # adds axis line back in
        axis.title.y = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 20000), limits = c(0, 39000)) +
  geom_text(data = TC, aes(x = Count, label = Percentage, y = charac),
            hjust = -0.3, size = 5) +
  scale_y_discrete(labels = wrap_format(30))





# Part 2: Create density plots of characteristics for ASL, crossings and CLT 
# width when saving 190
# height asl- 330, cross 300, clt 670


# 1) ASL
asl_df = c_asl %>%
  mutate(length = st_length(geometry)) # gives me the length of all ASL
summary(asl_df$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.222   3.897   4.503   4.597   5.082  20.633 

asl_nil = asl_df %>%
  filter(ASL_FDR == FALSE & ASL_FDRLFT == FALSE & ASL_FDCENT == FALSE & ASL_FDRIGH == FALSE &
           ASL_SHARED == FALSE & ASL_COLOUR == "NONE") %>%
  mutate(charac = "ASL_NIL")  # create df of just asl_nil and their assoc length
asl_feeder = asl_df %>%
  filter(ASL_FDR == TRUE) %>%
  mutate(charac = "ASL_FDR")
asl_left = asl_df %>%
  filter(ASL_FDRLFT == TRUE) %>%
  mutate(charac = "ASL_FDRLFT")
asl_centre = asl_df %>%
  filter(ASL_FDCENT == TRUE) %>%
  mutate(charac = "ASL_FDCENT")
asl_right = asl_df %>%
  filter(ASL_FDRIGH == TRUE) %>%
  mutate(charac = "ASL_FDRIGH")
asl_shared = asl_df %>%
  filter(ASL_SHARED == TRUE) %>%
  mutate(charac = "ASL_SHARED")
asl_colour = asl_df %>%
  filter(ASL_COLOUR != "NONE") %>%
  mutate(charac = "ASL_COLOUR_F")
asl_density_df = rbind(asl_feeder, asl_nil, asl_left, asl_colour, asl_centre, asl_right, asl_shared) %>%
  st_drop_geometry() %>%
  mutate(charac = factor(charac,
                         levels = c("ASL_FDR", "ASL_FDRLFT", "ASL_NIL", "ASL_COLOUR_F", "ASL_FDCENT",
                                    "ASL_FDRIGH", "ASL_SHARED"),
                         labels = c("Feeder lane present", "Left feeder lane", "No characteristics", 
                                    "Coloured tarmac", "Centre feeder lane", "Right feeder lane", 
                                    "Shared e.g. with buses"))) %>%
  select(c(length, charac)) %>%
  units::drop_units()  # finalise density plot df with rows for every characteristic (contains 6220 obs)

# Obtain summary stats by grouped characteristics
asl_gp_med = asl_density_df %>%
  group_by(charac) %>%
  summarise(grp_median = median(length), grp_max = max(length)) # identifies longest asls in each group

# Create dataframe of which facets needs astericks
asl_ann_text = data.frame(x = rep(10, 7),
                          y = rep(0, 7), 
                          charac = factor(c("Feeder lane present", "Left feeder lane", "No characteristics", 
                                            "Coloured tarmac", "Centre feeder lane", "Right feeder lane", 
                                            "Shared e.g. with buses"), 
                                          levels = c("Feeder lane present", "Left feeder lane", "No characteristics", 
                                                             "Coloured tarmac", "Centre feeder lane", "Right feeder lane", 
                                                             "Shared e.g. with buses")), 
                          label = c("*", "*", "*", "*", "", "", ""))

# Create density plot of ASL length with median and * indicated axis truncated
asl_density = ggplot(asl_density_df) +
  geom_density(aes(x = length, fill = "grey"), fill = "grey") +
  facet_wrap(~charac, ncol = 1) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 5, 10), limits = c(0, 10.4)) +
  geom_vline(data = asl_gp_med, aes(xintercept = grp_median),
             linetype = "longdash") +
  xlab(label = "Length (m)") +
  geom_text(data = asl_ann_text, aes(x = x, y = y, label = label), size = 8, show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_blank(),
      axis.text = element_text(size = 16),
      axis.title.x = element_text(size = 20))

# 2) Crossings
cross_df = c_crossings %>%
  mutate(width = st_length(geometry)) # gives me the width of all Crossings

cross_nil = cross_df %>%
  filter(CRS_SIGNAL == FALSE & CRS_SEGREG == FALSE & CRS_CYGAP == FALSE & CRS_PEDEST == FALSE &
           CRS_LEVEL == FALSE) %>%
  mutate(charac = "CRS_NIL")  # create df of just crossing_nil and their assoc length
cross_sig = cross_df %>%
  filter(CRS_SIGNAL == TRUE) %>%
  mutate(charac = "CRS_SIGNAL")
cross_seg = cross_df %>%
  filter(CRS_SEGREG == TRUE) %>%
  mutate(charac = "CRS_SEGREG")
cross_gap = cross_df %>%
  filter(CRS_CYGAP == TRUE) %>%
  mutate(charac = "CRS_CYGAP")
cross_ped = cross_df %>%
  filter(CRS_PEDEST == TRUE) %>%
  mutate(charac = "CRS_PEDEST")
cross_level = cross_df %>%
  filter(CRS_LEVEL == TRUE) %>%
  mutate(charac = "CRS_LEVEL")

cross_density_df = rbind(cross_level, cross_ped, cross_gap, cross_nil, cross_seg, cross_sig) %>%
  st_drop_geometry() %>%
  mutate(charac = factor(charac,
                         levels = c("CRS_SIGNAL", "CRS_SEGREG", "CRS_NIL", "CRS_CYGAP",
                                    "CRS_PEDEST", "CRS_LEVEL"),
                         labels = c("Signal controlled crossing", "Cyclists segregated from other users",
                                    "No characteristics", "Gap in island or kerb for cyclists", 
                                    "Pedestrian-only crosssing (Cyclists dismount)", 
                                    "Crossing over rail or tram tracks"))) %>%
  select(c(width, charac)) %>%
  units::drop_units()  # finalise density plot df with rows for every characteristic (contains 2285 obs)

# # Obtain summary stats by grouped characteristics and max widths for astericks
cross_gp_med = cross_density_df %>%
  group_by(charac) %>%
  summarise(grp_median = median(width), grp_max = max(width))

# Create dataframe of which facets needs astericks
cross_ann_text = data.frame(x = rep(24, 6),
                          y = rep(0.01, 6), 
                          charac = factor(c("Signal controlled crossing", "Cyclists segregated from other users",
                                            "No characteristics", "Gap in island or kerb for cyclists", 
                                            "Pedestrian-only crosssing (Cyclists dismount)", 
                                            "Crossing over rail or tram tracks"), 
                                          levels = c("Signal controlled crossing", "Cyclists segregated from other users",
                                                     "No characteristics", "Gap in island or kerb for cyclists", 
                                                     "Pedestrian-only crosssing (Cyclists dismount)", 
                                                     "Crossing over rail or tram tracks")), 
                          label = c("*", "*", "*", "*", "*", ""))



# Create density plot of crossing length with median
cross_density = ggplot(cross_density_df) +
  geom_density(aes(x = width, fill = "grey"), fill = "grey") +
  facet_wrap(~charac, ncol = 1) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 10, 20), limits = c(0, 25)) +
  geom_vline(data = cross_gp_med, aes(xintercept = grp_median),
             linetype = "longdash") +
  xlab(label = "Width (m)") +
  geom_text(data = cross_ann_text, aes(x = x, y = y, label = label), size = 8, show.legend = FALSE) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20))


# 3) CLT
clt_df = c_cyclelanetrack %>%
  mutate(length = st_length(geometry))

clt_carr = clt_df %>%
  filter(CLT_CARR == "TRUE") %>%
  mutate(charac = "CLT_CARR")
clt_seg = clt_df %>%
  filter(CLT_SEGREG == "TRUE") %>%
  mutate(charac = "CLT_SEGREG")
clt_stepp = clt_df %>%
  filter(CLT_STEPP == "TRUE") %>%
  mutate(charac = "CLT_STEPP")
clt_parseg = clt_df %>%
  filter(CLT_PARSEG == "TRUE") %>%
  mutate(charac = "CLT_PARSEG")
clt_shared = clt_df %>%
  filter(CLT_SHARED == "TRUE") %>%
  mutate(charac = "CLT_SHARED")
clt_mand = clt_df %>%
  filter(CLT_MANDAT == "TRUE") %>%
  mutate(charac = "CLT_MANDAT")
clt_advis = clt_df %>%
  filter(CLT_ADVIS == "TRUE") %>%
  mutate(charac = "CLT_ADVIS")
clt_priori = clt_df %>%
  filter(CLT_PRIORI == "TRUE") %>%
  mutate(charac = "CLT_PRIORI")
clt_contra = clt_df %>%
  filter(CLT_CONTRA == "TRUE") %>%
  mutate(charac = "CLT_CONTRA")
clt_bidire = clt_df %>%
  filter(CLT_BIDIRE == "TRUE") %>%
  mutate(charac = "CLT_BIDIRE")
clt_cbypas = clt_df %>%
  filter(CLT_CBYPAS == "TRUE") %>%
  mutate(charac = "CLT_CBYPAS")
clt_bbypas = clt_df %>%
  filter(CLT_BBYPAS == "TRUE") %>%
  mutate(charac = "CLT_BBYPAS")
clt_park = clt_df %>%
  filter(CLT_PARKR == "TRUE") %>%
  mutate(charac = "CLT_PARKR")
clt_water = clt_df %>%
  filter(CLT_WATERR == "TRUE") %>%
  mutate(charac = "CLT_WATERR")
clt_ptime = clt_df %>%
  filter(CLT_PTIME == "TRUE") %>%
  mutate(charac = "CLT_PTIME")
clt_colour = clt_df %>%
  filter(CLT_COLOUR != "NONE") %>%
  mutate(charac = "CLT_COLOUR_F")


clt_density_df = rbind(clt_carr, clt_seg, clt_stepp, clt_parseg, clt_shared, 
                       clt_mand, clt_advis, clt_priori, clt_contra, clt_bidire, 
                       clt_cbypas, clt_bbypas, clt_park, clt_water, clt_ptime, clt_colour) %>%
  st_drop_geometry() %>%
  mutate(charac = factor(charac,
                         levels = c("CLT_CARR", "CLT_BIDIRE", "CLT_SHARED", "CLT_ADVIS", 
                                    "CLT_COLOUR_F", "CLT_PARKR", "CLT_PARSEG", "CLT_PTIME", 
                                    "CLT_PRIORI", "CLT_SEGREG", "CLT_MANDAT", "CLT_CONTRA", 
                                    "CLT_WATERR", "CLT_BBYPAS", "CLT_STEPP", "CLT_CBYPAS"),
                         labels = c("On-carriageway", "Bi-directional (two-way flow)", 
                                    "Shared e.g. with buses", "Advisory cycle lane (painted line)", 
                                    "Coloured tarmac", "Route by or through a Park", 
                                    "Partially segregated", "Part-time cycle cycle lane/track", 
                                    "Cycle lane/track has priority over other users", 
                                    "Fully segregated", "Mandatory cycle lane (painted line)", 
                                    "Contraflow cycle lane/track", "Route by river, canal or water feature", 
                                    "Continuous cycle facilities at bus stop", 
                                    "Stepped segregation", "Cycle bypass at traffic signals"))) %>%
  select(c(length, charac)) %>%
  units::drop_units()  # finalise density plot df with rows for every characteristic (contains 67310 obs)

# Obtain summary stats by grouped characteristics
clt_gp_med = clt_density_df %>%
  group_by(charac) %>%
  summarise(grp_median = median(length), grp_max = max(length))

# Create dataframe of which facets needs astericks
clt_ann_text = data.frame(x = rep(200, 16),
                            y = rep(0.01, 16), 
                            charac = factor(c("On-carriageway", "Bi-directional (two-way flow)", 
                                              "Shared e.g. with buses", "Advisory cycle lane (painted line)", 
                                              "Coloured tarmac", "Route by or through a Park", 
                                              "Partially segregated", "Part-time cycle cycle lane/track", 
                                              "Cycle lane/track has priority over other users", 
                                              "Fully segregated", "Mandatory cycle lane (painted line)", 
                                              "Contraflow cycle lane/track", "Route by river, canal or water feature", 
                                              "Continuous cycle facilities at bus stop", 
                                              "Stepped segregation", "Cycle bypass at traffic signals"), 
                                            levels = c("On-carriageway", "Bi-directional (two-way flow)", 
                                                       "Shared e.g. with buses", "Advisory cycle lane (painted line)", 
                                                       "Coloured tarmac", "Route by or through a Park", 
                                                       "Partially segregated", "Part-time cycle cycle lane/track", 
                                                       "Cycle lane/track has priority over other users", 
                                                       "Fully segregated", "Mandatory cycle lane (painted line)", 
                                                       "Contraflow cycle lane/track", "Route by river, canal or water feature", 
                                                       "Continuous cycle facilities at bus stop", 
                                                       "Stepped segregation", "Cycle bypass at traffic signals")), 
                            label = c("*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*"))

# Create density plot of CLT length with median
clt_density = ggplot(clt_density_df) +
  geom_density(aes(x = length, fill = "grey"), fill = "grey") +
  facet_wrap(~charac, ncol = 1) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 190), limits = c(0, 208)) +
  geom_vline(data = clt_gp_med, aes(xintercept = grp_median),
             linetype = "longdash") +
  xlab(label = "Length (m)") +
  geom_text(data = clt_ann_text, aes(x = x, y = y, label = label), size = 8, show.legend = FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # removes all grid lines
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20))

###########################
# Saving plots

library(patchwork)
asl_charac = asl_count_space + asl_density + plot_layout(widths = c(3, 1)) # want 3:1 to get all labels displayed
cross_charac = cross_count_space + cross_density + plot_layout(widths = c(3, 1))
clt_charac = clt_count_space + clt_density + plot_layout(widths = c(3, 1))
sig_charac = sig_count_space + plot_spacer() + plot_layout(widths = c(3, 1))
tc_charac = tc_count + plot_spacer() + plot_layout(widths = c(3, 1))





################################################################################
# 6) Comparison of variables of on v off road infrastructure
# including drawing bar charts


# create new variable which is clearly on/off road
clt_on_off = c_cyclelanetrack %>%
  st_drop_geometry() %>%
  mutate(on_off = case_when(CLT_CARR == 'TRUE' ~ "onroad", 
                            TRUE ~ "offroad")) 

# create multiple datasets that measure length of variables by on/off road status
on_off_l = clt_on_off %>%
  group_by(on_off) %>%
  summarise(sum = sum(length_km)) %>%
  mutate(perc = drop_units(round((sum/sum(sum)*100), digit = 1)))

seg_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_SEGREG == TRUE) %>%
  summarise(segreg = sum(length_km)) %>%
  mutate(segreg_l_perc = drop_units(round((segreg/sum(segreg)*100), digit = 1)))
stepp_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_STEPP == TRUE) %>%
  summarise(stepp = sum(length_km)) %>%
  mutate(stepp_l_perc = drop_units(round((stepp/sum(stepp)*100), digit = 1)))
partseg_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PARSEG == TRUE) %>%
  summarise(partsegreg = sum(length_km)) %>%
  mutate(partsegreg_l_perc = drop_units(round((partsegreg/sum(partsegreg)*100), digit = 1)))
shared_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_SHARED == TRUE) %>%
  summarise(shared = sum(length_km)) %>%
  mutate(shared_l_perc = drop_units(round((shared/sum(shared)*100), digit = 1)))
mandat_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_MANDAT == TRUE) %>%
  summarise(mandat = sum(length_km)) %>%
  mutate(mandat_l_perc = drop_units(round((mandat/sum(mandat)*100), digit = 1)))
advis_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_ADVIS == TRUE) %>%
  summarise(advis = sum(length_km)) %>%
  mutate(advis_l_perc = drop_units(round((advis/sum(advis)*100), digit = 1)))
priority_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PRIORI == TRUE) %>%
  summarise(priority = sum(length_km)) %>%
  mutate(priority_l_perc = drop_units(round((priority/sum(priority)*100), digit = 1)))
contra_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_CONTRA == TRUE) %>%
  summarise(contra = sum(length_km)) %>%
  mutate(contra_l_perc = drop_units(round((contra/sum(contra)*100), digit = 1)))
bidir_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_BIDIRE == TRUE) %>%
  summarise(bidir = sum(length_km)) %>%
  mutate(bidir_l_perc = drop_units(round((bidir/sum(bidir)*100), digit = 1)))
bypass_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_CBYPAS == TRUE) %>%
  summarise(bypass = sum(length_km)) %>%
  mutate(bypass_l_perc = drop_units(round((bypass/sum(bypass)*100), digit = 1)))
busbypass_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_BBYPAS == TRUE) %>%
  summarise(busbypass = sum(length_km)) %>%
  mutate(busbypass_l_perc = drop_units(round((busbypass/sum(busbypass)*100), digit = 1)))
park_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PARKR == TRUE) %>%
  summarise(park = sum(length_km)) %>%
  mutate(park_l_perc = drop_units(round((park/sum(park)*100), digit = 1)))
water_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_WATERR == TRUE) %>%
  summarise(water = sum(length_km)) %>%
  mutate(water_l_perc = drop_units(round((water/sum(water)*100), digit = 1)))
parttime_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_PTIME == TRUE) %>%
  summarise(parttime = sum(length_km)) %>%
  mutate(parttime_l_perc = drop_units(round((parttime/sum(parttime)*100), digit = 1)))
colour_l = clt_on_off %>%
  group_by(on_off) %>%
  filter(CLT_COLOUR != "NONE") %>%
  summarise(colour = sum(length_km)) %>%
  mutate(colour_l_perc = drop_units(round((colour/sum(colour)*100), digit = 1)))

# join these datasets together to get summary of on/off road comparison of lengths
on_off_clt_comparison_lengths = plyr::join_all(
  list(colour_l, parttime_l, water_l, park_l, busbypass_l, bypass_l, bidir_l, contra_l, 
       priority_l, advis_l, mandat_l, shared_l, partseg_l, stepp_l, seg_l),
  by = 'on_off', type = 'left')
  
rm(colour_l, parttime_l, water_l, park_l, busbypass_l, bypass_l, bidir_l, contra_l, 
   priority_l, advis_l, mandat_l, shared_l, partseg_l, stepp_l, seg_l)

# convert NAs to 0 (there are no onroad lanes that are by water)
on_off_clt_comparison_lengths$water[is.na(on_off_clt_comparison_lengths$water)] = 0 
on_off_clt_comparison_lengths$water_l_perc[is.na(on_off_clt_comparison_lengths$water_l_perc)] = 0 

# get in format suitable for ggplot 
perc = on_off_clt_comparison_lengths %>%
  pivot_longer(cols = c(colour_l_perc, parttime_l_perc, water_l_perc, park_l_perc, busbypass_l_perc, bypass_l_perc, 
                        bidir_l_perc, contra_l_perc, priority_l_perc, advis_l_perc, mandat_l_perc, shared_l_perc, 
                        partsegreg_l_perc, stepp_l_perc, segreg_l_perc), 
               names_to = c('characteristic', '.value'),
               names_sep = "\\_l_") %>%
  select(c("on_off", "characteristic", "perc"))
length = on_off_clt_comparison_lengths %>%
  pivot_longer(cols = c("colour", "parttime", "water", "park", "busbypass", "bypass", 
                               "bidir", "contra", "priority", "advis", "mandat", "shared", 
                               "partsegreg", "stepp", "segreg"), 
               names_to = 'characteristic', 
               values_to = "length") %>%
  mutate(round_length = drop_units(round(length, digit = 1))) %>%
  select(c("on_off", "characteristic", "length", "round_length"))

# join together
on_off_comparison_lengths4ggplot = left_join(length, perc)

off_var_order = on_off_comparison_lengths4ggplot %>%
  filter(on_off == "offroad") %>%
  arrange(desc(perc))
test = pull(off_var_order$characteristic)


# Below creates new column that we use to order the bars and gives sensible variable labels
on_off_comparison_lengths4ggplot_order = on_off_comparison_lengths4ggplot %>%
  mutate(variable_order = factor(characteristic,
                                 levels = c("mandat", "priority", "advis", "contra",
                                            "stepp", "colour","parttime", "busbypass",
                                            "segreg", "bypass","shared","partsegreg", 
                                            "park", "bidir", "water"),
                                 labels = c("Mandatory cycle lane", "Given Priority", 
                                            "Advisory cycle lane", "Contraflow",
                                            "Stepped", "Coloured tarmac", "Part-time", 
                                            "Continuous facilities through bus stop", 
                                            "Fully-segregated", "Cycle bypass", 
                                            "Shared cycle lane", "Part-segregated",
                                            "Park route", "Bi-directional", "Water route"))) %>%
  mutate(on_off_order = factor(on_off,
                               levels = c("offroad", "onroad"), 
                               labels = c("Off-road", "On-road"))) # rename columns for legend label



# Create visualisations 
# Stacked bar chart of % of characteristics on/off road - matching table graphics 650x550
percent_chart = ggplot() +
  geom_bar(data = on_off_comparison_lengths4ggplot_order,
           aes(x = perc, y = variable_order, fill = on_off_order), stat = "identity",
           colour = "black") +
  scale_fill_manual(values = c("white", "grey")) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 50, 100), limits = c(0, 105)) + 
  labs(x = paste0("Percentage", "\n", "of Length")) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(), # removes y axis lines
        legend.position = "none",
        axis.text = element_text(size = 16, colour = "grey25"),
        axis.title.x = element_text(size = 20))

# create stacked bar chart of length - matching table graphics  400x550
length_chart = ggplot() +
  geom_bar(data = on_off_comparison_lengths4ggplot_order,
           aes(x = round_length, y = variable_order, fill = on_off_order), stat = "identity",
           colour = "black") +
  scale_fill_manual(values = c("white", "grey")) +
  labs(x = paste0("Length in km", "\n", "")) +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.text = element_text(size = 16, colour = "grey25"),
        panel.grid = element_blank(),  # removes all grid lines
        axis.line.x = element_line(size=0.1, color="black"),
        legend.title = element_blank(), # adds axis line back in
        legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20)) +
  scale_x_continuous(expand = c(0,0), breaks = c(0, 1500), limits = c(0, 2050))   
# save with width at 439

# Save on/off plots
library(patchwork)
on_off_fig = percent_chart + plot_spacer() + length_chart + plot_layout(widths = c(1.8, 0.05, 1.8))

