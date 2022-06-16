################################################################################
#       Effect of enclosure change on Galapagos tortoises' behaviour           #
################################################################################



###___ WORKSPACE PREPARATION ___________________________________________________

rm(list=ls())
getwd()

library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(changepoint)
library(data.table)
library(scales)
library(EloRating)
library(lme4)



###___ LATENCY TO HABITUATION ________________________________________________

#__Data import__
df_time <- read.csv("Data_Time_budgets.csv", sep=";")
lapply(df_time, class)
df_time <- df_time %>%  mutate(Date = dmy(Date),
                               Arena = factor(Arena, ordered = FALSE),
                               Individual = factor(Individual, ordered = FALSE))


#__Change-point analysis__
# (replace for each tortoise)
Pol_act <- as.data.table(df_time)[Individual == "Polly" & Arena == "New"]
plot(Pol_act$DaysCount, Pol_act$Active, type = "b", 
     xlab = "Days", ylab = "Time active (mins)")
de.tr_Polly <- c(0,diff(Pol_act$Active))
plot(de.tr_Polly)                                             # Difference to the mean
cpts(cpt.mean(de.tr_Polly, method = "BinSeg"))                # Binary Segmentation: D6 for all
plot(cpt.mean(c(0, diff(Pol_act$Active)), method = "BinSeg")) # Locate the changepoint 

# (check assumptions of normality and independence of the residuals)
m1 = (de.tr_Polly)
m1.amoc = cpt.mean(de.tr_Polly)
means = param.est(m1.amoc)$mean
m1.resid = m1 - rep(means, seg.len(m1.amoc))
shapiro.test(m1.resid) #normality
acf(m1.resid)          #no-autocorrelation

# (colors for the changepoints)
df_time$color <- case_when(df_time$Arena == "New" & df_time$DaysCount == "2" | df_time$DaysCount == "6"~ "red3",
                           df_time$Arena == "New" & df_time$DaysCount == "3" & df_time$Individual == "Dolly" ~ "red3",
                           df_time$Arena == "New" & df_time$DaysCount == "4" & df_time$Individual == "Polly" ~ "red3",
                           df_time$Arena == "New" & df_time$DaysCount == "4" & df_time$Individual == "Priscilla" ~ "red3",
                           df_time$Arena == "New" & df_time$DaysCount == "5" & df_time$Individual == "Dolly" ~ "red3",
                           df_time$Arena == "New" & df_time$DaysCount == "5" & df_time$Individual == "Polly" ~ "red3",
                           TRUE ~ "black")

#__Control chart__
df_time %>%
  mutate(Arena = factor(Arena, levels = c("Old", "New")),
          DaysCount = factor(DaysCount, ordered = FALSE)) %>% 
  ggplot(aes(x=DaysCount, y=Active, group=Individual)) +
  geom_point(aes(color = color)) +
  scale_color_identity() + 
  geom_line(aes(linetype=Individual)) +
  theme_minimal() +
  facet_grid(. ~ Arena) +
  labs (title = "Variation in activity time throughout days") +
  xlab("Days of observation in each arena") +
  ylab("Time active (mins)") +
  theme(
    plot.title = element_text(size = 19, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text.x = element_text(size = 15, face="bold"))



###___ TIME BUDGETS IN HABITUATED INDIVIDUALS __________________________________

#__Remove new arena habituation period [D1-D6]__
df_budgets <- df_time %>% filter(Date != "2021-09-09", 
                                 Date != "2021-09-10",
                                 Date != "2021-09-11",
                                 Date != "2021-09-12",
                                 Date != "2021-09-13",
                                 Date != "2021-09-14") %>% select(-color)

#__Average and sd per individual__
df_budgets %>%
  group_by(Arena, Individual) %>%
  get_summary_stats(Walking, Eating, Water_bathing, Mud_bathing, Heat_bathing, Resting, OOS,
                    Active, Inactive, Observed, type = "mean_sd")

# (turn into long format)
df_budgets <- aggregate(df_budgets[, 4:13], list(df_budgets$Individual, df_budgets$Arena), mean)
df_budgets <- df_budgets %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  rename(Individual = Group.1,
         Arena = Group.2)

#__Paired samples t-test__
# (replace for each response variable)
t.test(Walking ~ Arena, data = df_budgets, paired = TRUE) 

#__Data import__ 
df_budgets_pies <- read.csv("Summary_table_ind_time_budgets.csv", sep=";")
df_budgets_pies <- df_budgets_pies %>% mutate(Behaviour = factor(Behaviour, levels = c("Walking", "Eating", "Water_bathing",
                                                                                       "Mud_bathing", "Heat_bathing", "Resting",
                                                                                       "OOS", "Active", "Inactive", "Observed"))) 

# (colorblind friendly palettes)
time_budget_palette_new <- c("#CC79A7", "#009E73", "#56B4E9", "#E69F00", "#D55E00", "#F0E442", "#999999")
time_budget_palette_old <- c("#CC79A7", "#009E73", "#56B4E9", "#D55E00", "#F0E442", "#999999")

#__Pie charts__
# (replace for each tortoise/arena)
total <- sum(df_budgets_pies$Dolly.Old)
ggplot(df_budgets_pies %>% filter (Behaviour != "Mud_bathing",
                                                  Behaviour != "OOS") 
                      , aes(x="", y=Dolly.Old, fill=Behaviour)) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = time_budget_palette_old) +
  theme_void() +
  geom_col(color = "black") +
  labs(title = "Dolly's average time budget in the old enclosure") +
  geom_text(aes(x = 1.6, label = percent(Dolly.Old/total)), position = position_stack(vjust = .5)) 



###___ AGGRESSIVENESS __________________________________________________________

#__Data import__
df_inter <- read.csv("Data_Interactions.csv", sep=";")
lapply(df_inter, class)

# (format variables appropriately)
df_inter <- df_inter %>% mutate(Date = dmy(Date),
                           Daytime = factor(Daytime, ordered = TRUE),
                           Arena = factor(Arena, ordered = FALSE),
                           Outcome = factor(Outcome, ordered = FALSE),
                           Actor = factor(Actor, ordered = FALSE),
                           Recipient = factor(Recipient, ordered = FALSE),
                           ID_winner = factor(ID_winner, ordered = FALSE),
                           ID_looser = factor(ID_looser, ordered = FALSE),
                           LZeq = as.numeric(LZeq),
                           LZFmax = as.numeric(LZFmax),
                           Close_resource = factor(Close_resource, ordered = FALSE)) %>%
  mutate(Days = case_when(Date == "2021-03-14" ~ 1,
                          Date == "2021-03-15" ~ 2,
                          Date == "2021-03-16" ~ 3,
                          Date == "2021-03-17" ~ 4,
                          Date == "2021-03-18" ~ 5,
                          Date == "2021-09-09" ~ 1,
                          Date == "2021-09-10" ~ 2,
                          Date == "2021-09-11" ~ 3,
                          Date == "2021-09-12" ~ 4,
                          Date == "2021-09-13" ~ 5,
                          Date == "2021-09-14" ~ 6,
                          Date == "2021-09-15" ~ 7,
                          Date == "2021-09-16" ~ 8,
                          Date == "2021-09-17" ~ 9,
                          Date == "2021-09-18" ~ 10))

#__Stacked barplot of all interaction types per day__
daily_summary <- df_inter %>% group_by(Days, Arena, Outcome) %>% tally()
daily_summary %>% mutate(Arena = factor(Arena, levels = c("Old", "New")),
                                             Outcome = factor(Outcome, levels = c("Peaceful", "Intimidation", "Fight"))) %>%
  ggplot(aes(fill=Outcome, y=n, x=Days)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(. ~ Arena) +
  scale_fill_manual(values = c("rosybrown1", "sienna1", "red3")) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1,10,1))

#__Average and sd per interaction type once habituated__
daily_summary1 <- daily_summary %>% filter(Arena == "Old" | Arena == "New" & Days > 6) 
daily_summary1 %>%
  mutate(Arena = factor(Arena, levels = c("Old", "New")),
                                        Outcome = factor(Outcome, levels = c("Peaceful", "Intimidation", "Fight"))) %>%
  group_by(Arena, Outcome) %>%
  summarise(
    count = n(),
    mean = mean(n, na.rm = TRUE),
    sd = sd(n, na.rm = TRUE)
  )

#__Unpaired t-tests__
# (replace for each interaction type)
occu_inter <-  daily_summary1 %>% filter(Outcome == "Fight") 
with(occu_inter, shapiro.test(n[Arena == "Old"]))      #check normality of difference between pairs (Shapiro-Wilk: p>0.05 -> can assume normality)
with(occu_inter, shapiro.test(n[Arena == "New"]))      #idem
var.test(n ~ Arena, data = occu_inter)                 #check equal variance between pairs
t.test(n ~ Arena, data = occu_inter, var.equal = TRUE) #t-test: if p>0.05 no significant difference

#__Paired t-test__
occu_inter <-  daily_summary1 %>% filter(Outcome == "Fight" & Days != 5)
t.test(n ~ Arena, data = occu_inter, paired = TRUE) #t-test: if p>0.05 no significant difference

# (for all interactions)
tot_inter <- daily_summary %>% group_by(Days,Arena) %>% summarize(Interactions = sum(n)) 
with(tot_inter, shapiro.test(Interactions[Arena == "Old"])) 
with(tot_inter, shapiro.test(Interactions[Arena == "New"])) 
var.test(Interactions ~ Arena, data = tot_inter) 
t.test(Interactions ~ Arena, data = tot_inter, var.equal = TRUE)

#__Two proportions Z-test__
daily_summary %>% group_by(Arena) %>% summarize(Interactions = sum(n)) 
daily_summary %>% filter(Outcome == "Fight") %>% group_by(Arena) %>% summarize(Interactions = sum(n)) 
prop.test(x = c(27, 15), n = c(169, 37)) # Vector x = (nb fights in OLD, nb fights in NEW) _ n = (nb interactions in OLD, nb interactions in NEW) 

 

###___ ELO RATINGS AND HIERARCHY _______________________________________________

#__Adapt the data: package can't handle data if all interactions/period occur within the same month -> must make fake dates that cross bt months
df_elo <- df_inter %>% filter(Outcome != "Peaceful") %>% 
  mutate(Fake_date = case_when(Date < "2021-04-01" ~ Date - 16,
                               Date > "2021-04-01" ~ Date - 16)) %>% 
  filter(Arena == "New",
         Date > "2021-09-14") 

#__Compute Elo ratings__
myk <- list(Fight = 100, Intimidation = 200)  #fights (k=100) and intimidations (k=200)
elosum <- elo.seq(winner=df_elo$ID_winner, loser=df_elo$ID_looser, Date=df_elo$Fake_date, intensity = df_elo$Outcome, k=myk) # draw=NULL,presence=NULL, startvalue=1000, k=100, normprob=TRUE, init="average", intensity=NULL, iterate=0, progressbar=FALSE) 
summary(elosum)
extract_elo(elosum)
eloplot(elosum, color = FALSE)
stab_elo(elosum)
table(df_elo$Actor)
table(df_elo$ID_looser)

#__Plot Elo ratings per individual__
df_plot<- df_inter %>% filter(Outcome != "Peaceful") %>%
  mutate(Fake_date = case_when(Date < "2021-04-01" ~ Date + 15,
                               Date > "2021-04-01" ~ Date - 165)) %>% #The package can't handle data if, within each period, all interactions occur within the same month -> must make fake dates that cross bt months
  filter(Date < "2021-03-19" | Date > "2021-09-14") 
elosum <- elo.seq(winner=df_plot$ID_winner, loser=df_plot$ID_looser, Date=df_plot$Fake_date, intensity = df_plot$Outcome, k=myk) # draw=NULL,presence=NULL, startvalue=1000, k=100, normprob=TRUE, init="average", intensity=NULL, iterate=0, progressbar=FALSE) 
eloplot(elosum, color = FALSE)



###___ DETERMINANTS OF THE OUTCOME OF AN INTERACTION ___________________________

#__Data import__
df_deter <- df_inter %>% filter(Arena == "New" & Days > 6)  %>%
  mutate(Fight = case_when(Outcome == "Fight" ~ 1,
                           TRUE ~ 0),
         Daytime = factor(Daytime, ordered = FALSE),
         Pair = factor(case_when(Actor == "Polly" & Recipient == "Priscilla" | Actor == "Priscilla" & Recipient == "Polly" ~ "A",
                                 Actor == "Polly" & Recipient == "Dolly" | Actor == "Dolly" & Recipient == "Polly" ~ "B",
                                 TRUE ~ "C"), ordered = FALSE))

mod <- glm(Fight ~ Daytime + Pair + LZeq + Nb_visitors + Close_to_resource + Days,
           family=binomial,
           data = df_deter)
summary(mod)

