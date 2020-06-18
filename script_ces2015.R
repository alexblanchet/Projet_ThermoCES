################ Download and load data
temp <- tempfile()
download.file("https://ces-eec.sites.olt.ubc.ca/files/2017/04/CES2015_Combined_R.zip",temp)
load(unz(temp, "CES2015_Combined_R.RData"))
df <- CES2015_Combined
rm(CES2015_Combined)

################ Thermometer items' list
liste.thermo <- c("p_like_can","p_like_fran","p_like_angl", "p_like_abor", "p_like_femi", "p_like_immg",
                  "p_like_us", "p_like_qc", "p_like_gays", "p_like_mino", "p_like_polit")

var.names <- c("Language", "Canada", "Francophones", "Anglophones", "Aboriginals", 
               "Feminists", "Immigrants", "The US", "Quebec", "Gays & Lesbians", 
               "Minorities", "Politicians")

################ Data cleaning
df[, liste.thermo][df[,liste.thermo] > 100] <- NA

df$first_lang[df$first_lang > 5] <- 6
df$first_lang <- factor(df$first_lang, levels = c(1,5,6),
                        labels = c("English", "French", "Other"))

################ Compute means and SDs

t.thermo.mu <- aggregate(df[liste.thermo], by = df["first_lang"], 
                         FUN = mean,  na.rm=TRUE)
colnames(t.thermo.mu) <- var.names

t.thermo.sd <- aggregate(df[liste.thermo], by = df["first_lang"], 
                         FUN = sd,  na.rm=TRUE)
colnames(t.thermo.sd) <- var.names

t.thermo.n <- aggregate(df[liste.thermo], by = df["first_lang"], 
                        function(x) length(which(!is.na(x))))
colnames(t.thermo.n) <- var.names

################ Data prep for ggplot
library(reshape2)
t.thermo.mu.l <- melt(t.thermo.mu, id.vars = "Language")
colnames(t.thermo.mu.l) <- c("Language", "Target", "Mean")

t.thermo.sd.l <- melt(t.thermo.sd, id.vars = "Language")
colnames(t.thermo.sd.l) <- c("Language", "Target", "sd")

t.thermo.n.l <- melt(t.thermo.n, id.vars = "Language")
colnames(t.thermo.n.l) <- c("Language", "Target", "n")

# Merge les 3
t.thermo <- merge(t.thermo.mu.l, t.thermo.sd.l,  by = c("Language", "Target"))
t.thermo <- merge(t.thermo, t.thermo.n.l,  by = c("Language", "Target"))

# CI's
t.thermo$ll <- t.thermo$Mean - (1.96*(t.thermo$sd/sqrt(t.thermo$n))) 
t.thermo$ul <- t.thermo$Mean + (1.96*(t.thermo$sd/sqrt(t.thermo$n))) 

library(ggplot2)
library(RColorBrewer)

# Ordre du plot
t.thermo$ordre <- rank(t.thermo$Mean)

# With ggstance::geom_pointrangeh
ggplot(t.thermo, aes(y = reorder(Target, ordre), x = Mean, color = Language)) + 
  ggstance::geom_pointrangeh(aes(xmin = ll, xmax = ul), 
                             position = position_dodge(width = 0.4), 
                             shape=1) + # dodge not working
  ylab("Target Group") +
  xlab("Average Thermometer Rating") +
 # ylim(45,100) +
  scale_color_brewer("Respondents' Language", palette = "Set1") +
  labs(title = "Average Thermometer Rating for Various Target Groups by Respondent's Mother Tongue",
       subtitle = "Data: Canadian Election Study 2015") +
  theme_minimal()
ggsave("thermo.jpg", width = 10, height = 5)

######################### Figure delta ingroup

########## Compute delta ingroup
t.thermo$detal_ingroup <- ifelse(
  t.thermo$Language == "English", t.thermo$Mean - t.thermo[2, 3], ifelse(
  t.thermo$Language == "French", t.thermo$Mean - t.thermo[16, 3], ifelse(
  t.thermo$Language == "Other", t.thermo$Mean - t.thermo[30, 3],  NA)
  ))

t.thermo$detal_ingroup.ll <- ifelse(
  t.thermo$Language == "English", t.thermo$ll - t.thermo[2, 3], ifelse(
  t.thermo$Language == "French", t.thermo$ll - t.thermo[16, 3], ifelse(
  t.thermo$Language == "Other", t.thermo$ll - t.thermo[30, 3],  NA)
  ))

t.thermo$detal_ingroup.ul <- ifelse(
  t.thermo$Language == "English", t.thermo$ul - t.thermo[2, 3], ifelse(
  t.thermo$Language == "French", t.thermo$ul - t.thermo[16, 3], ifelse(
  t.thermo$Language == "Other", t.thermo$ul - t.thermo[30, 3],  NA)
  ))

# Figure
ggplot(t.thermo, aes(y = reorder(Target, ordre), 
                     x = detal_ingroup, 
                     color = Language)) + 
  ggstance::geom_pointrangeh(aes(xmin = detal_ingroup.ll, 
                                 xmax = detal_ingroup.ul), 
                             position = position_dodge(width = 0.4), 
                             shape=1) + # dodge not working
  ylab("Target Group") +
  xlab("Difference Between Average Themometer Rating and Ingroup") +
 # ylim(-20, 20) +
  scale_color_brewer("Respondents' Language", palette = "Set1") +
  labs(title = "Difference Between Average Themometer Rating and Ingroup for Various Target Groups by Respondent's Mother Tongue",
       subtitle = "Data: Canadian Election Study 2015 \n
       In groups are respectively Anglophones, Francophones and 
       Minorities for English, French and Others as mother tongue") +
  geom_vline(xintercept = 0, colour = "red", lty = 2) +
  theme_minimal()
ggsave("delta_ingroup.jpg", width = 10, height = 5)



