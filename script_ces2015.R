temp <- tempfile()
download.file("https://ces-eec.sites.olt.ubc.ca/files/2017/04/CES2015_Combined_R.zip",temp)
load(unz(temp, "CES2015_Combined_R.RData"))
d <- CES2015_Combined
rm(CES2015_Combined)

liste.thermo <- c("p_like_can","p_like_fran","p_like_angl", "p_like_abor", "p_like_femi", "p_like_immg",
                  "p_like_us", "p_like_qc", "p_like_gays", "p_like_mino", "p_like_polit")

d[, liste.thermo][d[,liste.thermo] > 100] <- NA

d$first_lang[d$first_lang > 5] <- 6
d$first_lang <- factor(d$first_lang, levels = c(1,5,6),
                       labels = c("English", "French", "Other"))

t.thermo <- aggregate(d[liste.thermo], by = d["first_lang"], FUN = mean,  na.rm=TRUE)

t.thermo.mu <- aggregate(d[liste.thermo], by = d["first_lang"], FUN = mean,  na.rm=TRUE)
colnames(t.thermo.mu) <- c("Language", "Canada", "Francophones", "Anglophones", "Aboriginals", 
                           "Feminists", "Immigrants", "The US", "Quebec", "Gays & Lesbians", 
                           "Minorities", "Politicians")

t.thermo.sd <- aggregate(d[liste.thermo], by = d["first_lang"], FUN = sd,  na.rm=TRUE)
colnames(t.thermo.sd) <- c("Language", "Canada", "Francophones", "Anglophones", "Aboriginals", 
                           "Feminists", "Immigrants", "The US", "Quebec", "Gays & Lesbians", 
                           "Minorities", "Politicians")

t.thermo.n <- aggregate(d[liste.thermo], by = d["first_lang"], function(x) length(which(!is.na(x))))
colnames(t.thermo.n) <- c("Language", "Canada", "Francophones", "Anglophones", "Aboriginals", 
                           "Feminists", "Immigrants", "The US", "Quebec", "Gays & Lesbians", 
                           "Minorities", "Politicians")


library(reshape2)
t.thermo.mu.l <- melt(t.thermo.mu, id.vars = "Language")
colnames(t.thermo.mu.l) <- c("Language", "Target", "Mean")

t.thermo.sd.l <- melt(t.thermo.sd, id.vars = "Language")
colnames(t.thermo.sd.l) <- c("Language", "Target", "sd")

t.thermo.n.l <- melt(t.thermo.n, id.vars = "Language")
colnames(t.thermo.n.l) <- c("Language", "Target", "n")



t.thermo <- merge(t.thermo.mu.l, t.thermo.sd.l,  by = c("Language", "Target"))
t.thermo <- merge(t.thermo, t.thermo.n.l,  by = c("Language", "Target"))

t.thermo$ll <- t.thermo$Mean - (1.96*(t.thermo$sd/sqrt(t.thermo$n))) # fonctionne pas encore
t.thermo$ul <- t.thermo$Mean + (1.96*(t.thermo$sd/sqrt(t.thermo$n))) # fonctionne pas encore

library(ggplot2)
library(RColorBrewer)

t.thermo$ordre <- rank(t.thermo$Mean)

ggplot(t.thermo, aes(x= reorder(Target, ordre), y=Mean, color=Language)) +
  geom_pointrange(aes(ymin=ll, ymax=ul), position=position_dodge(.3), shape=1) +
#  geom_vline(xintercept = tapply(pred.dat$fit, pred.dat$Language, mean, na.rm=T)[1], linetype="dotted", size=1) +
  xlab("Target Group") +
  ylab("Average Thermometer Rating") +
  ylim(45,100) +
  scale_color_brewer("Respondents' Language", palette = "Set1") +
  coord_flip() +
  labs(title = "Average Thermometer Rating for Various Target Groups by Respondent's Mother Tongue",
       subtitle = "Data: Canadian Election Study 2015") +
  theme_minimal()

###############################################################################################


#colnames(t.thermo) <- c("Language", "Canada", "Francophones", "Anglophones", "Aboriginals")

d2 <- d[,c(liste.thermo, "first_lang", "id")]

colnames(d2) <- c("Canada", "Francophones", "Anglophones", "Aboriginals", "Feminists", "Immigrants",
                  "The US", "Quebec", "Gays & Lesbians", "Minorities", "Politicians", "first_lang", "id" )



d2.l <- melt(d2, id.vars = c("first_lang", "id"))
colnames(d2.l) <- c("Language", "id", "Group", "Thermometer" )

m1 <- lm(Thermometer ~ Language*Group, d2.l)
summary(m1)

pred.dat <- expand.grid(Language=c("English", "French", "Other"),
                        Group=unique(d2.l$Group))

pred.dat <- cbind(pred.dat,
                  predict(m1, newdata = pred.dat, interval = "confidence"))

# Ordonnancer les trucs...
ordre <- aggregate(pred.dat[, 3], list(pred.dat$Group), mean)
ordre$order <- rank(ordre$x)
ordre <- rep(ordre$order, each=3) 
pred.dat$ordre <- ordre


tapply(pred.dat$fit, pred.dat$Language, mean, na.rm=T)

library(ggplot2)
library(RColorBrewer)

ggplot(pred.dat, aes(x= reorder(Group, ordre), y=fit, color=Language)) +
  geom_pointrange(aes(ymin=lwr, ymax=upr), position=position_dodge(.3), shape=1) +
  geom_vline(xintercept = tapply(pred.dat$fit, pred.dat$Language, mean, na.rm=T)[1], linetype="dotted", size=1) +
  xlab("Target Group") +
  ylab("Average Thermometer Rating") +
  ylim(45,100) +
  scale_color_brewer("Respondents' Language", palette = "Set1") +
  coord_flip() +
  labs(title = "Average Thermometer Rating for Various Target Groups by Respondent's Mother Tongue",
       subtitle = "Data: Canadian Election Study 2015") +
  theme_minimal()