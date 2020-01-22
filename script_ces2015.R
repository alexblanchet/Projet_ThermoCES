temp <- tempfile()
download.file("https://ces-eec.sites.olt.ubc.ca/files/2017/04/CES2015_Combined_R.zip",temp)
load(unz(temp, "CES2015_Combined_R.RData"))
d <- CES2015_Combined
rm(CES2015_Combined)

d$p_like_abor[d$p_like_abor > 100] <- NA
d$p_like_angl[d$p_like_angl > 100] <- NA
d$p_like_fran[d$p_like_fran > 100] <- NA
d$p_like_can[d$p_like_can > 100] <- NA
d$p_like_immg[d$p_like_immg > 100] <- NA

d$p_like_femi[d$p_like_femi > 100] <- NA
d$p_like_us[d$p_like_us > 100] <- NA
d$p_like_qc[d$p_like_qc > 100] <- NA
d$p_like_gays[d$p_like_gays > 100] <- NA
d$p_like_mino[d$p_like_mino > 100] <- NA
d$p_like_polit[d$p_like_polit > 100] <- NA


d$first_lang[d$first_lang > 5] <- 6
d$first_lang <- factor(d$first_lang, levels = c(1,5,6),
                       labels = c("English", "French", "Other"))


t.thermo <- aggregate(d[c("p_like_can","p_like_fran","p_like_angl", "p_like_abor", "p_like_femi", "p_like_immg",
                          "p_like_us", "p_like_qc", "p_like_gays", "p_like_mino", "p_like_polit")], 
                      by = d["first_lang"], FUN=mean,  na.rm=TRUE)
#colnames(t.thermo) <- c("Language", "Canada", "Francophones", "Anglophones", "Aboriginals")



d2 <- d[,c("p_like_can","p_like_fran","p_like_angl", "p_like_abor", "p_like_femi", "p_like_immg",
           "p_like_us", "p_like_qc", "p_like_gays", "p_like_mino", "p_like_polit", "first_lang", "id")]

colnames(d2) <- c("Canada", "Francophones", "Anglophones", "Aboriginals", "Feminists", "Immigrants",
                  "The US", "Quebec", "Gays & Lesbians", "Minorities", "Politicians", "first_lang", "id" )

library(reshape2)

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