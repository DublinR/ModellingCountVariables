require(ggplot2)

dat <- read.csv("negbin.csv")

head(dat)
#############################################################

dat <- within(dat, {
    prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
    id <- factor(id)
})

summary(dat)


##############################################################

ggplot(dat, aes(daysabs, fill = prog)) 
	+ geom_histogram(binwidth = 1) 
	+ facet_grid(prog ~ ., margins = TRUE, scales = "free")

##############################################################

with(dat, tapply(daysabs, prog, function(x) {
    sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

##############################################################

library(MASS)
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))

##############################################################

m2 <- update(m1, . ~ . - prog)
anova(m1, m2)


m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat)
pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)


(est <- cbind(Estimate = coef(m1), confint(m1)))


exp(est)


newdata1 <- data.frame(math = mean(dat$math), prog = factor(1:3, levels = 1:3, 
    labels = levels(dat$prog)))
newdata1$phat <- predict(m1, newdata1, type = "response")
newdata1


newdata2 <- data.frame(
  math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
  levels(dat$prog)))

newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(math, DaysAbsent)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")
