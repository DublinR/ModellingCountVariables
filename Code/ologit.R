dat <- read.csv("ologit.csv")
head(dat)


## one at a time, table apply, pared, and public
lapply(dat[, c("apply", "pared", "public")], table)

## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ public + apply + pared, data = dat))
 
##                        pared   0   1
## public apply                        
## 0      unlikely              175  14
##        somewhat likely        98  26
##        very likely            20  10
## 1      unlikely               25   6
##        somewhat likely        12   4
##        very likely             7   3
 
summary(dat$gpa)


ggplot(dat, aes(x = apply, y = gpa)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(pared ~ public, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


###########################################################

## fit ordered logit model and store results 'm'
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)

## view a summary of the model
summary(m)


## store table
(ctable <- coef(summary(m)))

 
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))


(ci <- confint(m)) # default method gives profiled CIs
 

 
confint.default(m) # CIs assuming normality


## odds ratios
exp(coef(m))
 
##  pared public    gpa 
## 2.8511 0.9429 1.8514
 
## OR and CI
exp(cbind(OR = coef(m), ci))



sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dat, summary(as.numeric(apply) ~ pared + public + gpa, fun=sf)))

glm(I(as.numeric(apply) >= 2) ~ pared, family="binomial", data = dat)
 

 
glm(I(as.numeric(apply) >= 3) ~ pared, family="binomial", data = dat)


lnewdat <- melt(newdat, id.vars = c("pared", "public", "gpa"),
  variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat)
 
##   pared public   gpa    Level Probability
## 1     0      0 1.900 unlikely      0.7376
## 2     1      0 1.921 unlikely      0.4932
## 3     0      0 1.942 unlikely      0.7325
## 4     1      0 1.964 unlikely      0.4867
## 5     0      0 1.985 unlikely      0.7274
## 6     1      0 2.006 unlikely      0.4802
 
ggplot(lnewdat, aes(x = gpa, y = Probability, colour = Level)) +
  geom_line() +
  facet_grid(pared ~ public, scales="free",
    labeller=function(x, y) sprintf("%s = %d", x, y))


