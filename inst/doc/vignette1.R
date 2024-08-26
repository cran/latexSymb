## -----------------------------------------------------------------------------
library(latexSymb)
vf1 <- lsymb("V")
vf2 <- lsymb("W")

## -----------------------------------------------------------------------------
inner <- function(x,y) ang(lsymb(x, ",", y))
cov.der <- function(x) lsymb("D", x)/"dt"
ddt <- function(x) lsymb("d", x)/"dt"

## -----------------------------------------------------------------------------
lenv("equation", 
     lsymb(
           ddt(inner(vf1, vf2)), 
           "=", 
           inner(cov.der(vf1), vf2) + inner(vf1, cov.der(vf2))
     )
)

## -----------------------------------------------------------------------------
lenv("align*", 
     c(
       lsymb(ddt(inner(vf1, vf2)), "&=\\\\"),
       lsymb("&=", inner(cov.der(vf1), vf2) + inner(vf1, cov.der(vf2)))
     )
)

