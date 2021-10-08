
interp_cum_deaths <- function(dated_df) {

df <- data.frame(x = seq(nrow(dated_df)), y = cumsum(dated_df$excess_deaths_mean))

## Set up the size of the basis functions/number of knots
k <- round(nrow(dated_df)/2)

## This fits the unconstrained model but gets us smoothness parameters that
## that we will need later
unc <- mgcv::gam(y ~ s(x, k = k, bs = "cr"), data = df)

## This creates the cubic spline basis functions of `x`
## It returns an object containing the penalty matrix for the spline
## among other things; see ?smooth.construct for description of each
## element in the returned object
sm <- mgcv::smoothCon(mgcv::s(x, k = k, bs = "cr"), df, knots = NULL)[[1]]

## This gets the constraint matrix and constraint vector that imposes
## linear constraints to enforce montonicity on a cubic regression spline
## the key thing you need to change is `up`.
## `up = TRUE` == increasing function
## `up = FALSE` == decreasing function (as per your example)
## `xp` is a vector of knot locations that we get back from smoothCon
CF <- mgcv::mono.con(sm$xp, up = TRUE)   # get constraints: up = FALSE == Decreasing con

## Fill in G, the object pcsl needs to fit; this is just what `pcls` says it needs:
## X is the model matrix (of the basis functions)
## C is the identifiability constraints - no constraints needed here
##   for the single smooth
## sp are the smoothness parameters from the unconstrained GAM
## p/xp are the knot locations again, but negated for a decreasing function
## y is the response data
## w are weights and this is fancy code for a vector of 1s of length(y)
G <- list(X = sm$X, C = matrix(0,0,0), sp = unc$sp,
          p = sm$xp, # note the - here! This is for decreasing fits!
          y = df$y,
          w = df$y*0+1)
G$Ain <- CF$A    # the monotonicity constraint matrix
G$bin <- CF$b    # the monotonicity constraint vector, both from mono.con
G$S <- sm$S     # the penalty matrix for the cubic spline
G$off <- 0      # location of offsets in the penalty matrix

## Do the constrained fit
p <- mgcv::pcls(G)  # fit spline (using s.p. from unconstrained fit)

## predict at 100 locations over range of x - get a smooth line on the plot
newx <- with(df, data.frame(x = df$x))
fv <- mgcv::Predict.matrix(sm, newx) %*% p
newx <- transform(newx, yhat = fv[,1])

out_df <- data.frame(date = dated_df$date,
                     deaths = c(0, diff(newx$yhat)))

return(out_df)

}
