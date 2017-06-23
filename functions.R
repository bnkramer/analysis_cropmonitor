auc = function(x, y, from = min(x), to = max(x), type = c("linear","spline"), absolutearea = FALSE, ...){
    # auc from the MESS package
    type <- match.arg(type)
    if (length(x) != length(y)) 
        stop("x and y must have the same length")
    if (length(unique(x)) < 2) 
        return(NA)
    if (type == "linear") {
        if (absolutearea) 
            y <- y - min(y)
        values <- approx(x, y, xout = sort(unique(c(from, to, 
            x[x > from & x < to]))), ...)
        res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
        if (absolutearea) 
            res <- res - min(y) * (max(x) - min(x))
    }
    else {
        if (absolutearea) 
            myfunction <- function(x) {
                abs(splinefun(x, y, method = "natural"))
            }
        else myfunction <- splinefun(x, y, method = "natural")
        res <- integrate(myfunction, lower = from, upper = to)$value
    }
    res
}
