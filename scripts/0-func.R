library("data.table")
#library(leaflet)
library("Rcpp")

#give names to walks
makeWalkName <- function(walkPtId) {
    walkName <- paste0("WALK|", walkPtId)
    return(walkName)
}

# Eliminate jumps in the data with low angle and instant in time
# Currently not just eliminating first too quick point.
makeLoRandomJump <- function(dt) {
    dt[, dist := haverDistVector(lon, lat), by=userId]
    dt[, vel := dist/(time-shift(time))]
    # Finds first point in series going faster than 200. 
    loIsFast <- dt[, (shift(vel) < 200) & (vel >= 200)]

    dt[, angle := findAngVector(lon, lat, dist)]
    loIsAngle <- dt[, (vel > 5) & (time - shift(time) < 300) &
                    ((angle < .25*pi)| is.nan(angle))]
    ret <- loIsFast | loIsAngle
    ret[is.na(ret)] <- FALSE
    return(ret)
}

# Remove those jumps
RemoveJumps <- function(dt) {
    for(i in c(1:3)) {
        loIsRemoveRow <- makeLoRandomJump(dt)
        dt <- dt[!(loIsRemoveRow),]
    }
    return(dt)
}


makeLoIsStill <- function(dt) {
    ret1 <- c(rep(FALSE, dt[,.N]))
    ret <- makeLoIsStillHelper(ret1, dt[, accuracy], dt[, lon], dt[, lat],
                               factor(dt[, userId]))
    return(ret)
}

# Assumes that entries are sorted by user then time, ascending.  
makeLoIsNewWalk <- function(dt, minSpeed, maxSpeed) {
    dt[, (vel < minSpeed) | (vel > maxSpeed)]
    return(ret)
}
