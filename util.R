get_properties <- function(
    watlas,
    x='X_raw',
    y='Y_raw'
) {
  # speed
  watlas$speed_in <-
    atl_get_speed(
      data = watlas,
      time = "TIME",
      type = "in",
      X = x,
      Y = y
    )
  watlas$speed_out <-
    atl_get_speed(
      data = watlas,
      time = "TIME",
      type = "out",
      X = x,
      Y = y
    )
  # turning angle
  watlas$angle <- 
    atl_turning_angle(
      data = watlas,
      time = "TIME",
      X = x,
      Y = y)
  # create delta time feature, which is the time difference in seconds
  # basically, how many seconds passed since the last localisation
  watlas$dtime <- sapply(
    1:nrow(watlas),
    FUN = function(i) {
      if (i == 1) return(NA)
      timediff <- as.numeric(
        difftime(
          watlas[i, "time"], 
          watlas[i - 1, "time"],
          units="secs")
        )
      return(timediff)
    }
  )
  # step length
  watlas_sf <- st_as_sf(watlas, coords=c(x, y), crs=st_crs(32631))
  distance <- st_distance(head(watlas_sf, -1), tail(watlas_sf, -1), by_element=T)
  watlas$step_length <- c(NA, distance)
  return(watlas)
}
