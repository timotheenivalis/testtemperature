#' Fahrenheit to Kelvin conversion
#'
#' This function takes a temperature expressed in Fahrenheit and convert it to Kelvin.
#'
#' @param temp A temperature expressed in Fahrenheit
#'
#' @return A temperature in Kelvin
#'
#' @examples
#' fahr_to_kelvin(32)
#'
#' @export
fahr_to_kelvin <- function(temp) {
  stopifnot(is.numeric(temp))
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#' Kelvin to Celsius conversion
#'
#' This function takes a temperature expressed in Kelvin and convert it to Celsius.
#'
#' @param temp A temperature expressed in Kelvin
#'
#' @return A temperature in Celsius.
#'
#' @examples
#' kelvin_to_celsius(300)
#'
#' @export
kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}

#' Celsius to Kelvin conversion
#'
#' This function takes a temperature expressed in Celsius and convert it to Kelvin.
#'
#' @param temp A temperature expressed in Celsius
#'
#' @return A temperature in Kelvin
#'
#' @examples
#' kelvin_to_celsius(300)
#'
#' @export
celsius_to_kelvin <- function(temp) {
  kelvin <- temp + 273.15
  return(kelvin)
}


#' Wide to long conversion for standard monthly temperature
#'
#' This function takes a standard monthly temperature dataframe in wide format to long format.
#'
#' @param dat A temperature dataframe
#' @param id.vars Character string. vector of id variables. Can be integer (variable position) or string (variable name). If blank, will use all non-measured variables.
#' @param variable.name Character string. Nname of variable used to store measured variable names
#' @param value.name Character string. Name of variable used to store values
#'
#' @return A dataframe
#'
#' @examples
#' data("tuggeranong")
#' Wide_To_Long_Temperature(tuggeranong)
#'
#' @export
Wide_To_Long_Temperature <- function(dat, id.vars="Year",
                                     variable.name = "Month",
                                     value.name = "Temperature"){
  stopifnot(expr={is.data.frame(dat)
    id.vars %in% colnames(dat)
  })
  melttemp <- reshape2::melt(dat, id.vars = id.vars,
                   variable.name = variable.name,
                   value.name = value.name)
  return(melttemp)
}

#' Plot temperature trends
#'
#' Plot temperature trends by month and estimate the average change in temperature across years.
#'
#' @param dat A temperature dataframe
#'
#' @return A graphic
#'
#' @examples
#' data("tuggeranong")
#' plot_trend(tuggeranong)
#'
#' @export
plot_trend <- function(dat){
  melttemp <- Wide_To_Long_Temperature(dat)

  model <- summary(stats::lm(Temperature ~ Year + Month, data = melttemp))
  year_effect <- round(model$coefficients["Year",],3)

  ggplot2::ggplot(data=melttemp,
                  ggplot2::aes(x=melttemp$Year,y=melttemp$Temperature, color=melttemp$Month))+
    ggplot2::geom_point() + ggplot2::geom_smooth(alpha=0.2) +
    ggplot2::annotate("label", x = mean(melttemp$Year),
             y = max(melttemp$Temperature),
             label = paste0(ifelse(year_effect[1]>=0, "+", "-"),
                            year_effect[1], "C/year, p=",year_effect[4]))
}
