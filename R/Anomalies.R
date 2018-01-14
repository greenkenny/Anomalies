#' @name AnomaliesAll
#' @param x Time series as a two column data frame where the first column consists of the
#' timestamps and the second column consists of the observations.
#' @param max_anoms Maximum number of anomalies that algorithm will detect as a percentage of the
#' data.
#' @param direction Directionality of the anomalies to be detected. Options are:
#' \code{'pos' | 'neg' | 'both'}.
#' @param alpha The level of statistical significance with which to accept or reject anomalies.
#' @param only_last Find and report anomalies only within the last day or hr in the time series.
#' \code{NULL | 'day' | 'hr'}.
#' @param threshold Only report positive going anoms above the threshold specified. Options are:
#' \code{'None' | 'med_max' | 'p95' | 'p99'}.
#' @param e_value Add an additional column to the anoms output containing the expected value.
#' @param longterm Increase anom detection efficacy for time series that are greater than a month.
#' See Details below.
#' @param piecewise_median_period_weeks The piecewise median time window as described in Vallis, Hochenbaum, and Kejariwal (2014).
#' Defaults to 2.
#' @param plot A flag indicating if a plot with both the time series and the estimated anoms,
#' indicated by circles, should also be returned.
#' @param y_log Apply log scaling to the y-axis. This helps with viewing plots that have extremely
#' large positive anomalies relative to the rest of the data.
#' @param xlabel X-axis label to be added to the output plot.
#' @param ylabel Y-axis label to be added to the output plot.
#' @param title Title for the output plot.
#' @param verbose Enable debug messages.
#' @param na.rm Remove any NAs in timestamps.(default: FALSE)
#' @return The returned value is a list with the following components.
#' @return \item{anoms}{Data frame containing timestamps, values, and optionally expected values.}
#' @return \item{plot}{A graphical object if plotting was requested by the user. The plot contains
#' the estimated anomalies annotated on the input time series.}
#' @return One can save \code{anoms} to a file in the following fashion:
#' \code{write.csv(<return list name>[["anoms"]], file=<filename>)}
#' @return One can save \code{plot} to a file in the following fashion:
#' \code{ggsave(<filename>, plot=<return list name>[["plot"]])}
#'
#' @examples
#' data(raw_data)
#' AnomaliesAll(raw_data, max_anoms=0.02, direction='both', plot=TRUE)
#' # To detect only the anomalies on the last day, run the following:
#' AnomaliesAll(raw_data, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
#' @seealso \code{\link{AnomaliesVec}}
#' @export
#'
AnomaliesAll <- function(x, max_anoms = 0.10, direction = 'pos',
                               alpha = 0.05, only_last = NULL, threshold = 'None',
                               e_value = FALSE, longterm = FALSE, piecewise_median_period_weeks = 2, plot = FALSE,
                               y_log = FALSE, xlabel = '', ylabel = 'count',
                               title = NULL, verbose=FALSE, na.rm = FALSE){

  if(!is.data.frame(x)){
    stop("data must be a single data frame.")
  } else {
    if(ncol(x) != 2 || !is.numeric(x[[2]])){
      stop("data must be a 2 column data.frame, with the first column being a set of timestamps, and the second coloumn being numeric values.")
    }
    if (!(class(x[[1]])[1] == "POSIXlt")) {
      x <- format_timestamp(x)
    }
  }
  if (any((names(x) == c("timestamp", "count")) == FALSE)) {
    colnames(x) <- c("timestamp", "count")
  }

  if(!is.logical(na.rm)){
    stop("na.rm must be either TRUE (T) or FALSE (F)")
  }

  if(any(is.na(x$timestamp))){
    if(na.rm){
      x <- x[-which(is.na(x$timestamp)), ]
    } else {
      stop("timestamp contains NAs, please set na.rm to TRUE or remove the NAs manually.")
    }
  }

  if(max_anoms > .49){
    stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x[[2]]), 0), " data_points =", length(x[[2]]),")."))
  } else if(max_anoms < 0){
    stop("max_anoms must be positive.")
  } else if(max_anoms == 0){
    warning("0 max_anoms results in max_outliers being 0.")
  }
  if(!direction %in% c('pos', 'neg', 'both')){
    stop("direction options are: pos | neg | both.")
  }
  if(!(0.01 <= alpha || alpha <= 0.1)){
    if(verbose) message("Warning: alpha is the statistical signifigance, and is usually between 0.01 and 0.1")
  }
  if(!is.null(only_last) && !only_last %in% c('day','hr')){
    stop("only_last must be either 'day' or 'hr'")
  }
  if(!threshold %in% c('None','med_max','p95','p99')){
    stop("threshold options are: None | med_max | p95 | p99.")
  }
  if(!is.logical(e_value)){
    stop("e_value must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(longterm)){
    stop("longterm must be either TRUE (T) or FALSE (F)")
  }
  if(piecewise_median_period_weeks < 2){
    stop("piecewise_median_period_weeks must be at greater than 2 weeks")
  }
  if(!is.logical(plot)){
    stop("plot must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(y_log)){
    stop("y_log must be either TRUE (T) or FALSE (F)")
  }
  if(!is.character(xlabel)){
    stop("xlabel must be a string")
  }
  if(!is.character(ylabel)){
    stop("ylabel must be a string")
  }
  if(!is.character(title) && !is.null(title)){
    stop("title must be a string")
  }
  if(is.null(title)){
    title <- ""
  } else {
    title <- paste(title, " : ", sep="")
  }

  gran <- get_gran(x, 1)

  if(gran == "day"){
    num_days_per_line <- 7
    if(is.character(only_last) &&  only_last == 'hr'){
      only_last <- 'day'
    }
  } else {
    num_days_per_line <- 1
  }

  if(gran == "sec"){
    x <- format_timestamp(aggregate(x[2], format(x[1], "%Y-%m-%d %H:%M:00"), eval(parse(text="sum"))))
  }

  period = switch(gran,
                  min = 1440,
                  hr = 24,
                  # if the data is daily, then we need to bump the period to weekly to get multiple examples
                  day = 7)
  num_obs <- length(x[[2]])

  if(max_anoms < 1/num_obs){
    max_anoms <- 1/num_obs
  }

  if(longterm){
    if(gran == "day"){
      num_obs_in_period <- period*piecewise_median_period_weeks + 1
      num_days_in_period <- (7*piecewise_median_period_weeks) + 1
    } else {
      num_obs_in_period <- period*7*piecewise_median_period_weeks
      num_days_in_period <- (7*piecewise_median_period_weeks)
    }

    last_date <- x[[1]][num_obs]

    all_data <- vector(mode="list", length=ceiling(length(x[[1]])/(num_obs_in_period)))
    for(j in seq(1,length(x[[1]]), by=num_obs_in_period)){
      start_date <- x[[1]][j]
      end_date <- min(start_date + lubridate::days(num_days_in_period), x[[1]][length(x[[1]])])
      if(difftime(end_date, start_date, units = "days") == as.difftime(num_days_in_period, units="days")){
        all_data[[ceiling(j/(num_obs_in_period))]] <- subset(x, x[[1]] >= start_date & x[[1]] < end_date)
      }else{
        all_data[[ceiling(j/(num_obs_in_period))]] <- subset(x, x[[1]] > (last_date-lubridate::days(num_days_in_period)) & x[[1]] <= last_date)
      }
    }
  }else{
    all_data <- list(x)
  }

  all_anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
  seasonal_plus_trend <- data.frame(timestamp=numeric(0), count=numeric(0))

  for(i in 1:length(all_data)) {

    anomaly_direction = switch(direction,
                               "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
                               "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
                               "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.

    s_h_esd_timestamps <- detect_anoms(all_data[[i]], k=max_anoms, alpha=alpha, num_obs_per_period=period, use_decomp=TRUE, use_esd=FALSE,
                                       one_tail=anomaly_direction$one_tail, upper_tail=anomaly_direction$upper_tail, verbose=verbose)

    data_decomp <- s_h_esd_timestamps$stl
    s_h_esd_timestamps <- s_h_esd_timestamps$anoms

    if(!is.null(s_h_esd_timestamps)){
      anoms <- subset(all_data[[i]], (all_data[[i]][[1]] %in% s_h_esd_timestamps))
    } else {
      anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
    }

    if(threshold != "None"){
      periodic_maxs <- tapply(x[[2]],as.Date(x[[1]]),FUN=max)

      if(threshold == 'med_max'){
        thresh <- median(periodic_maxs)
      }else if (threshold == 'p95'){
        thresh <- quantile(periodic_maxs, .95)
      }else if (threshold == 'p99'){
        thresh <- quantile(periodic_maxs, .99)
      }
      anoms <- subset(anoms, anoms[[2]] >= thresh)
    }
    all_anoms <- rbind(all_anoms, anoms)
    seasonal_plus_trend <- rbind(seasonal_plus_trend, data_decomp)
  }

  all_anoms <- all_anoms[!duplicated(all_anoms[[1]]), ]
  seasonal_plus_trend <- seasonal_plus_trend[!duplicated(seasonal_plus_trend[[1]]), ]

  if(!is.null(only_last)){
    start_date <- x[[1]][num_obs]-lubridate::days(7)
    start_anoms <- x[[1]][num_obs]-lubridate::days(1)
    if(gran == "day"){
      breaks <- 3*12
      num_days_per_line <- 7
    } else {
      if(only_last == 'day'){
        breaks <- 12
      }else{
        start_date <- lubridate::floor_date(x[[1]][num_obs]-lubridate::days(2), "day")
        start_anoms <- x[[1]][num_obs]-lubridate::hours(1)
        breaks <- 3
      }
    }

    x_subset_single_day <- subset(x, (x[[1]] > start_anoms))
    x_subset_week <- subset(x, ((x[[1]] <= start_anoms) & (x[[1]] > start_date)))
    all_anoms <- subset(all_anoms, all_anoms[[1]] >= x_subset_single_day[[1]][1])
    num_obs <- length(x_subset_single_day[[2]])
  }

  anom_pct <- (length(all_anoms[[2]]) / num_obs) * 100

  if(anom_pct == 0){
    if(verbose) message("No anomalies detected.")
    return (list("anoms"=data.frame(), "plot"=plot.new()))
  }

  if(plot){
    plot_title <-  paste(title, round(anom_pct, digits=2), "% Anomalies (alpha=", alpha, ", direction=", direction,")", sep="")
    if(longterm){
      plot_title <- paste(plot_title, ", longterm=T", sep="")
    }

    color_name <- paste("\"", title, "\"", sep="")
    alpha <- 0.8
    if(!is.null(only_last)){
      xgraph <- ggplot2::ggplot(x_subset_week, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x_subset_week, ggplot2::aes_string(colour=color_name), alpha=alpha*.33) + ggplot2::geom_line(data=x_subset_single_day, ggplot2::aes_string(color=color_name), alpha=alpha)
      week_rng = get_range(x_subset_week, index=2, y_log=y_log)
      day_rng = get_range(x_subset_single_day, index=2, y_log=y_log)
      yrange = c(min(week_rng[1],day_rng[1]), max(week_rng[2],day_rng[2]))
      xgraph <- add_day_labels_datetime(xgraph, breaks=breaks, start=as.POSIXlt(min(x_subset_week[[1]]), tz="UTC"), end=as.POSIXlt(max(x_subset_single_day[[1]]), tz="UTC"), days_per_line=num_days_per_line)
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }else{
      xgraph <- ggplot2::ggplot(x, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "gray60"), panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x, ggplot2::aes_string(colour=color_name), alpha=alpha)
      yrange <- get_range(x, index=2, y_log=y_log)
      xgraph <- xgraph + ggplot2::scale_x_datetime(labels=function(x) ifelse(as.POSIXlt(x, tz="UTC")$hour != 0,strftime(x, format="%kh", tz="UTC"), strftime(x, format="%b %e", tz="UTC")),
                                                  expand=c(0,0))
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }

    xgraph <- xgraph + ggplot2::geom_point(data=all_anoms, ggplot2::aes_string(color=paste("\"zzz_",title,"\"",sep="")), size = 3, shape = 1)

    xgraph <- xgraph + ggplot2::theme(legend.position="none")

    xgraph <- xgraph + add_formatted_y(yrange, y_log=y_log)

  }

  all_anoms[[1]] <- format(all_anoms[[1]], format="%Y-%m-%d %H:%M:%S")

  if(e_value) {
    anoms <- data.frame(timestamp=all_anoms[[1]], anoms=all_anoms[[2]],
                        expected_value=subset(seasonal_plus_trend[[2]], as.POSIXlt(seasonal_plus_trend[[1]], tz="UTC") %in% all_anoms[[1]]),
                        stringsAsFactors=FALSE)
  } else {
    anoms <- data.frame(timestamp=all_anoms[[1]], anoms=all_anoms[[2]], stringsAsFactors=FALSE)
  }

  anoms$timestamp <- as.POSIXlt(anoms$timestamp, tz="UTC")

  if(plot){
    return (list(anoms = anoms, plot = xgraph))
  } else {
    return (list(anoms = anoms, plot = plot.new()))
  }
}

#' @name AnomaliesPart
#' @param x Time series as a column data frame, list, or vector, where the column consists of
#' the observations.
#' @param max_anoms Maximum number of anomalies that algorithm will detect as a percentage of the
#' data.
#' @param direction Directionality of the anomalies to be detected. Options are:
#' \code{'pos' | 'neg' | 'both'}.
#' @param alpha The level of statistical significance with which to accept or reject anomalies.
#' @param period Defines the number of observations in a single period, and used during seasonal
#' decomposition.
#' @param only_last Find and report anomalies only within the last period in the time series.
#' @param threshold Only report positive going anoms above the threshold specified. Options are:
#' \code{'None' | 'med_max' | 'p95' | 'p99'}.
#' @param e_value Add an additional column to the anoms output containing the expected value.
#' @param longterm_period Defines the number of observations for which the trend can be considered
#' flat. The value should be an integer multiple of the number of observations in a single period.
#' This increases anom detection efficacy for time series that are greater than a month.
#' @param plot A flag indicating if a plot with both the time series and the estimated anoms,
#' indicated by circles, should also be returned.
#' @param y_log Apply log scaling to the y-axis. This helps with viewing plots that have extremely
#' large positive anomalies relative to the rest of the data.
#' @param xlabel X-axis label to be added to the output plot.
#' @param ylabel Y-axis label to be added to the output plot.
#' @param title Title for the output plot.
#' @param verbose Enable debug messages
#' @return The returned value is a list with the following components.
#' @return \item{anoms}{Data frame containing index, values, and optionally expected values.}
#' @return \item{plot}{A graphical object if plotting was requested by the user. The plot contains
#' the estimated anomalies annotated on the input time series.}
#' @return One can save \code{anoms} to a file in the following fashion:
#' \code{write.csv(<return list name>[["anoms"]], file=<filename>)}
#' @return One can save \code{plot} to a file in the following fashion:
#' \code{ggsave(<filename>, plot=<return list name>[["plot"]])}
#' @examples
#' data(raw_data)
#' AnomaliesPart(raw_data[,2], max_anoms=0.02, period=1440, direction='both', plot=TRUE)
#' # To detect only the anomalies in the last period, run the following:
#' AnomaliesPart(raw_data[,2], max_anoms=0.02, period=1440, direction='both',
#' only_last=TRUE, plot=TRUE)
#' @seealso \code{\link{AnomaliesAll}}
#' @export
AnomaliesPart = function(x, max_anoms=0.10, direction='pos',
                               alpha=0.05, period=NULL, only_last=F,
                               threshold='None', e_value=F, longterm_period=NULL,
                               plot=F, y_log=F, xlabel='', ylabel='count',
                               title=NULL, verbose=FALSE){

  if(is.data.frame(x) && ncol(x) == 1 && is.numeric(x[[1]])){
    x <- data.frame(timestamp=c(1:length(x[[1]])), count=x[[1]])
  } else if(is.vector(x) || is.list(x) && is.numeric(x)) {
    x <- data.frame(timestamp=c(1:length(x)), count=x)
  } else {
    stop("data must be a single data frame, list, or vector that holds numeric values.")
  }

  if(max_anoms > .49){
    stop(paste("max_anoms must be less than 50% of the data points (max_anoms =", round(max_anoms*length(x[[2]]), 0), " data_points =", length(x[[2]]),")."))
  }
  if(!direction %in% c('pos', 'neg', 'both')){
    stop("direction options are: pos | neg | both.")
  }
  if(!(0.01 <= alpha || alpha <= 0.1)){
    if(verbose) message("Warning: alpha is the statistical signifigance, and is usually between 0.01 and 0.1")
  }
  if(is.null(period)){
    stop("Period must be set to the number of data points in a single period")
  }
  if(!is.logical(only_last)){
    stop("only_last must be either TRUE (T) or FALSE (F)")
  }
  if(!threshold %in% c('None', 'med_max', 'p95', 'p99')){
    stop("threshold options are: None | med_max | p95 | p99.")
  }
  if(!is.logical(e_value)){
    stop("e_value must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(plot)){
    stop("plot must be either TRUE (T) or FALSE (F)")
  }
  if(!is.logical(y_log)){
    stop("y_log must be either TRUE (T) or FALSE (F)")
  }
  if(!is.character(xlabel)){
    stop("xlabel must be a string")
  }
  if(!is.character(ylabel)){
    stop("ylabel must be a string")
  }
  if(!is.character(title) && !is.null(title)){
    stop("title must be a string")
  }
  if(is.null(title)){
    title <- ""
  } else {
    title <- paste(title, " : ", sep="")
  }

  num_obs <- length(x[[2]])

  if(max_anoms < 1/num_obs){
    max_anoms <- 1/num_obs
  }

  if(!is.null(longterm_period)){
    all_data <- vector(mode="list", length=ceiling(length(x[[1]])/(longterm_period)))
    for(j in seq(1,length(x[[1]]), by=longterm_period)){
      start_index <- x[[1]][j]
      end_index <- min((start_index + longterm_period - 1), num_obs)
      if((end_index - start_index + 1) == longterm_period){
        all_data[[ceiling(j/(longterm_period))]] <- subset(x, x[[1]] >= start_index & x[[1]] <= end_index)
      }else{
        all_data[[ceiling(j/(longterm_period))]] <- subset(x, x[[1]] > (num_obs-longterm_period) & x[[1]] <= num_obs)
      }
    }
  }else{
    all_data <- list(x)
  }

  all_anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
  seasonal_plus_trend <- data.frame(timestamp=numeric(0), count=numeric(0))

  for(i in 1:length(all_data)) {

    anomaly_direction = switch(direction,
                               "pos" = data.frame(one_tail=TRUE, upper_tail=TRUE), # upper-tail only (positive going anomalies)
                               "neg" = data.frame(one_tail=TRUE, upper_tail=FALSE), # lower-tail only (negative going anomalies)
                               "both" = data.frame(one_tail=FALSE, upper_tail=TRUE)) # Both tails. Tail direction is not actually used.

    s_h_esd_timestamps <- detect_anoms(all_data[[i]], k=max_anoms, alpha=alpha, num_obs_per_period=period, use_decomp=TRUE, use_esd=FALSE,
                                       one_tail=anomaly_direction$one_tail, upper_tail=anomaly_direction$upper_tail, verbose=verbose)

    data_decomp <- s_h_esd_timestamps$stl
    s_h_esd_timestamps <- s_h_esd_timestamps$anoms

    if(!is.null(s_h_esd_timestamps)){
      anoms <- subset(all_data[[i]], (all_data[[i]][[1]] %in% s_h_esd_timestamps))
    } else {
      anoms <- data.frame(timestamp=numeric(0), count=numeric(0))
    }

    if(threshold != "None"){
      if(!is.null(longterm_period)){
        periodic_maxs <- tapply(all_data[[i]][[2]], c(0:(longterm_period-1))%/%period, FUN=max)
      }else{
        periodic_maxs <- tapply(all_data[[i]][[2]], c(0:(num_obs-1))%/%period, FUN=max)
      }

      if(threshold == 'med_max'){
        thresh <- median(periodic_maxs)
      }else if (threshold == 'p95'){
        thresh <- quantile(periodic_maxs, .95)
      }else if (threshold == 'p99'){
        thresh <- quantile(periodic_maxs, .99)
      }
      anoms <- subset(anoms, anoms[[2]] >= thresh)
    }
    all_anoms <- rbind(all_anoms, anoms)
    seasonal_plus_trend <- rbind(seasonal_plus_trend, data_decomp)
  }

  all_anoms <- all_anoms[!duplicated(all_anoms[[1]]), ]
  seasonal_plus_trend <- seasonal_plus_trend[!duplicated(seasonal_plus_trend[[1]]), ]

  if(only_last){
    x_subset_single_period <- data.frame(timestamp=x[[1]][(num_obs-period+1):num_obs], count=x[[2]][(num_obs-period+1):num_obs])
    past_obs <- period*7
    if(num_obs < past_obs){
      past_obs <- num_obs-period
    }

    x_subset_previous <- data.frame(timestamp=x[[1]][(num_obs-past_obs+1):(num_obs-period+1)], count=x[[2]][(num_obs-past_obs+1):(num_obs-period+1)])

    all_anoms <- subset(all_anoms, all_anoms[[1]] >= x_subset_single_period[[1]][1])
    num_obs <- length(x_subset_single_period[[2]])
  }

  anom_pct <- (length(all_anoms[[2]]) / num_obs) * 100

  if(anom_pct == 0){
    if(verbose) message("No anomalies detected.")
    return (list("anoms"=data.frame(), "plot"=plot.new()))
  }

  if(plot){
    plot_title <-  paste(title, round(anom_pct, digits=2), "% Anomalies (alpha=", alpha, ", direction=", direction,")", sep="")
    if(!is.null(longterm_period)){
      plot_title <- paste(plot_title, ", longterm=T", sep="")
    }

    color_name <- paste("\"", title, "\"", sep="")
    alpha <- 0.8
    if(only_last){
      all_data <- rbind(x_subset_previous, x_subset_single_period)
      lines_at <- seq(1, length(all_data[[2]]), period)+min(all_data[[1]])
      xgraph <- ggplot2::ggplot(all_data, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x_subset_previous, ggplot2::aes_string(colour=color_name), alpha=alpha*.33) + ggplot2::geom_line(data=x_subset_single_period, ggplot2::aes_string(color=color_name), alpha=alpha)
      yrange <- get_range(all_data, index=2, y_log=y_log)
      xgraph <- xgraph + ggplot2::scale_x_continuous(breaks=lines_at, expand=c(0,0))
      xgraph <- xgraph + ggplot2::geom_vline(xintercept=lines_at, color="gray60")
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }else{
      num_periods <- num_obs/period
      lines_at <- seq(1, num_obs, period)

      inc <- 2
      while(num_periods > 14){
        num_periods <- num_obs/(period*inc)
        lines_at <- seq(1, num_obs, period*inc)
        inc <- inc + 1
      }
      xgraph <- ggplot2::ggplot(x, ggplot2::aes_string(x="timestamp", y="count")) + ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), text=ggplot2::element_text(size = 14))
      xgraph <- xgraph + ggplot2::geom_line(data=x, ggplot2::aes_string(colour=color_name), alpha=alpha)
      yrange <- get_range(x, index=2, y_log=y_log)
      xgraph <- xgraph + ggplot2::scale_x_continuous(breaks=lines_at, expand=c(0,0))
      xgraph <- xgraph + ggplot2::geom_vline(xintercept=lines_at, color="gray60")
      xgraph <- xgraph + ggplot2::labs(x=xlabel, y=ylabel, title=plot_title)
    }

    xgraph <- xgraph + ggplot2::geom_point(data=all_anoms, ggplot2::aes_string(color=paste("\"zzz_",title,"\"",sep="")), size = 3, shape = 1)

    xgraph <- xgraph + ggplot2::theme(axis.text.x=ggplot2::element_blank()) + ggplot2::theme(legend.position="none")

    xgraph <- xgraph + add_formatted_y(yrange, y_log=y_log)
  }

  if(e_value) {
    anoms <- data.frame(index=all_anoms[[1]], anoms=all_anoms[[2]], expected_value=subset(seasonal_plus_trend[[2]], seasonal_plus_trend[[1]] %in% all_anoms[[1]]))
  } else {
    anoms <- data.frame(index=all_anoms[[1]], anoms=all_anoms[[2]])
  }

  if(plot){
    return (list(anoms = anoms, plot = xgraph))
  } else {
    return (list(anoms = anoms, plot = plot.new()))
  }
}

get_range <- function(dfs, index = 2, y_log = F) {
  vals <- dfs[[index]]
  if(y_log) vals <- vals[vals > 0]
  vrange = range(vals, na.rm=TRUE)
  vmin = vrange[1]
  vmax = vrange[2]
  return(c(vmin, vmax))
}

add_formatted_y <- function(yrange, y_log = FALSE, expand = TRUE, digits = 1) {
  ymin <- yrange[1]
  ymax <- yrange[2]

  if (expand) {
    ymax <- ymax + (ymax - ymin) * .2
  }

  if(abs(ymax) > 1000000) {
    divisor <- 1000000
    unit <- "M"
  }
  else if(abs(ymax) > 1000) {
    divisor <- 1000
    unit <- "k"
  }
  else {
    divisor <- 1
    unit <- ""
  }

  if(y_log){
    transform = "log10"
  }
  else {
    transform = "identity"
  }

  return (ggplot2::scale_y_continuous(breaks=seq(ymin, ymax, length.out=6), limits=c(ymin, ymax), labels=function(x) paste(round(x/divisor, digits=digits),unit,sep=""), trans = transform))

}

add_day_labels_datetime <- function(tsplot, breaks = 6, start = NULL, end = NULL,
                                    days_per_line = 1) {
  if (is.null(start)) {
    start <- min(tsplot$data[[1]])
  }

  if (is.null(end)) {
    end <- max(tsplot$data[[1]])
  }

  start_breaks <- start
  attributes(start_breaks)$tzone <- "UTC"

  lines_start <- trunc.POSIXt(start_breaks, units = "days")
  attributes(lines_start)$tzone <- "UTC"

  lines_at <- seq(lines_start, end, as.difftime(days_per_line, units = "days"))
  lines_at <- lines_at[lines_at > start & lines_at < end]

  minor_breaks <- seq(trunc.POSIXt(start_breaks, units="days"), end,
                      as.difftime(breaks, units = "hours"))
  minor_breaks <- minor_breaks[minor_breaks > start & minor_breaks <= end]

  if (start$min == 0) {
    minor_breaks <- as.POSIXct(c(start, minor_breaks))
  }

  outplot <- tsplot + ggplot2::scale_x_datetime(breaks = minor_breaks,
                                                labels = function(x) ifelse(as.POSIXlt(x, tz = "UTC")$hour != 0,strftime(x, format="%kh", tz="UTC"), strftime(x, format="%b %e", tz="UTC")),
                                                expand = c(0, 0))

  if (length(lines_at) > 0) {
    outplot <- outplot + ggplot2::geom_vline(xintercept = as.numeric(lines_at), color = "gray60")
  }

  return(outplot)
}

detect_anoms <- function(data, k = 0.49, alpha = 0.05, num_obs_per_period = NULL,
                         use_decomp = TRUE, use_esd = FALSE, one_tail = TRUE,
                         upper_tail = TRUE, verbose = FALSE) {
  # Args:
  #	 data: Time series to perform anomaly detection on.
  #	 k: Maximum number of anomalies that ESD will detect as a percentage of the data.
  #	 alpha: The level of statistical significance with which to accept or reject anomalies.
  #	 num_obs_per_period: Defines the number of observations in a single period, and used during seasonal decomposition.
  #	 use_decomp: Use seasonal decomposition during anomaly detection.
  #	 one_tail: If TRUE only positive or negative going anomalies are detected depending on if upper_tail is TRUE or FALSE.
  #	 upper_tail: If TRUE and one_tail is also TRUE, detect only positive going (right-tailed) anomalies. If FALSE and one_tail is TRUE, only detect negative (left-tailed) anomalies.
  #	 verbose: Additionally printing for debugging.
  # Returns:
  #   A list containing the anomalies (anoms) and decomposition components (stl).

  if(is.null(num_obs_per_period)){
    stop("must supply period length for time series decomposition")
  }

  num_obs <- nrow(data)

  if(num_obs < num_obs_per_period * 2){
    stop("Anom detection needs at least 2 periods worth of data")
  }

  posix_timestamp <- if (class(data[[1L]])[1L] == "POSIXlt") TRUE else FALSE

  if (length(rle(is.na(c(NA,data[[2L]],NA)))$values)>3){
    stop("Data contains non-leading NAs. We suggest replacing NAs with interpolated values (see na.approx in Zoo package).")
  } else {
    data <- na.omit(data)
  }

  data_decomp <- stl(ts(data[[2L]], frequency = num_obs_per_period),
                     s.window = "periodic", robust = TRUE)

  data <- data.frame(timestamp = data[[1L]], count = (data[[2L]]-data_decomp$time.series[,"seasonal"]-median(data[[2L]])))

  data_decomp <- data.frame(timestamp=data[[1L]], count=(as.numeric(trunc(data_decomp$time.series[,"trend"]+data_decomp$time.series[,"seasonal"]))))

  if(posix_timestamp){
    data_decomp <- format_timestamp(data_decomp)
  }
  max_outliers <- trunc(num_obs*k)

  if(max_outliers == 0){
    stop(paste0("With longterm=TRUE, AnomalyDetection splits the data into 2 week periods by default. You have ", num_obs, " observations in a period, which is too few. Set a higher piecewise_median_period_weeks."))
  }

  func_ma <- match.fun(median)
  func_sigma <- match.fun(mad)

  n <- length(data[[2L]])
  if (posix_timestamp){
    R_idx <- as.POSIXlt(data[[1L]][1L:max_outliers], tz = "UTC")
  } else {
    R_idx <- 1L:max_outliers
  }

  num_anoms <- 0L

  for (i in 1L:max_outliers){
    if(verbose) message(paste(i,"/", max_outliers,"completed"))

    if(one_tail){
      if(upper_tail){
        ares <- data[[2L]] - func_ma(data[[2L]])
      } else {
        ares <- func_ma(data[[2L]]) - data[[2L]]
      }
    } else {
      ares = abs(data[[2L]] - func_ma(data[[2L]]))
    }

    data_sigma <- func_sigma(data[[2L]])
    if(data_sigma == 0)
      break

    ares <- ares/data_sigma
    R <- max(ares)

    temp_max_idx <- which(ares == R)[1L]

    R_idx[i] <- data[[1L]][temp_max_idx]

    data <- data[-which(data[[1L]] == R_idx[i]), ]

    if(one_tail){
      p <- 1 - alpha/(n-i+1)
    } else {
      p <- 1 - alpha/(2*(n-i+1))
    }

    t <- qt(p,(n-i-1L))
    lam <- t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))

    if(R > lam)
      num_anoms <- i
  }

  if(num_anoms > 0) {
    R_idx <- R_idx[1L:num_anoms]
  } else {
    R_idx = NULL
  }

  return(list(anoms = R_idx, stl = data_decomp))
}

format_timestamp <- function(indf, index = 1) {
  if (class(indf[[index]])[1] == "POSIXlt") {
    return(indf)
  }
  if (stringr::str_detect(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%Y-%m-%d %H:%M:%S", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%Y-%m-%d %H:%M", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{2}/\\d{2}/\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%m/%d/%y", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{2}/\\d{2}/\\d{4}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%m/%d/%Y", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{4}\\d{2}\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%Y%m%d", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{4}/\\d{2}/\\d{2}/\\d{2}$")) {
    indf[[index]] <- strptime(indf[[index]], format="%Y/%m/%d/%H", tz="UTC")
  }
  else if (stringr::str_detect(indf[[index]][1], "^\\d{10}$")) {
    # Handle Unix seconds in milliseconds
    indf[[index]] <- as.POSIXlt(indf[[index]], origin="1970-01-01", tz="UTC")
  }

  return(indf)
}

get_gran = function(tsdf, index=1) {
  n = length(tsdf[[index]])
  # We calculate the granularity from the time difference between the last 2 entries (sorted)
  gran = round(difftime(max(tsdf[[index]]), sort(tsdf[[index]], partial = n-1)[n-1],
                        units = "secs"))

  if (gran >= 86400) {
    return("day")
  }
  else if (gran >= 3600) {
    return("hr")
  }
  else if (gran >= 60) {
    return("min")
  }
  else if (gran >= 1) {
    return("sec")
  }
  else {
    return("ms")
  }
}
