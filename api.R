library(plumber)
library(randomForest)
library(xgboost)

# Load models once at startup (not per request)
rf_del    <- readRDS("models/powerbi_rf_arrdel15.rds")
rf_can    <- readRDS("models/powerbi_rf_cancelled.rds")
xgb_del   <- xgb.load("models/powerbi_xgb_arrdel15.json")
xgb_can   <- xgb.load("models/powerbi_xgb_cancelled.json")
del_map   <- readRDS("models/powerbi_del_level_map.rds")
can_map   <- readRDS("models/powerbi_can_level_map.rds")

# Helper: collapse rare factor levels to "OTHER" (mirrors your reduce_levels)
recode_level <- function(val, allowed_levels) {
  if (val %in% allowed_levels) val else "OTHER"
}

#* @apiTitle Flight Delay & Cancellation Predictor
#* @apiDescription Predicts delay and cancellation probability using RF and XGBoost

#* Enable CORS so your frontend can call this
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* Predict delay probability
#* @post /predict/delay
#* @param Airline        Airline name (string)
#* @param Origin         Origin airport code (string)
#* @param Dest           Destination airport code (string)
#* @param CRSDepTime     Scheduled departure time HHMM (integer)
#* @param CRSElapsedTime Scheduled flight duration in minutes (integer)
#* @param Distance       Flight distance in miles (integer)
#* @param Month          Month 1-12 (integer)
#* @param DayOfWeek      Day of week 1=Mon 7=Sun (integer)
#* @param DepDel15       Whether departure was delayed 15+ min (0 or 1)
function(Airline, Origin, Dest, CRSDepTime, CRSElapsedTime,
         Distance, Month, DayOfWeek, DepDel15) {
  
  DepHour   <- floor(as.integer(CRSDepTime) / 100)
  IsWeekend <- ifelse(as.integer(DayOfWeek) %in% c(6, 7), 1, 0)
  CRSDepTime_int <- as.integer(CRSDepTime)
  TimeOfDay <- cut(CRSDepTime_int,
                   breaks = c(0, 600, 1200, 1800, 2400),
                   labels = c("Night", "Morning", "Afternoon", "Evening"),
                   include.lowest = TRUE)
  
  # Recode to known levels
  Airline_r <- recode_level(Airline, del_map$Airline)
  Origin_r  <- recode_level(Origin,  del_map$Origin)
  Dest_r    <- recode_level(Dest,    del_map$Dest)
  
  row <- data.frame(
    Airline          = factor(Airline_r,          levels = del_map$Airline),
    Origin           = factor(Origin_r,           levels = del_map$Origin),
    Dest             = factor(Dest_r,             levels = del_map$Dest),
    DepHour          = DepHour,
    TimeOfDay        = factor(as.character(TimeOfDay),
                              levels = c("Night","Morning","Afternoon","Evening")),
    DayOfWeek        = as.integer(DayOfWeek),
    Month            = as.integer(Month),
    DepDel15         = factor(as.integer(DepDel15), levels = c(0, 1)),
    Distance         = as.numeric(Distance),
    CRSElapsedTime   = as.numeric(CRSElapsedTime),
    AirlineDelayRate = 0.22,   # population mean fallback
    OriginDelayRate  = 0.22,
    RouteDelayRate   = 0.22,
    TrafficVolume    = 50,
    IsWeekend        = IsWeekend,
    stringsAsFactors = FALSE
  )
  
  # RF prediction
  rf_prob  <- tryCatch(
    predict(rf_del, newdata = row, type = "prob")[, "1"],
    error = function(e) NA
  )
  
  # XGBoost prediction
  row_enc  <- row
  for (col in names(row_enc)) {
    if (is.factor(row_enc[[col]])) row_enc[[col]] <- as.integer(row_enc[[col]])
  }
  xgb_mat  <- xgb.DMatrix(as.matrix(row_enc))
  xgb_prob <- tryCatch(
    predict(xgb_del, xgb_mat),
    error = function(e) NA
  )
  
  list(
    rf_delay_prob  = round(as.numeric(rf_prob),  4),
    xgb_delay_prob = round(as.numeric(xgb_prob), 4),
    input_echo     = list(
      airline = Airline, origin = Origin, dest = Dest,
      month = Month, day_of_week = DayOfWeek
    )
  )
}

#* Predict cancellation probability
#* @post /predict/cancel
function(Airline, Origin, Dest, CRSDepTime, CRSArrTime,
         CRSElapsedTime, Distance, Month, DayofMonth,
         DayOfWeek, DistanceGroup) {
  
  DepHour   <- floor(as.integer(CRSDepTime) / 100)
  IsWeekend <- ifelse(as.integer(DayOfWeek) %in% c(6, 7), 1, 0)
  CRSDepTime_int <- as.integer(CRSDepTime)
  TimeOfDay <- cut(CRSDepTime_int,
                   breaks = c(0, 600, 1200, 1800, 2400),
                   labels = c("Night", "Morning", "Afternoon", "Evening"),
                   include.lowest = TRUE)
  top20 <- c("ATL","ORD","DFW","DEN","CLT","LAX","LAS","PHX","MCO","SEA",
             "EWR","SFO","BOS","MSP","DTW","JFK","MIA","PHL","FLL","BWI")
  HighTrafficOrigin <- ifelse(Origin %in% top20, 1, 0)
  
  Airline_r <- recode_level(Airline, can_map$Airline)
  Origin_r  <- recode_level(Origin,  can_map$Origin)
  Dest_r    <- recode_level(Dest,    can_map$Dest)
  
  row <- data.frame(
    Airline           = factor(Airline_r,   levels = can_map$Airline),
    Origin            = factor(Origin_r,    levels = can_map$Origin),
    Dest              = factor(Dest_r,      levels = can_map$Dest),
    CRSDepTime        = as.numeric(CRSDepTime),
    CRSArrTime        = as.numeric(CRSArrTime),
    CRSElapsedTime    = as.numeric(CRSElapsedTime),
    Distance          = as.numeric(Distance),
    Month             = as.integer(Month),
    DayofMonth        = as.integer(DayofMonth),
    DayOfWeek         = as.integer(DayOfWeek),
    Operating_Airline = factor(Airline_r, levels = can_map$Operating_Airline),
    DistanceGroup     = as.integer(DistanceGroup),
    DepHour           = DepHour,
    IsWeekend         = IsWeekend,
    TimeOfDay         = factor(as.character(TimeOfDay),
                               levels = c("Night","Morning","Afternoon","Evening")),
    HighTrafficOrigin = HighTrafficOrigin,
    stringsAsFactors  = FALSE
  )
  
  rf_prob  <- tryCatch(
    predict(rf_can, newdata = row, type = "prob")[, "1"],
    error = function(e) NA
  )
  
  row_enc <- row
  for (col in names(row_enc)) {
    if (is.factor(row_enc[[col]])) row_enc[[col]] <- as.integer(row_enc[[col]])
  }
  xgb_mat  <- xgb.DMatrix(as.matrix(row_enc))
  xgb_prob <- tryCatch(
    predict(xgb_can, xgb_mat),
    error = function(e) NA
  )
  
  list(
    rf_cancel_prob  = round(as.numeric(rf_prob),  4),
    xgb_cancel_prob = round(as.numeric(xgb_prob), 4)
  )
}