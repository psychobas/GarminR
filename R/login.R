#' Log in to garmin connect
#'
#' @param username character, your Garmin Connect username (usuallly email address)
#' @param password character, your Garmin Connect password
#' @param start_date character, date from which on monitoring data should be collected
#' @param end_date character, date by which monitoring data should be collected
#' @param number_of_activities integer, number of activities to download
#' @param verbose boolean, display progress bar? (default = TRUE)
#' @param path character, where to create the databases to store the data (default = current working directory)
#'
#' @return
#' @export
#'
#' @examples
get_garmin_data <- function(username, password, start_date, end_date, number_of_activities, verbose = TRUE, path = NULL) {

  if (missing(username))
    stop("Username not provided")

  if (missing(password))
    stop("Username not password")

  if (missing(start_date))
    stop("Start date not provided")

  if (missing(end_date))
    stop("End date not provided")

  if (missing(number_of_activities))
    stop("Number of activities to get not provided")

  if (number_of_activities < 1)
    stop("Number of activities to get must be >= 1")

  if (as.Date(end_date) - as.Date(start_date) < 0)
    stop("end_date must be a later date than start_date")


  if (is.null(path)) {
    path <- getwd()
  }


  date_seq <- function(start_date_user, end_date_user) {
    today <- as.POSIXlt(Sys.Date())
    today$mday <- 1
    max_start_date <- as.Date(today) - 365
    if (start_date_user < max_start_date & max_start_date <= end_date_user) {
      return(seq(as.Date(max_start_date), as.Date(end_date_user), by = "day"))
    } else if (start_date_user > max_start_date) {
      return(seq(as.Date(start_date_user), as.Date(end_date_user), by = "day"))
    } else {
      message("Both start- and end date are more than one year in the past, cannot get detailed data (only daily summary data) for movement, heart rate, stress, body battery and respiration.")
      return(start_date_user)
    }
  }

  dates_max <- date_seq(start_date, end_date)





  #close all DB connections (cleanup if error occurred)
  on.exit(try(DBI::dbDisconnect(activities_db), silent = TRUE), add = TRUE)
  on.exit(try(DBI::dbDisconnect(summary_db), silent = TRUE), add = TRUE)
  on.exit(try(DBI::dbDisconnect(summary_db_details), silent = TRUE), add = TRUE)



  #connect_sso_login URL
  connect_sso_login <- "https://sso.garmin.com/sso/signin"

  #get_headers
  get_headers <- c(Referer = "https://connect.garmin.com/en-US/signin")

  #params
  params <- list(
    'service'                           = "https://connect.garmin.com/modern",
    'webhost'                           = "https://connect.garmin.com",
    'source'                            = "https://connect.garmin.com/en-US/signin",
    'redirectAfterAccountLoginUrl'      = "https://connect.garmin.com/modern",
    'redirectAfterAccountCreationUrl'   = "https://connect.garmin.com/modern",
    'gauthHost'                         = "https://sso.garmin.com/sso",
    'locale'                            = 'en_US',
    'id'                                = 'gauth-widget',
    'cssUrl'                            = "https://static.garmincdn.com/com.garmin.connect/ui/css/gauth-custom-v1.2-min.css",
    'privacyStatementUrl'               = '//connect.garmin.com/en-US/privacy/', #"https://www.garmin.com/de-DE/privacy/",#
    'clientId'                          = 'GarminConnect',
    'rememberMeShown'                   = 'true',
    'rememberMeChecked'                 = 'false',
    'createAccountShown'                = 'true',
    'openCreateAccount'                 = 'false',
    'displayNameShown'                  = 'false',
    'consumeServiceTicket'              = 'false',
    'initialFocus'                      = 'true',
    'embedWidget'                       = 'false',
    'generateExtraServiceTicket'        = 'true',
    'generateTwoExtraServiceTickets'    = 'false',
    'generateNoServiceTicket'           = 'false',
    'globalOptInShown'                  = 'true',
    'globalOptInChecked'                = 'false',
    'mobile'                            = 'false',
    'connectLegalTerms'                 = 'true',
    'locationPromptShown'               = 'true',
    'showPassword'                      = 'true',
    #I added these
    'showTermsOfUse'                    = 'false',
    'showPrivacyPolicy'                 = 'false',
    'showConnectLegalAge'               = 'false',
    'useCustomHeader'                   = 'false'
  )

  #specify common handle to use between requests -> cookie sharing
  hand <- httr::handle("a")


  #make first request to get csrf needed to log in
  r <- httr::GET(connect_sso_login, httr::add_headers(get_headers), query = params, handle = hand)
  r_content <- httr::content(r)
  r_content_char <- as.character(r_content)
  csrf <- regmatches(r_content_char,
                     regexpr('[A-Z0-9]{100}', r_content_char, perl = TRUE))



  #POST Login
  a <- httr::VERB(verb = "POST", url = "https://sso.garmin.com/sso/signin",
                  httr::add_headers(authority = "sso.garmin.com", `cache-control` = "max-age=0",
                                    `upgrade-insecure-requests` = "1", origin = "https://sso.garmin.com",
                                    `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.121 Safari/537.36 OPR/71.0.3770.198",
                                    accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
                                    `sec-fetch-site` = "same-origin", `sec-fetch-mode` = "navigate",
                                    `sec-fetch-user` = "?1", `sec-fetch-dest` = "iframe",
                                    referer = "https://sso.garmin.com/sso/signin?service=https%3A%2F%2Fconnect.garmin.com%2Fmodern%2F&webhost=https%3A%2F%2Fconnect.garmin.com%2Fmodern%2F&source=https%3A%2F%2Fconnect.garmin.com%2Fsignin%2F&redirectAfterAccountLoginUrl=https%3A%2F%2Fconnect.garmin.com%2Fmodern%2F&redirectAfterAccountCreationUrl=https%3A%2F%2Fconnect.garmin.com%2Fmodern%2F&gauthHost=https%3A%2F%2Fsso.garmin.com%2Fsso&locale=de_DE&id=gauth-widget&cssUrl=https%3A%2F%2Fconnect.garmin.com%2Fgauth-custom-v1.2-min.css&privacyStatementUrl=https%3A%2F%2Fwww.garmin.com%2Fde-DE%2Fprivacy%2Fconnect%2F&clientId=GarminConnect&rememberMeShown=true&rememberMeChecked=false&createAccountShown=true&openCreateAccount=false&displayNameShown=false&consumeServiceTicket=false&initialFocus=true&embedWidget=false&generateExtraServiceTicket=true&generateTwoExtraServiceTickets=false&generateNoServiceTicket=false&globalOptInShown=true&globalOptInChecked=false&mobile=false&connectLegalTerms=true&showTermsOfUse=false&showPrivacyPolicy=false&showConnectLegalAge=false&locationPromptShown=true&showPassword=true&useCustomHeader=false",
                                    `accept-language` = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7"),
                  # httr::set_cookies(SESSION = "4e9ebda3-ae1a-45a4-be61-ba91dee294b5",
                  #                   CASTGC = "TGT-2143569-WEtcg3Kfm5dCCPezdXblSEC5WYjNHAiAza69QR7wJjS27fZudb-cas",
                  #                   GarminUserPrefs = "de-DE", `_ga` = "GA1.2.1473667366.1600269350",
                  #                   notice_behavior = "implied,eu", `__cfduid` = "d70a229f3d134061c4d7ac584807d222e1600269352",
                  #                   `_gid` = "GA1.2.562781702.1602010910", `__cflb` = "02DiuHkH2SZrbLnjiuZcrrhAPFnPkNcRvATFjjGojzxw6",
                  #                   `__cfruid` = "8cfd582e9d5c6c76d4eb085a1b6631b467b2eb75-1602010916",
                  #                   org.springframework.web.servlet.i18n.CookieLocaleResolver.LOCALE = "de_DE",
                  #                   `GARMIN-SSO` = "1", GarminNoCache = "true",
                  #                   `GARMIN-SSO-GUID` = "2E26024F94705977CF0283BB86E7B3D6A668BE5D",
                  #                   `GARMIN-SSO-CUST-GUID` = "9f000e82-a5c0-4bc4-a051-398c495e580f",
                  #                   `_gat_gprod` = "1", utag_main = "v_id:0174977c7317004fed112b1026d804085005707d007e8$_sn:12$_ss:0$_st:1602093515684$ses_id:1602091051727%3Bexp-session$_pn:12%3Bexp-session"),
                  body = list(`username` = username,
                              `password` = password,
                              `embed` = "false",
                              `_csrf` = csrf),
                  encode = "form", query = list(service = "https://connect.garmin.com/modern/",
                                                webhost = "https://connect.garmin.com/modern/",
                                                source = "https://connect.garmin.com/signin/",
                                                redirectAfterAccountLoginUrl = "https://connect.garmin.com/modern/",
                                                redirectAfterAccountCreationUrl = "https://connect.garmin.com/modern/",
                                                gauthHost = "https://sso.garmin.com/sso", locale = "de_DE",
                                                id = "gauth-widget", cssUrl = "https://connect.garmin.com/gauth-custom-v1.2-min.css",
                                                privacyStatementUrl = "https://www.garmin.com/de-DE/privacy/connect/",
                                                clientId = "GarminConnect", rememberMeShown = "true",
                                                rememberMeChecked = "false", createAccountShown = "true",
                                                openCreateAccount = "false", displayNameShown = "false",
                                                consumeServiceTicket = "false", initialFocus = "true",
                                                embedWidget = "false", generateExtraServiceTicket = "true",
                                                generateTwoExtraServiceTickets = "false", generateNoServiceTicket = "false",
                                                globalOptInShown = "true", globalOptInChecked = "false",
                                                mobile = "false", connectLegalTerms = "true",
                                                showTermsOfUse = "false", showPrivacyPolicy = "false",
                                                showConnectLegalAge = "false", locationPromptShown = "true",
                                                showPassword = "true", useCustomHeader = "false"),
                  handle = hand)

  if (httr::status_code(a) != 200)
    stop(paste0("Login failed with status code ", httr::status_code(a)))

  #get ticket from response
  resp_char <- as.character(a)
  ticket <- regmatches(resp_char,
                       regexpr( "ticket=.*", resp_char, perl = TRUE))
  ticket <- gsub('\\";', "", gsub('ticket=', "", ticket))


  #get SessionID
  ticket_url <- paste0("https://connect.garmin.com/modern/?ticket=", ticket)

  Sys.sleep(3)

  session_id <- httr::GET(ticket_url, handle = httr::handle("new_handle"))

  session_headers <- session_id$all_headers
  sessionid <- unlist(session_headers)[grepl("SESSIONID", unlist(session_headers))]
  names(sessionid) <- NULL
  sessionid <- substr(sessionid, 1, 46)


  #use SessionID in headers
  headers = c(
    `authority` = 'connect.garmin.com',
    `cache-control` = 'max-age=0',
    `upgrade-insecure-requests` = '10',
    `user-agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.183 Safari/537.36 OPR/72.0.3815.320',
    `accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
    `sec-fetch-site` = 'none',
    `sec-fetch-mode` = 'navigate',
    `sec-fetch-user` = '?1',
    `sec-fetch-dest` = 'document',
    `accept-language` = 'de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7',
    `cookie` = sessionid
  )

  Sys.sleep(3)

  #get displayname
  disp_name <- httr::GET(url = "http://connect.garmin.com/modern", httr::add_headers(.headers=headers), handle = hand)
  disp_resp <- as.character(httr::content(disp_name))
  disp_json <- regmatches(disp_resp,
                          regexpr('\\("{\"*.*\\"}"\\);', disp_resp, perl = TRUE))
  disp_json <- regmatches(disp_resp,
                          regexpr('{\"*.*\\"}', disp_resp, perl = TRUE))
  disp_new <- gsub('\\\\\"', '"', disp_json)
  # as of 2020-12-28 this no longer works without the paste... used to work without..
  display_preferences <- jsonlite::fromJSON(paste0(disp_new, "}"))
  display_name <- display_preferences$displayName



  ###################
  ###################  get activities
  ###################

  if (verbose) {print(paste0("Getting ", number_of_activities, " activities"))}


  act_params = list(
    start = 0,
    limit = number_of_activities
  )

  #could also use this to get activities one by one: #uses this url: https://connect.garmin.com/modern/proxy/activity-service/activity/5816010582

  act <- httr::GET(url = 'http://connect.garmin.com/modern/proxy/activitylist-service/activities/search/activities', httr::add_headers(.headers=headers), handle = hand, query = act_params)
  activity_list <- httr::content(act, as = "text")
  #convert to df
  act_df <- jsonlite::fromJSON(activity_list, flatten = TRUE)

  activities_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(path, "/activities.sqlite"))

  #delete lists
  ind <- lapply(act_df, function(x) class(x) != "list")
  act_df <- act_df[,unlist(ind)]
  if (verbose & nrow(act_df) < number_of_activities) {print(paste0("There are only ", nrow(act_df), " recorded activities."))}
  DBI::dbWriteTable(activities_db, "activities_summary", act_df, overwrite = TRUE)






  ###################
  ###################  get daily summary
  ###################



  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  if (verbose) pb = utils::txtProgressBar(min = 0, max = length(dates), initial = 0, style = 3)
  if (verbose) {print(paste0("Getting daily summaries for ", length(dates), " days"))}

  url <- paste0('https://connect.garmin.com/modern/proxy/usersummary-service/usersummary/daily/',display_name)

  daily_summary_df <- data.frame()
  for (date in 1:length(dates)) {

    params = list(
      `calendarDate` = dates[date]
    )


    daily_summary <- httr::GET(url = url, handle = httr::handle("act"), query = params, httr::add_headers(.headers=headers))

    daily_summary_content <- httr::content(daily_summary)
    daily_summary_df_day <- data.frame(lapply(unlist(daily_summary_content), utils::type.convert), stringsAsFactors=FALSE)
    #print(dim(daily_summary_df))
    # fill in non-overlapping columns with NAs
    if (date != 1) {
      daily_summary_df_day[setdiff(names(daily_summary_df), names(daily_summary_df_day))] <- NA
      daily_summary_df[setdiff(names(daily_summary_df_day), names(daily_summary_df))] <- NA
    }
    daily_summary_df <- rbind(daily_summary_df, daily_summary_df_day)

    Sys.sleep(1.5)
    if (verbose) utils::setTxtProgressBar(pb,date)

  }

  summary_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(path, "/summary.sqlite"))
  DBI::dbWriteTable(summary_db, "daily_summary", daily_summary_df, overwrite = TRUE)








  ###################
  ###################  get daily movement
  ###################

  if (verbose) {print(paste0("Getting daily movement data for ", length(dates_max), " days"))}
  if (verbose) pb = utils::txtProgressBar(min = 0, max = length(dates_max), initial = 0, style = 3)


  daily_movement_df <- data.frame()

  for (date in 1:length(dates_max)) {

    params = list(
      `calendarDate` = dates_max[date]
    )
    Sys.sleep(2)
    if (verbose) utils::setTxtProgressBar(pb,date)



    res <- httr::GET(url = paste0('https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailyMovement/', display_name), httr::add_headers(.headers=headers), query = params,
                     handle = hand)
    movement <- httr::content(res, type = "text", encoding = "UTF-8")
    movement_list <- jsonlite::fromJSON(movement, flatten = TRUE)
    movement_df <- as.data.frame(movement_list$movementValues)
    if (nrow(movement_df) == 0) next
    names(movement_df) <- c("timestamp", "movement")
    #Fehler in names(movement_df) <- c("timestamp", "movement") :
    #Attribut 'names' [2] muss dieselbe Länge haben wie der Vektor [0]
    #Error in h(simpleError(msg, call)) :
    #  Fehler bei der Auswertung des Argumentes 'conn' bei der Methodenauswahl für Funktion 'dbDisconnect': Objekt 'summary_db_details' nicht gefunden
    movement_df[["date"]] <- as.character(params$calendarDate)
    daily_movement_df <- rbind(movement_df, daily_movement_df)
    #print(dim(daily_movement_df))



  }

  daily_movement_df[,1] <- as.character(as.POSIXct(daily_movement_df$timestamp/1000, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS"))

  DBI::dbWriteTable(summary_db, "daily_movement", daily_movement_df, overwrite = TRUE)


  ###################
  ###################  get heart rate
  ###################

  if (verbose) {print(paste0("Getting heart rate data for ", length(dates_max), " days"))}
  if (verbose) pb = utils::txtProgressBar(min = 0, max = length(dates_max), initial = 0, style = 3)


  daily_hr_df <- data.frame()

  for (date in 1:length(dates_max)) {

    params = list(
      `date` = dates_max[date]
    )
    if (verbose) utils::setTxtProgressBar(pb,date)
    Sys.sleep(2)


    res <- httr::GET(url = paste0("https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailyHeartRate/", display_name), httr::add_headers(.headers=headers), query = params,
                     handle = hand)
    res$status_code
    hr <- httr::content(res, type = "text", encoding = "UTF-8")
    hr_list <- jsonlite::fromJSON(hr, flatten = TRUE)
    hr_df <- as.data.frame(hr_list$heartRateValues)
    if (nrow(hr_df) == 0) next
    names(hr_df) <- c("timestamp", "heart_rate")
    daily_hr_df <- rbind(hr_df, daily_hr_df)




  }

  daily_hr_df[,1] <- as.character(as.POSIXct(daily_hr_df$timestamp/1000, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS"))

  DBI::dbWriteTable(summary_db, "daily_hr", daily_hr_df, overwrite = TRUE)


  ###################
  ###################  get daily stress and body battery
  ###################

  daily_stress_df <- data.frame()
  daily_body_battery_df <- data.frame()

  if (verbose) {print(paste0("Getting stress and body battery data for ", length(dates_max), " days"))}
  if (verbose) pb = utils::txtProgressBar(min = 0, max = length(dates_max), initial = 0, style = 3)



  for (date in 1:length(dates_max)) {

    if (verbose) utils::setTxtProgressBar(pb,date)
    Sys.sleep(2)


    res <- httr::GET(url = paste0('https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailyStress/', dates_max[date]), httr::add_headers(.headers=headers), query = params,
                     handle = hand)
    res$status_code
    stress <- httr::content(res, type = "text", encoding = "UTF-8")
    stress_list <- jsonlite::fromJSON(stress, flatten = TRUE)
    stress_df <- as.data.frame(stress_list$stressValuesArray)
    if (nrow(stress_df) == 0) next
    names(stress_df) <- c("timestamp", "stress_level")
    body_battery_df <- as.data.frame(stress_list$bodyBatteryValuesArray[,-2])
    if (nrow(body_battery_df) == 0) next
    names(body_battery_df) <- c("timestamp", "body_battery_level")

    daily_stress_df <- rbind(stress_df, daily_stress_df)
    daily_body_battery_df <- rbind(body_battery_df, daily_body_battery_df)






  }

  daily_body_battery_df[,1] <- as.character(as.POSIXct(as.numeric(daily_body_battery_df$timestamp)/1000, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS"))
  daily_stress_df[,1] <- as.character(as.POSIXct(as.numeric(daily_stress_df$timestamp)/1000, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS"))

  DBI::dbWriteTable(summary_db, "daily_stress", daily_stress_df, overwrite = TRUE)
  DBI::dbWriteTable(summary_db, "daily_body_battery", daily_body_battery_df, overwrite = TRUE)



  ###################
  ###################  get daily respiration
  ###################


  daily_respiration_df <- data.frame()

  if (verbose) {print(paste0("Getting respiration data data for ", length(dates_max), " days"))}
  if (verbose) pb = utils::txtProgressBar(min = 0, max = length(dates_max), initial = 0, style = 3)


  for (date in 1:length(dates_max)) {

    Sys.sleep(2)
    if (verbose) utils::setTxtProgressBar(pb,date)


    res <- httr::GET(url = paste0('https://connect.garmin.com/modern/proxy/wellness-service/wellness/daily/respiration/', dates_max[date]), httr::add_headers(.headers=headers), query = params,
                     handle = hand)
    respiration <- httr::content(res, type = "text", encoding = "UTF-8")
    respiration_list <- jsonlite::fromJSON(respiration, flatten = TRUE)
    respiration_df <- as.data.frame(respiration_list[length(respiration_list)])
    if (nrow(respiration_df) == 0) next
    names(respiration_df) <- c("timestamp", "respiration")

    daily_respiration_df <- rbind(respiration_df, daily_respiration_df)




  }

  daily_respiration_df[,1] <- as.character(as.POSIXct((daily_respiration_df$timestamp)/1000, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS"))

  DBI::dbWriteTable(summary_db, "daily_respiration", daily_respiration_df, overwrite = TRUE)


  ###################
  ###################  get sleep data
  ###################



  daily_sleep_levels_df <- data.frame()
  daily_sleep_movement_df <- data.frame()
  daily_sleep_summary_df <- data.frame()
  daily_sleep_pulse_ox_df <- data.frame()


  if (verbose) {print(paste0("Getting sleep data for ", length(dates), " days"))}



  for (date in 1:(length(dates))) {

    params = list(
      `date` = dates[date]
    )


    res <- httr::GET(url = paste0( 'https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailySleepData/', display_name), httr::add_headers(.headers=headers), query = params,
                     handle = hand)
    sleep <- httr::content(res, type = "text", encoding = "UTF-8")
    sleep_list <- jsonlite::fromJSON(sleep, flatten = TRUE)


    sleep_movement_df <- as.data.frame(sleep_list$sleepMovement)
    sleep_levels_df <- as.data.frame(sleep_list$sleepLevels)
    sleep_summary <- as.data.frame(t(unlist(sleep_list$dailySleepDTO)))
    sleep_pulse_ox_df <- as.data.frame(sleep_list$wellnessEpochSPO2DataDTOList)


    Sys.sleep(2)
    daily_sleep_levels_df <- rbind(sleep_levels_df, daily_sleep_levels_df)
    daily_sleep_movement_df <- rbind(sleep_movement_df, daily_sleep_movement_df)
    #handle different number of columns of dfs
    if (nrow(daily_sleep_summary_df) > 0) {
      daily_sleep_summary_df[setdiff(names(sleep_summary), names(daily_sleep_summary_df))] <- NA
      sleep_summary[setdiff(names(daily_sleep_summary_df), names(sleep_summary))] <- NA
    }
    daily_sleep_summary_df <- rbind(sleep_summary, daily_sleep_summary_df)
    daily_sleep_pulse_ox_df <- rbind(sleep_pulse_ox_df, daily_sleep_pulse_ox_df)

    if (verbose) utils::setTxtProgressBar(pb,date)




  }

  if (nrow(daily_sleep_levels_df) > 0) {

  daily_sleep_summary_df[,-c(3, 5, 6, 7, 19, 20)] <- apply(daily_sleep_summary_df[,-c(3, 5, 6, 7, 19, 20)], 2, function(x) as.numeric(x))
  daily_sleep_summary_df[,8:13] <- apply(daily_sleep_summary_df[,8:13], 2, function(x) {as.character(as.POSIXct(x/1000, origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%OS"))})

  DBI::dbWriteTable(summary_db, "daily_sleep_levels_df", daily_sleep_levels_df, overwrite = TRUE)
  DBI::dbWriteTable(summary_db, "daily_sleep_movement_df", daily_sleep_movement_df, overwrite = TRUE)
  }
  DBI::dbWriteTable(summary_db, "daily_sleep_summary_df", daily_sleep_summary_df, overwrite = TRUE)
  if (length(daily_sleep_pulse_ox_df) > 0) DBI::dbWriteTable(summary_db, "daily_sleep_pulse_ox_df", daily_sleep_pulse_ox_df, overwrite = TRUE)





  ###########
  ########### get steps
  ###########

  if (verbose) {print(paste0("Getting steps data for ", length(dates), " days"))}



  daily_activity_steps_df <- data.frame()


  for (date in 1:(length(dates))) {

    params = list(
      `date` = dates[date]
    )


    res <- httr::GET(url = paste0('https://connect.garmin.com/modern/proxy/wellness-service/wellness/dailySummaryChart/', display_name), httr::add_headers(.headers=headers), query = params,
                     handle = hand)
    steps <- httr::content(res, type = "text", encoding = "UTF-8")
    steps_list <- jsonlite::fromJSON(steps, flatten = TRUE)
    activity_steps_df <- as.data.frame(steps_list)

    daily_activity_steps_df <- rbind(activity_steps_df, daily_activity_steps_df)
    Sys.sleep(2)

    if (verbose) utils::setTxtProgressBar(pb,date)


  }

  if (nrow(daily_activity_steps_df) > 0) {
  DBI::dbWriteTable(summary_db, "daily_activity_steps_df", daily_activity_steps_df, overwrite = TRUE)
  }




  ###########
  ########### get activity details
  ###########

  if (verbose) pb = utils::txtProgressBar(min = 0, max = nrow(act_df), initial = 0, style = 3)


  if (verbose) {print(paste0("Getting activity details for ", nrow(act_df), " activities"))}



   #get type and ids from activity summary df
   type_and_ids <- data.frame(act_df$activityType.typeKey, act_df$activityId)
   unique_types <- unique(type_and_ids$act_df.activityType.typeKey)

   #create df for each activity type
   for(type in unique_types) {
     assign(paste0(type, "_activity_df"), data.frame())
   }

   #long
   for(type in unique_types) {
     assign(paste0(type, "_activity_df_long"), data.frame())
   }


   counter <- 1

   for (id in act_df$activityId) {

     url = paste0('https://connect.garmin.com/modern/proxy/activity-service/activity/', id, '/details')


     res <- httr::GET(url = url, httr::add_headers(.headers=headers), query = params, handle = hand)
     act <- httr::content(res, type = "text", encoding = "UTF-8")
     act_list <- jsonlite::fromJSON(act, flatten = TRUE)
     metrics <- unlist(act_list$activityDetailMetrics)
     names(metrics) <- NULL
     metrics_key <- act_list$metricDescriptors$key
     metrics_unit <- act_list$metricDescriptors$unit.key
     count <- act_list$metricsCount
     unique_metrics <- act_list$measurementCount

     #activity type
     activity_type <- act_df[counter, "activityType.typeKey"]
     activity_id <- act_df[counter, "activityId"]


     activity_details_df <- data.frame(value = metrics,
                                       metric = rep(metrics_key, times = count),
                                       unit = rep(metrics_unit, times = count),
                                       time_index = rep(1:count, each = unique_metrics))


     activity_details_df_long_temp <- activity_details_df[with(activity_details_df, order(metric, time_index)),]
     activity_details_df_long_temp[["activity_type"]] <- activity_type
     activity_details_df_long_temp[["activity_id"]] <- activity_id

     # activity_details_df_wide_temp <- stats::reshape(activity_details_df, idvar = c("time_index"), timevar = "metric", direction = "wide")
     # activity_details_df_wide_temp[["activity_type"]] <- activity_type
     # activity_details_df_wide_temp[["activity_id"]] <- activity_id



     #assign(paste0(activity_type, "_activity_df"), rbind(get(paste0(activity_type, "_activity_df")), activity_details_df_wide_temp))
     #long
     assign(paste0(activity_type, "_activity_df_long"), rbind(get(paste0(activity_type, "_activity_df_long")), activity_details_df_long_temp))


     #make wides dfs once all activities are downloaded
     if (counter == nrow(act_df)) {

       for(type in unique_types) {
         activity_details_df_wide_temp <- stats::reshape(get(paste0(activity_type, "_activity_df_long")), idvar = c("time_index","activity_id"), timevar = "metric", direction = "wide")
         activity_details_df_wide_temp[["activity_type"]] <- activity_type
         activity_details_df_wide_temp[["activity_id"]] <- activity_id
         assign(paste0(type, "_activity_df_wide"), activity_details_df_wide_temp)
       }
     }


     counter <- counter ++ 1
     Sys.sleep(2)

     if (verbose) utils::setTxtProgressBar(pb,counter)



   }




   summary_db_details <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(path, "/activity_details.sqlite"))
   #create df for each activity type
   for(type in unique_types) {
     #skip if df is empty
     if (nrow(get(paste0(type,"_activity_df"))) < 1) {next}
     DBI::dbWriteTable(summary_db_details, paste0(type, "_activities_wide"), get(paste0(type,"_activity_df")), overwrite = TRUE)
     DBI::dbWriteTable(summary_db_details, paste0(type, "_activities_long"), get(paste0(type,"_activity_df_long")), overwrite = TRUE)

   }










}






