# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Compile scheduler script
#
# All functions!
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

build_rl_script <- function(arg_playlist) {
  # !TEST! # 
  # arg_playlist <- c("20200106_ma07.180_ochtendeditie",         # the playlist made in NipperNext
  #                   "20181118_zo10.060_een_vroege_wandeling")  # the playlist to use as replay
  # arg_playlist <- c("20220602_do11-060_onbekend_onbemind")
  # arg_playlist <- c("20230313_ma07-180_ochtendeditie")
  # !TEST! # 
  
  playlist <- arg_playlist[1];
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Info types samenstellen - zie tabblad "schedule_radiologik"
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # const_0
  sch01_C0 <- "Radiologik Schedule Segment" %>% as_tibble()
  
  # dag
  sch01_dag <- rls_dagletters(playlist) %>% as_tibble()  
  
  # lengte
  sch01_lengte <- rls_lengte(playlist) %>% as_tibble()
  
  # dj_voorkeur
  sch01_dj_voorkeur <- "standaard" %>% as_tibble()
  
  # stuur_naar_dj
  sch01_stuur_naar_dj <- "ProgramTo=0" %>% as_tibble()
  
  # start
  sch01_start <- rls_30m_blokken(playlist) %>% as_tibble()
  
  # const_4
  sch01_C4 <- "0" %>% as_tibble()
  
  # leeg_1
  sch01_leeg_1 <- "" %>% as_tibble()
  
  # venster_van en -tot
  v_limiet <- rls_venster(playlist)
  sch01_venster_van <- paste(v_limiet[1], "0", sep = "\t") %>% as_tibble() 
  sch01_venster_tot <- paste(v_limiet[2], "0", sep = "\t") %>% as_tibble()
  
  # const_8
  sch01_C8 <- "ProgramCopyPath=nopath" %>% as_tibble()
  
  # color
  sch01_color <- "ColorLabel=238,238,153" %>% as_tibble()
  
  # const_10
  sch01_const_10 <- "0" %>% as_tibble()
  
  # leeg_2
  sch01_leeg_2 <- "" %>% as_tibble()
  
  # const_12
  sch01_const_12 <- "Display=True" %>% as_tibble()
  
  # const_13
  sch01_const_13 <- "PlayRotatediniTunes=False" %>% as_tibble()
  
  # const_14
  sch01_const_14 <- "Notes=" %>% as_tibble()
  
  # const_15
  sch01_const_15 <- paste("PrePostAppleScripts=", "", sep = "\t") %>% as_tibble()
  
  # const_16
  sch01_const_16 <- "AlbumSeparation=0" %>% as_tibble()
  
  # const_17
  sch01_const_17 <- "Begin Script" %>% as_tibble()
  
  # load
  rlprg_file <- paste0(if_else(is.na(arg_playlist[2]), 
                               playlist, 
                               arg_playlist[2]), 
                       ".rlprg")
  
  sch01_load <- paste("load", "", "", "", "", rlprg_file, "", "", "", "", "", "", "", sep = "\t") %>% 
    as_tibble()
  
  # aanvullingen (aanvullijst bevat steeds maar 1 stuk)
  sch01_play_a <- paste("pick", "", "",          "",          "r",
                        "nipper_aanvullen_klassiek_A", "", "", "", "", "", "", "", sep = "\t") %>% 
    as_tibble()
  
  sch01_play_b <- paste("pick", "", "",          "",          "r",
                        "nipper_aanvullen_klassiek_B", "", "", "", "", "", "", "", sep = "\t") %>% 
    as_tibble()

  sch01_play_c <- paste("pick", "", "",          "",          "r",
                        "nipper_aanvullen_klassiek_C", "", "", "", "", "", "", "", sep = "\t") %>% 
    as_tibble()
  
  script_file <- bind_rows(sch01_C0,
                           sch01_dag,
                           sch01_lengte,
                           sch01_dj_voorkeur, 
                           sch01_stuur_naar_dj,
                           sch01_start, 
                           sch01_C4, 
                           sch01_leeg_1,
                           sch01_venster_van,
                           sch01_venster_tot, 
                           sch01_C8,
                           sch01_color,
                           sch01_const_10,
                           sch01_leeg_2,
                           sch01_const_12,
                           sch01_const_13,
                           sch01_const_14,
                           sch01_const_15,
                           sch01_const_16,
                           sch01_const_17,
                           sch01_load,
                           sch01_play_a,
                           sch01_play_b,
                           sch01_play_c
  )
  
  # zet de startscripts voor de playlists in de schedules-map van RL, naam begint met
  # een volgnummer: 1 + <aantal scripts in deze map>
  home_radiologik_schedules <- "C:/cz_salsa/nipper/temp_rlprg/"
  # home_radiologik_schedules <- paste0(home_prop("home_radiologik_win"), "Schedule/")
  nrow_schedules <- 1L + dir_ls(path = home_radiologik_schedules) %>% 
    as_tibble() %>% nrow
  
  script_file_name <- sprintf(paste0("%03d - ", 
                                     paste0(str_sub(playlist, 1, 4),
                                            "-",
                                            str_sub(playlist, 5, 6),
                                            "-",
                                            str_sub(playlist, 7)
                                     )
                              ),
                              nrow_schedules) %>% 
    str_replace_all(pattern = "\\.", replacement = "-")
  
  script_file_name <- paste0(home_radiologik_schedules, script_file_name)
  
  # bestaand script laten staan
  existing_script <- dir_ls(path = home_radiologik_schedules,
                            type = "file",
                            regexp = str_sub(path_file(script_file_name), 7))
  
  if (length(existing_script) == 0) {
    write.table(x = script_file, file = script_file_name, row.names = FALSE, col.names = FALSE, 
                sep = "\t", quote = FALSE, fileEncoding = "UTF-8") 
    
    flog.info("RL-schedulerjob toegevoegd: %s", cur_pl, name = "nsbe_log")
  } else {
    flog.info("RL-schedulerjob al aanwezig: %s", cur_pl, name = "nsbe_log")
  }
}

rls_dagletters <- function(some_playlist) {
  # some_playlist <- "20180603_wo07.060_de_titel_klassiek"
  dag_kort <- str_sub(some_playlist, 10, 11)
  rls_dagletters_result <- case_when(dag_kort == "ma" ~ "_M_____",
                                     dag_kort == "di" ~ "__T____",
                                     dag_kort == "wo" ~ "___W___",
                                     dag_kort == "do" ~ "____T__",
                                     dag_kort == "vr" ~ "_____F_",
                                     dag_kort == "za" ~ "______S",
                                     TRUE             ~ "S______"
  )
}

rls_lengte <- function(some_playlist) {
  # some_playlist <- "20180603_wo07.060_de_titel_klassiek"
  rls_lengte_result <- str_sub(some_playlist, 15, 17) %>% as.integer %>% as.character
}

rls_30m_blokken <- function(some_playlist){
  # some_playlist <- "20180603_wo07.180_de_titel_klassiek"
  rls_30m_blokken_result <- some_playlist %>% str_sub(12, 13) %>% as.integer
  rls_30m_blokken_result <- as.character(2 * rls_30m_blokken_result)
}

rls_venster <- function(some_playlist) {
  # !TEST! # some_playlist <- "20181231_wo00.420_de_nacht_klassiek"
  venster_datum_start <- str_sub(some_playlist, 1, 8) %>% ymd
  venster_datum_stop <- venster_datum_start + days(1)
  rl_date_fmt <- stamp_date("23 Mar 2018", locale = "C", quiet = T)
  venster_datum_start %<>% rl_date_fmt %>% str_replace(pattern = "mei\\.", replacement = "mei ")
  venster_datum_stop %<>% rl_date_fmt %>% str_replace(pattern = "mei\\.", replacement = "mei ")
  rls_venster_result <- c(venster_datum_start, venster_datum_stop)
}

# rls_fill is disabled! Want:
# Een fill na een RL-playlist werkt anders werkt dan een fill na een gewone iTunes-playlist. 
# Het verschil zit in de manier waarop Scheduler bepaalt hoeveel muziek er al klaargezet is 
# na het laden van de playlist zelf, dus vóór het uitvoeren van de fill. 
# Als het een iTunes-playlist is, neemt hij de actuele queue-lengte van de player. 
# Als het een RL-playlist is, neemt hij de waarde die in de RL-playlist staat - de schatting dus. 
# Daar zit het probleem. Omdat de schatting meestal te hoog is, wordt de aanvulling te kort. 
# De fill daarom altijd uitvoeren met tracks van minstens 15 minuten. De playlist zal dan meestal 
# veel te lang worden, maar de interrupt van het volgende programma zal hem op tijd uitfaden. 
rls_fill <- function(some_playlist) {
  # some_playlist <- "20181231_wo00.420_de_nacht_klassiek"
  fill_start <- some_playlist %>% str_sub(15, 17) %>% as.integer %>% -1
  rls_fill_result <- c(str_c(fill_start, ":54"), str_c(fill_start, ":59"))
}
