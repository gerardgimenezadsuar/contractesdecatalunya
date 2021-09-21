
library(pbapply)
library(data.table)
library(dplyr)
setwd("/Users/gerardgimenezadsuar/contractesdecatalunya")
options(timeout=200)

start.time <- Sys.time()
contractes_o<- read.csv("https://analisi.transparenciacatalunya.cat/api/views/hb6v-jcbf/rows.csv?accessType=DOWNLOAD&sorting=true")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#contractes_red <- contractes_o %>% dplyr::filter(EXERCICI > 2016)
#saveRDS(contractes_red, "contractes_hist.rds")
start.time <- Sys.time()
contractes_2019 <- contractes_o %>% dplyr::filter(EXERCICI > 2019) %>%
  dplyr::filter(IMPORT.ADJUDICACIO > 0) %>%
  dplyr::select(EXERCICI, ORGANISME.CONTRACTANT, ADJUDICATARI, DATA.ADJUDICACIO, IMPORT.ADJUDICACIO, ANYS.DURADA)
saveRDS(contractes_2019, "contractes.rds")

contract <- readRDS("/Users/gerardgimenezadsuar/contractesdecatalunya/contractes.rds")

contractes_2021 <- contractes_o %>% dplyr::filter(EXERCICI > 2020) %>%
  dplyr::filter(IMPORT.ADJUDICACIO > 14950 & IMPORT.ADJUDICACIO < 15000 | IMPORT.ADJUDICACIO > 39950 & IMPORT.ADJUDICACIO < 40000) %>%
  dplyr::select(EXERCICI, ORGANISME.CONTRACTANT,PROCEDIMENT.ADJUDICACIO, ADJUDICATARI, DESCRIPCIO.EXPEDIENT,DATA.ADJUDICACIO, IMPORT.ADJUDICACIO, ANYS.DURADA) %>%
  dplyr::group_by(ORGANISME.CONTRACTANT) %>%
  dplyr::summarise(num = n()) %>%
  ungroup %>%
  dplyr::mutate(num/sum(num))


library(dplyr)
# contr <- contractes %>% dplyr::filter(EXERCICI > 2019) %>%
#   dplyr::filter(IMPORT.ADJUDICACIO > 0) %>%
#   dplyr::select(EXERCICI, AGRUPACIO.ORGANISME,ORGANISME.CONTRACTANT, ADJUDICATARI, CODI.EXPEDIENT,PROCEDIMENT.ADJUDICACIO,CONTRACTE, DATA.ADJUDICACIO, IMPORT.ADJUDICACIO, ANYS.DURADA)
# contr$DATA.ADJUDICACIO <- as.Date(contr$DATA.ADJUDICACIO, "%d/%m/%Y")
# saveRDS(contr, "contr.rds")
# contractes <- readRDS("contr.rds")
# tapply(contr$IMPORT.ADJUDICACIO, contr$PROCEDIMENT.ADJUDICACIO, function(x) sum(x)*100/sum(contr$IMPORT.ADJUDICACIO))
#
# ranking_menors <- contr %>%
#   dplyr::group_by(ORGANISME.CONTRACTANT,PROCEDIMENT.ADJUDICACIO) %>%
#   dplyr::summarise(totals = sum(IMPORT.ADJUDICACIO)) %>%
#   dplyr::group_by(ORGANISME.CONTRACTANT) %>%
#   dplyr::mutate(share= totals/sum(totals))
#
# contractes.desc <- contractes_o %>% dplyr::filter(EXERCICI > 2019) %>%
#   dplyr::filter(IMPORT.ADJUDICACIO > 0) %>%
#   dplyr::group_by(CONTRACTE) %>%
#   dplyr::summarize(nombre=n())
#
# contractes.desc <- contractes.desc[order(-contractes.desc$nombre),]
#
# head(order(contractes.desc$nombre))
#
# ll_c <- toString(contractes.desc$CONTRACTE[1:500])

# dif <- setdiff(contractes,contractes_2019)
# # total històric:
# tapply(contractes$IMPORT.ADJUDICACIO, contractes$EXERCICI, sum)
#
# adj <- unique(contractes$ADJUDICATARI)
#
# pro <- pbsapply(adj, function(i) tapply(contractes_red[contractes_red$ADJUDICATARI == i,]$IMPORT.ADJUDICACIO,contractes_red[contractes_red$ADJUDICATARI == i,]$EXERCICI, sum))
#
# df_pro <- rbindlist(lapply(pro, as.data.frame.list), fill=TRUE)
# df_pro$adjudicatari <- adj
# df_pro[is.na(df_pro)] <- 0
# df_pro$total <- df_pro[,1] + df_pro[,2] + df_pro[,3] + df_pro[,4] + df_pro[,5]
#
# df_pro_f <- df_pro %>%
#   dplyr::select(adjudicatari, X2017, X2018, X2019, X2020, X2021, total)
#
# saveRDS(df_pro_f, "historic.rds")
#
#
#
# # comprovador per empresa:
# tapply(contractes[contractes$ADJUDICATARI == "FUNDACIÓ PIA INSTITUT PERE TARRÉS D'EDUCACIÓ EN L' ESPLAI",]$IMPORT.ADJUDICACIO,contractes[contractes$ADJUDICATARI == "FUNDACIÓ PIA INSTITUT PERE TARRÉS D'EDUCACIÓ EN L' ESPLAI",]$EXERCICI, sum)
#
#
# con <- contractes %>%
#   dplyr::filter(EXERCICI %in% 2021) %>%
#   dplyr::group_by(ADJUDICATARI) %>%
#   dplyr::summarise("Total adjudicat (en €)" = sum(IMPORT.ADJUDICACIO),
#                    "Nº contractes totals" =n(),
#                    "Nº contractes menors al límit d'import" = sum(IMPORT.ADJUDICACIO < 15000 & IMPORT.ADJUDICACIO > 14900) + sum(IMPORT.ADJUDICACIO < 50000 & IMPORT.ADJUDICACIO > 49900) ,
#                    "Durada mitjana (en anys)" = round(mean(ANYS.DURADA),2))
# con <- con[order(-con$`Total adjudicat (en €)`),]
# nrow(con)
# adj <- con$ADJUDICATARI[1:10000]
# adj


con20 <- contractes_o %>%
  dplyr::filter(EXERCICI %in% 2020) %>%
  dplyr::group_by(ADJUDICATARI) %>%
  dplyr::summarise("Total adjudicat 2020 (en €)" = sum(IMPORT.ADJUDICACIO))

con19 <- contractes_o %>%
  dplyr::filter(EXERCICI %in% 2019) %>%
  dplyr::group_by(ADJUDICATARI) %>%
  dplyr::summarise("Total adjudicat 2019 (en €)" = sum(IMPORT.ADJUDICACIO))

con18 <- contractes_o %>%
  dplyr::filter(EXERCICI %in% 2018) %>%
  dplyr::group_by(ADJUDICATARI) %>%
  dplyr::summarise("Total adjudicat 2018 (en €)" = sum(IMPORT.ADJUDICACIO))

con17 <- contractes_o %>%
  dplyr::filter(EXERCICI %in% 2017) %>%
  dplyr::group_by(ADJUDICATARI) %>%
  dplyr::summarise("Total adjudicat 2017 (en €)" = sum(IMPORT.ADJUDICACIO))

# con16 <- contractes %>%
#   dplyr::filter(EXERCICI %in% 2016) %>%
#   dplyr::group_by(ADJUDICATARI) %>%
#   dplyr::summarise("Total adjudicat 2016 (en €)" = sum(IMPORT.ADJUDICACIO))



con_total <- merge(con20, con19, by = "ADJUDICATARI", all = TRUE)
con_total <- merge(con_total, con18, by = "ADJUDICATARI", all = TRUE)
con_total <- merge(con_total, con17, by = "ADJUDICATARI", all = TRUE)
#con_total <- merge(con_total, con16, by = "ADJUDICATARI", all = TRUE)
con_total[is.na(con_total)] <- 0
con_total$`Total últims 4 anys` <- con_total[,2] + con_total[,3] +con_total[,4] +con_total[,5]
con_total <- con_total[order(-con_total$`Total últims 4 anys`),]
con_total <- con_total[con_total$`Total últims 4 anys` > 0,]
con_total[,2:6] <- format(con_total[,2:6], nsmall=2, big.mark=".", decimal.mark="," )
saveRDS(con_total, "historic.rds")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
rsconnect::deployDoc(doc="contractesdecatalunya.Rmd",appName="contractesdecatalunya" ,launch.browser = T, forceUpdate = T)


# contractes <- readRDS("contractes.rds") # reloads 2019-2020 data
# historic <- readRDS("historic.rds") # reloads the historic data



# contractes_2021 <- contractes_o %>% dplyr::filter(EXERCICI > 2020) %>%
#   dplyr::filter(IMPORT.ADJUDICACIO > 14900 & IMPORT.ADJUDICACIO < 15000) %>%
#   dplyr::select(EXERCICI, ORGANISME.CONTRACTANT, ADJUDICATARI, DATA.ADJUDICACIO, IMPORT.ADJUDICACIO,CONTRACTE, ANYS.DURADA)
#
# for (i in list(unique(contractes_2021$ORGANISME.CONTRACTANT))) {
#
# }
#
# resum_14999 <- tapply(contractes_2021$ADJUDICATARI, contractes_2021$ORGANISME.CONTRACTANT, table)
# sum(resum_14999[[1]] > 1)
#
# df<-list()
# j <- 0
# for (i in 1:length(resum_14999)){
#   if (sum(resum_14999[[i]]>1) != 0) {
#     j <- j + 1
#     df[[j]] <- resum_14999[[i]]
#   }
# }
# df

