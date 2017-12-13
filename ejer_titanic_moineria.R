instalar <- function(paquete) {
  
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

paquetes <- c('lubridate', 'magrittr', 'ggvis', 'dplyr', 'tidyr', 'readr', 'rvest', 
              'ggplot2', 'stringr', #'ggthemes',
              'googleVis', 'shiny', 'tibble', 'vcd', 'vcdExtra',
              'GGally', 'readODS', 'readxl', "RSQLite")

lapply(paquetes, instalar)

titanic_path <- '/home/abraham/intro_to_ds/data/Titanic/titanic.ods'

ds_names <- ods_sheets(titanic_path)
ds_names

clean_sheet_name <- function(sheet_name) {
  str_replace_all(str_replace_all(string=sheet_name, pattern=" ", replace="_"), pattern="'", replace="") %>% 
    str_to_lower()
}

sapply(ds_names, clean_sheet_name)


save_sheet <- function(sheet_name) {
  file_name <-  paste0("/home/abraham/intro_to_ds/data/Titanic/", clean_sheet_name(sheet_name), ".rds")
  saveRDS(object = read_ods(titanic_path, sheet = sheet_name), file = file_name)
}


lapply(ods_sheets(titanic_path), save_sheet)



rm(list=ls())

rds_files <- dir("/home/abraham/intro_to_ds/data/Titanic/", pattern = "*.rds", full.names = TRUE)

#lapply te devolverá las cosas en un lista... una lista de dataframes :)
ds <- lapply(rds_files, read_rds)
class(ds)
class(ds[[1]])
#cuantos dataframes contiene esta lista? 
length(ds)

#basename elimina todo el path del nombre excepto la última parte (se quedará con la extensión del archivo!), ?basename
names(ds) <- lapply(rds_files, basename)
names(ds)

#veamos qué nombres tiene cada dataframe
lapply(ds, names)

#si quisieramos obtener los conjuntos de nombres únicos
lapply(ds, names) %>% unique()

lapply(ds, head)

ds <- ds[-which(lapply(lapply(ds, names), length) == 2)]

num_cols <- lapply((lapply(ds, names)), length) %>% unlist() %>% min()
num_cols 


lapply(ds, str)


ds <- lapply(ds, function(x) lapply(x, as.character))
#verifiquemos 
lapply(ds, str)

#bind_rows es como rbind solo que optimizado por Hadley Wickham :) 
titanic <- bind_rows(ds)[, 1:num_cols]

names(titanic) <- str_replace_all(names(titanic), "/| ", "_") %>% 
  str_to_lower()
names(titanic)

titanic <- tbl_df(titanic)
titanic

titanic <- titanic %>% 
  separate(name, into=c("last_name", "name"), sep=",", extra="drop") %>%
  separate(fare, into=c("pounds", "shillings", "pence"), sep=" ", extra="drop") %>%
  separate(age, into=c("age", "units"), sep=2, extra="drop") %>%
  mutate(sex=ifelse(grepl("Miss|Mrs|Mme.|Lady|Doña|Ms", name), 'F',
                    ifelse(grepl("Mr|Sir|Sig|Dr|Master|Captain|Major|Rev.|Colonel|Fr|Don.", name), 'M', NA))) %>% 
  mutate(boat_location=ifelse(as.integer(boat) %in% c(9:16), 'Popa', 
                              ifelse(boat %in% c(LETTERS[1:4]) | as.integer(boat) %in% c(1:8), 'Proa', NA))) %>% 
  mutate(age=ifelse(units == "m", 1, as.integer(age))) %>% 
  mutate(survived=!is.na(boat)) %>%
  dplyr::select(-c(shillings, pence, body, units)) %>%
  mutate(pounds=str_replace(pounds, "£", "") %>% as.integer()) %>%
  mutate(class_dept=as.factor(class_dept), group=as.factor(group), ship=as.factor(ship),
         joined=as.factor(joined), job=as.factor(job), boat=as.factor(boat),
         sex=as.factor(sex), boat_location=as.factor(boat_location))



summary(titanic)


titanic <- titanic %>% mutate(age = ifelse(age <= 18, "infante",
                                           ifelse(age > 65, "adulto mayor", 
                                                  "adulto")))
#verifiquemos
titanic


ggplot(titanic, aes(pounds)) + 
  geom_histogram(binwidth = 30, na.rm=T) +
  theme_bw()


titanic <- titanic %>% 
  group_by(ticket) %>% 
  mutate(pounds_per_ticket = round(pounds/n())) %>% 
  ungroup()

titanic

titanic %>% filter(class_dept %in% c('1st Class', '2nd Class', '3rd Class')) %>%
  ggplot(aes(pounds_per_ticket)) + 
  geom_histogram(binwidth = 10) + 
  facet_grid(class_dept~., scales = "free_y") +
  theme_bw()


titanic %>% 
  group_by(boat_location) %>% 
  summarise(n=n())


titanic %>%
  group_by(boat) %>%
  summarise(n=n()) %>%
  arrange(desc(n))






library(RSQLite)

berka_db <- src_sqlite(path="~/Documents/itam/introduction_to_ds/intro_to_ds/data/berka/berka.raw", 
                       create=FALSE)
db_list_tables(berka_db$con)




















