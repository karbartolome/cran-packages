


# Librerías ---------------------------------------------------------------
library(rvest)
library(xml2)
library(purrr)
library(dplyr)
library(rvest)
library(cranlogs)
library(stringr)



# Task views: paquetes en cada task view ----------------------------------
path_taskviews <- "https://cran.microsoft.com/snapshot/2022-01-01/web/views/"

get_packages <- function(task_view){
  path <- paste0(path_taskviews,
                 task_view,".html")
  
  url <- url(path, "rb")
  
  paquetes <- read_html(url) %>% 
    html_node('body > ul:nth-child(5)') %>% 
    list() %>% 
    map(~html_nodes(.x, "li") %>% html_text()) %>% 
    .[[1]]
  
  close(url)
  
  return(paquetes)
  
}


url <- url(path_taskviews, 'rb')

df_task_views <- read_html(url) %>% 
  html_table() %>% .[[1]] %>% 
  as.data.frame() %>% 
  rename(task_view=X1, task_view_descr=X2) %>% 
  mutate(package=map(task_view, ~get_packages(.x)))

close(url)

rm(path_taskviews, get_packages, url)

df_task_views <- df_task_views %>% 
  tidyr::unnest(cols=c(package)) %>% 
  mutate(package = gsub(' \\(core\\)','',package))





# CRAN: Paquetes disponibles y descripción --------------------------------
path <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
url <- url(path, 'rb')

df_cran_packages <- read_html(url) %>% 
  html_table() %>% .[[1]] %>% 
  as.data.frame() %>% 
  inner_join(available.packages() %>% as.data.frame(), on='package') %>% 
  rename(package=Package)

close(url)
rm(path, url)


# Hay un par de paquetes que están en task views pero no en CRAN:
df_task_views %>% filter(!package %in% df_cran_packages$package) %>% 
  pull(package) %>% unique()



# CRAN + Task views -------------------------------------------------------

df <- df_task_views %>% 
  left_join(df_cran_packages)
  
  


# CRAN: descargas 2021-----------------------------------------------------

chunks <- split(df$package, 
                ceiling(seq_along(df$package) / 500))

df_cran_descargas = data.frame()
for(i in chunks){
  temp <- cran_downloads(
    from='2021-01-01', to='2021-12-31', 
    packages = i) %>% 
    mutate(month = lubridate::floor_date(date, 'month')) %>% 
    group_by(package, month) %>% 
    summarise(n=sum(count))
  
  df_cran_descargas <- df_cran_descargas %>% 
    bind_rows(temp)
}


rm(chunks)

library(ggplot2)
df_cran_descargas %>% filter(package=='dplyr') %>% 
  ggplot(aes(x=month, y=n))+
  geom_line()



# Task views + CRAN + descargas por mes -----------------------------------

df <- df %>% 
  left_join(df_cran_descargas)



# Guardar datos -----------------------------------------------------------

write.csv(df, '01_data/df_task_views.csv', row.names=FALSE)

























MRAN <- "http://mran.revolutionanalytics.com/snapshot/2022-01-01/"

pdb <- MRAN %>%
  contrib.url(type = "source") %>%
  available.packages(type="source", filters = NULL)


library(igraph)
library(miniCRAN)
library(igraph)
library(magrittr)

g <- pdb[, "Package"] %>%
  makeDepGraph(availPkgs = pdb, 
               suggests=FALSE, 
               enhances=TRUE, 
               includeBasePkgs = FALSE)




pr <- g %>%
  page.rank(directed = FALSE) %>%
  use_series("vector") %>%
  sort(decreasing = TRUE) %>%
  as.matrix %>%
  set_colnames("page.rank")


# Display results ---------------------------------------------------------

head(pr, 25)


# build dependency graph of top packages ----------------------------------

set.seed(42)
pr %>%
  head(25) %>%
  rownames %>%
  makeDepGraph(pdb) %>%
  plot(main="Top packages by page rank", cex=0.5)





