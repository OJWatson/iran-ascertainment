library(devtools)
install_github("adamkucharski/scrapR")
library(scrapR)
library(grImport)
library(tidyverse)

setwd(file.path(here::here(), "analysis/data/raw/hospitalisations"))

# get the first y ticks from the plots
first_y_ticks <- c(100, 100, 100, 200, 100, 30, 50, 1000,
                   100, 50, 250, 50, 250, 50, 50, 50,
                   200, 50, 100, 50, 200, 100, 50, 100,
                   200, 200, 300, 100, 100, 100, 50)

# and qickly run through load_pdf_data calls below to work out the first labelled x axis
guide_x_first <- c(15, 15, 15, 14, 14, 13, 12, 12,
                   11, 12, 14, 14, 14, 15, 14, 16,
                   12, 14, 13, 13, 14, 13, 15, 14,
                   15, 13, 14, 14, 14, 14, 14)

# province names
provs <- c("East Azerbaijan","West Azerbaijan","Ardabil","Isfahan",
           "Alborz","Ilam","Bushehr","Tehran","Chahar Mahaal and Bakhtiari",
           "South Khorasan","Razavi Khorasan","North Khorasan","Khuzestan",
           "Zanjan","Semnan","Sistan and Baluchistan","Fars","Qazvin",
           "Qom","Kurdistan","Kerman","Kermanshah","Kohgiluyeh and Boyer Ahmad",
           "Golestan","Gilan","Lorestan","Mazandaran","Markazi","Hormozgan","Hamedan","Yaz")

# what are the pages
pages <- gsub(".pdf", "", unlist(strsplit(list.files(), "pages_")), fixed = TRUE)
pages <- sort(unique(na.omit(as.numeric(pages))))
res <- vector("list", length(pages))

for(i in seq_along(pages)){

  # load the image in
  load_PDF_data(file_name = paste0("pages_", pages[i], ".pdf"))

  # write the guide file as this seems broken in scrapr
  write.csv(
    data.frame(point = c(guide_x_first[i],guide_x_first[i]+20,5,6,2,3),
               value = c(0, as.integer(as.Date("2021-10-23") - as.Date("2020-02-20")), 0, first_y_ticks[i], NA, NA),
               axis = c("x","x","y","y", "data","data")),
    file = paste0("pages_", pages[i], ".pdf.guide.csv")
  )

  # extract our data
  suppressMessages(extract_PDF_data(paste0("pages_", pages[i], ".pdf")))

  conf <- read.csv(paste0("pages_", pages[i], ".pdf2.csv"))
  susp <- read.csv(paste0("pages_", pages[i], ".pdf3.csv"))

  df <- rbind(
    data.frame(date = as.Date("2020-02-20") + conf$x,
               hosp = conf$y,
               type = "confirmed"),
    data.frame(date = as.Date("2020-02-20") + susp$x,
               hosp = susp$y,
               type = "suspected")
  )

  df$page <- pages[i]
  df$province <- provs[i]
  res[[i]] <- df

}

res_all <- do.call(rbind, res)
saveRDS(res_all, file.path(here::here(), "analysis/data/derived/hospitalisations.rds"))
ggplot(res_all, aes(date, hosp, color = type)) + geom_line() +
  ggpubr::theme_pubclean() + theme(legend.position = "right", axis.line = element_line()) +
  xlab("") + ylab("Hospitalisations") +
  facet_wrap(~province, scales = "free_y")

# AND do the national

# load the image in
load_PDF_data(file_name = "iran.pdf")

# write the guide file as this seems broken in scrapr
write.csv(
  data.frame(point = c(12,30,5,6,2,3),
             value = c(0, as.integer(as.Date("2021-10-23") - as.Date("2020-02-20")), 0, 5000, NA, NA),
             axis = c("x","x","y","y", "data","data")),
  file = paste0("iran.pdf.guide.csv")
)

# extract our data
extract_PDF_data(paste0("iran.pdf"))

conf <- read.csv(paste0("iran.pdf2.csv"))
susp <- read.csv(paste0("iran.pdf3.csv"))

df <- rbind(
  data.frame(date = as.Date("2020-02-20") + conf$x,
             hosp = conf$y,
             type = "confirmed"),
  data.frame(date = as.Date("2020-02-20") + susp$x,
             hosp = susp$y,
             type = "suspected")
)

df$page <- 2
df$province <- "Iran"
res_all <- rbind(res_all, df)
saveRDS(res_all, file.path(here::here(), "analysis/data/derived/hospitalisations.rds"))
