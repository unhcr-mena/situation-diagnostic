rm(list = ls())

## Load all required packages         #############################################
source("code/0-packages.R")
source("code/0-config.R")

### Double Check that you have the last version
#source("https://raw.githubusercontent.com/unhcr/koboloadeR/master/inst/script/install_github.R")
#install.packages("devtools")
#library("devtools")
#install_github("unhcr/koboloadeR", ref = "gh-pages")

library(koboloadeR)

## kobo_projectinit()
## Now Position your form & your data in the data folder



## Load form and building dictionnary #############################################
#rm(form)
#form <- "form.xls"
## Generate & Load dictionnary
cat("\n\n\n Generate dictionnary from the xlsform \n\n\n\n")
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
#rm(form)


# Load data #######################################################################
cat("\n\n\n Load original dataset \n\n\n\n")
#data.or <- read_excel("data/data.xlsx")
data.or <- read_excel("data/datatest.xlsx")
## exported with CSV legacy
#data.or <- read.csv("data/data.csv", na.strings = "n/a")

#names(data.or)
### Need to replace slash by point in the variable name
## get variable name from data
#datalabel <- as.data.frame( names(data.or))
#names(datalabel)[1] <- "nameor"
#datalabel$nameor <- as.character(datalabel$nameor)

## new variables name without /
#library(stringr)
#datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")
## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
#names(data.or) <- datalabel[, 2]
#names(data.or)

#household <- data.or



## Separate Yemen North & Yemen South
data.or[ 2, c("intro.Operation")] <- "NorthYemen"
data.or[ 3, c("intro.Operation")] <- "SouthYemen"

## Check to split select_multiple if data is extracted from ODK ###################
cat("\n\n\n Now split select_multiple  variables \n\n\n\n")
data <- kobo_split_multiple(data.or, dico)


#remove columns with all NA
data <- household[colSums(!is.na(data)) > 0]


## We now save a back up in the data folder to be used for the Rmd  ###############
cat("\n\nWrite backup ready for calculation \n")
write.csv(data,"data/datafull.csv", row.names = FALSE, na = "")

## Compute indicators if defined ##################################################
#source("code/2-create-indicators.R")
## Writing to a new version of the dico
#write.csv(dico, paste0("data/dico_",form,"-indic.csv"), row.names = FALSE, na = "")

## Re-encoding data now based on the dictionnary -- ##############################
## the xlsform dictionnary can be adjusted this script re-runned till satisfaction
cat("\n\n\n Now  re-encode data  \n\n\n\n")
household <- kobo_encode(data, dico)


## Cheking the labels matching... #################################################
## household is the default root data componnents to be used -- in order to deal with nested dataset
cat("\n\n\n Now  labeling variables \n\n\n\n")
household <- kobo_label(household, dico)


## We now save a back up in the data folder to be used for the Rmd  ###############
cat("\n\nWrite backup ready for report generation \n")
write.csv(household,"data/data2.csv", row.names = FALSE, na = "")


### Generating indicator
library(readxl)
questions <- read_excel("data/form.xls", sheet = "survey")
#names(questions)
## Subset themes & concept
questions <- questions[ !(is.na(questions$theme)) , c("name", "theme", "concept",  "sign"  )]

## merge with dico to obtain the full variable name
questions <- merge( x = questions, y = dico[dico$type %in% c("select_multiple_d", "select_one") ,
                                            c("name", "fullname", "label", "type")], by = "name", allx = TRUE)

## Checking what type of questions we have
questions$type <- as.character(questions$type)
levels(as.factor(questions$type))




## Variable to be kep for scoring

household2 <- household[ c("")]


## Now compile indicators for each country
for (i in 1:nrow(questions)) {
   #i <- 4
  questionname <- as.character(questions[ i, c("fullname")])
  questionlabel <- as.character(questions[ i, c("label")])
  questiontype <- as.character(questions[ i, c("type")])
  cat(paste0( i, "- ",questionlabel, "\n" ))

  ### Now subset all variables modalities with the matching questions names
  ## case indicator is a select_one
  if (questiontype == "select_one") {
    dicovar <- as.character(dico[dico$type == "select_one_d" & grepl(questionname,dico$fullname ), c("fullname") ])
  } else {
    ## case  indicator is a select_multiple
    dicovar <- as.character(dico[dico$type == "select_multiple" & grepl(questionname,dico$fullname ), c("fullname") ])
  }

  dataquest <- data[ , dicovar]

  ## Now compile the score using the scores recorded in the dictionnary


  for (j in 1:nrow(data)) {
    # j <- 1
    operation <- as.character(data[j, c("intro.Operation")])

    cat(paste0( i, "- ",questionlabel, "-", j, "-", operation, "\n" ))

    for (h in 1:ncol(data)) {
       # names(data)
       # h <-
      #stringdist_join(x = questionname, y = names(data), by = "fullname", mode = "left", max_dist = 2)



      allcolumn <- dplyr::filter(data, !grepl(questionname,names(data)))
      str(names(data))
      str(names(data))

      if ( data[j , h] == questionname ) {


      }
    }
  }

}
