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

## Small function used later in case a modality is not used
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}


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
data <- data[colSums(!is.na(data)) > 0]


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

questions$maxscore <- 0
questions$mean <- 0
questions$std <- 0

## Checking what type of questions we have
questions$type <- as.character(questions$type)
levels(as.factor(questions$type))


## initialise score dataframe
scores <- data.frame(operation = as.character("test"),
                     indicator = as.character("test"),
                     score = 0,
                     stringsAsFactors = FALSE)





## Now compile indicators for each country #####
for (i in 1:nrow(questions)) {
  # i <- 41
  questionname <- as.character(questions[ i, c("fullname")])
  questionlabel <- as.character(questions[ i, c("label")])
  questiontype <- as.character(questions[ i, c("type")])
  cat(paste0( i, "- ",questionlabel, "\n" ))

  ### Now subset all variables modalities with the matching questions names
  ## case indicator is a select_one
  if (questiontype == "select_one") {
    dicovar <- as.character(dico[dico$type == "select_one" & grepl(questionname,dico$fullname ), c("fullname") ])

    ## Subset this question
    dataquest <- as.data.frame(data[ , dicovar])
    names(dataquest)[1] <- questionname
    ## Create one column per modalities for this question
    modalities <- as.data.frame(levels(as.factor(as.character(dataquest[ ,1]))))
    if (nrow(modalities) > 1 ) {
      for (h in 1:nrow(modalities)) {
        # h <-1
        modal <- as.character(modalities[ h , 1])
        dataquest[ , h + 1] <- ifelse(dataquest[ ,  1] == modal, 1, 0)
        names(dataquest)[h + 1] <- paste0(questionname, ".", modal)
      }


      datascored <- as.data.frame(data$intro.Operation)
      datascored[ , 2] <-  questionname
      datascored[ , 3] <-  0
      names(datascored)[1] <- "operation"
      names(datascored)[2] <- "indicator"
      names(datascored)[3] <- "score"
      ## Apply the score for the respective modalities
      for (k in 2:ncol(dataquest)) {
        # k <- 2
        fulname1 <- names(dataquest)[k]
        scorefullnames1 <- dico[ dico$fullname == fulname1 , c("score")]

        datascored[ , k + 2 ] <- ifelse( dataquest[ , k - 1] == 1,  1 * scorefullnames1 , 0)
        datascored[ , 3] <- datascored[ , 3] + datascored[ , k + 2 ]
      }
      scores <- rbind(scores, datascored[ , 1:3] )

      ## Get the maximum score, mean & standard deviation  to append to main table
      maxscore <- max(datascored[ , 3])
      questions[questions$fullname == questionname , c("maxscore")] <- maxscore


      mean <- mean(datascored[ , 3])
      questions[questions$fullname == questionname , c("mean")] <- mean


      std <- sd(datascored[ , 3])
      questions[questions$fullname == questionname , c("std")] <- std

      rm(datascored)
    }


  } else {

    ## case  indicator is a select_multiple
    dicovar <- as.character(dico[dico$type == "select_multiple" & grepl(questionname,dico$fullname ), c("fullname") ])

    #check first that those modalities were actually used in the dataset
    dicodata <- as.character(names(data))
    dicovar2 <- intersect_all(dicovar, dicodata)

    ## case there's no data at all -- ie. dicovar2 is empty
    if ( !identical(dicovar2, character(0)) ) {

        ## Select all modalities for this question
        dataquest <- data[ , dicovar2]

        datascored <- as.data.frame(data$intro.Operation)
        datascored[ , 2] <-  questionname
        datascored[ , 3] <-  0
        names(datascored)[1] <- "operation"
        names(datascored)[2] <- "indicator"
        names(datascored)[3] <- "score"
        ## Apply the score for the respective modalities
        for (j in 1:ncol(dataquest)) {
          # j <- 1
          fulname1 <- names(dataquest)[j]
          scorefullnames1 <- dico[ dico$fullname == fulname1 , c("score")]

          datascored[ , j + 3 ] <- ifelse( dataquest[, j] == 1,  1 * scorefullnames1 , 0)
          datascored[ , 3] <- datascored[ , 3] + datascored[ , j + 3 ]
        }
        scores <- rbind(scores, datascored[ , 1:3] )

        ## Get the maximum score, mean & standard deviation  to append to main table
        maxscore <- max(datascored[ , 3])
        questions[questions$fullname == questionname , c("maxscore")] <- maxscore


        mean <- mean(datascored[ , 3])
        questions[questions$fullname == questionname , c("mean")] <- mean


        std <- sd(datascored[ , 3])
        questions[questions$fullname == questionname , c("std")] <- std



        rm(datascored)
    }

  }

}

scores.full <- merge( x = scores, y = questions, by.x = "indicator", by.y = "fullname")

## Scale score
scores.full$scaled.score <- scores.full$score / scores.full$maxscore

## Assign direction
str(scores.full$sign)
scores.full$scaled.directed.score <- scores.full$scaled.score * scores.full$sign

##identify potential outliers -    (score - mean)/(standard deviation)
scores.full$test <- (scores.full$score - scores.full$mean) / scores.full$std

## concatenate name
scores.full$newname <- paste0(scores.full$theme, "_", scores.full$name)


## Backup for report
write.csv(scores.full,"data/scorefull.csv", row.names = FALSE, na = "")
write.csv(questions,"data/questions.csv", row.names = FALSE, na = "")

levels(as.factor(scores.full$concept))

### Compile for Cronbach alpha #########
scores.wide <- scores.full[ !is.na(scores.full$scaled.score)  , ]
scores.wide <- dcast(scores.wide, operation  ~ newname, value.var = "scaled.score", sum)
row.names(scores.wide ) <- scores.wide$operation
scores.wide$operation <- NULL
Cronbach <- alpha(scores.wide, check.keys = TRUE)


### Compile summary score per sector

themes.op <- as.data.frame(data$intro.Operation)
themes <- as.data.frame(unique(questions$theme))
for (i in 1:nrow(themes)) {
  # i <- 2
  theme.this <- as.character(themes[ i, 1])
  ## subset
  scores.this <- scores.full[ !is.na(scores.full$scaled.score) & scores.full$theme == theme.this , ]

  ## go from long to wide
  scores.this.wide <- dcast(scores.this, operation  ~ newname, value.var = "scaled.score", sum)

  ## put operation name as row.names
  row.names(scores.this.wide ) <- scores.this.wide$operation
  scores.this.wide$operation <- NULL

  ## Compile the score for the theme
  themes.op[ , i + 1] <- rowSums(scores.this.wide, na.rm = TRUE, dims = 1)  / ncol(scores.this.wide)
  ## sclae it
  #themes.op[ , i + 1] <- round(scale(themes.op[ , i + 1],
  #                             center = min(themes.op[ , i + 1]),
  #                             scale = max(themes.op[ , i + 1]) - min(themes.op[ , i + 1])) , 2)
  names(themes.op)[i + 1] <- theme.this

  ## save with different name
  assign(  paste("scores.", theme.this, sep = ""), scores.this.wide )
  rm(scores.this, theme.this,scores.this.wide )

}


##### Step : subsetting by concept to check clusters #####
concept.op <- as.data.frame(data$intro.Operation)
concept <- as.data.frame(unique(questions$concept))
for (i in 1:nrow(concept)) {
  # i <- 2
  concept.this <- as.character(concept[ i, 1])
  ## subset
  scores.this <- scores.full[ !is.na(scores.full$scaled.score) & scores.full$concept == concept.this , ]

  ## go from long to wide
  scores.this.wide <- dcast(scores.this, operation  ~ newname, value.var = "scaled.score", sum)

  ## put operation name as row.names
  row.names(scores.this.wide ) <- scores.this.wide$operation
  scores.this.wide$operation <- NULL


  ## Compile the score for the concept
  concept.op[ , i + 1] <- rowSums(scores.this.wide, na.rm = TRUE, dims = 1) / ncol(scores.this.wide)
  ## scale it
  #concept.op[ , i + 1] <- round(scale(concept.op[ , i + 1],
  #                                   center = min(concept.op[ , i + 1]),
  #                                   scale = max(concept.op[ , i + 1]) - min(concept.op[ , i + 1])) , 2)


  names(concept.op)[i + 1] <- concept.this


  ##
  assign(  paste("scores.", concept.this, sep = ""), scores.this.wide )
  rm(scores.this, concept.this,scores.this.wide )
}



## get a vector with subregion for representation
subregion <- as.data.frame(data[order(data$intro.Operation), c("intro.SubRegion")])
names(subregion)[1] <- "subregion"

