# Load main data file, including all indices
df = readRDS("~/cropmonitor/cropmonitor.rds")

# screen for test sites in the US / EU
df = df[which(df$longitude > 70),]
df = df[, -which(names(df) %in% "questionnaireresult")]

# read questionnaire data and merges with the original
# data
if (file.exists("~/cropmonitor/questionaire.xlsx")){
  quest = readxl::read_excel("~/cropmonitor/questionaire.xlsx")
  quest = quest[,-(2:9)]
  df = merge(df, quest, by = 'reportid', all.x = TRUE)
}

# create unique field vector
df$userfield = paste(df$uniqueuserid,df$uniquecropsiteid,sep = "-")
rm(quest)
