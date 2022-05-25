#Stop Words
get_stopwords <- function(wordlist = "standard") {
  if (wordlist == "standard") {
    url <- "https://raw.githubusercontent.com/peterdalle/svensktext/master/stoppord/stoppord.csv"
  } else if (wordlist == "many") {
    url <- "https://raw.githubusercontent.com/peterdalle/svensktext/master/stoppord/stoppord-mycket.csv"
  } else if (wordlist == "politics") {
    url <- "https://raw.githubusercontent.com/peterdalle/svensktext/master/stoppord/stoppord-politik.csv"
  } else {
    stop(paste0("Argument 'wordlist' must be 'standard', 'many' or",
                " 'politics', not '", wordlist, "'."), call.=FALSE)
  }
  return(read.csv(url, header=TRUE, encoding="UTF-8", stringsAsFactors=FALSE,
                  col.names=c("word")))
}

path_parl = "/home/petter/data/ThesisCode/Topic Model/Data/RIksdagsledamöterNamn.csv"

df_parl = read.csv(path_parl, encoding="UTF-8", head=TRUE)

stopwords_politics = get_stopwords("politics") 
stopwords_mycket = get_stopwords("many") 

df_parl$Efternamn[-1637]
#create custom stopwords
stopwords_parliament = c("ulf", "annie", "karin", "lorentz", "fredrik", "göran", "magdalena", "åsa", "miljöpartiet", "socialdemokraterna", "vänsterpartiet", "liberalerna", "folkpartiet", "kristdemokraterna", "moderaterna", "sverigedemokraterna", "centerpartiet", "talman", "Talman", "Talmannen", "talmannen", "vice", "andra vice", "tredje vice", "ålderspresident", "ålderspresidenten", "herr", "fru", "representant") %>% as_tibble()
customstopwords = c(stopwords_politics$word, stopwords_mycket$word, df_parl$Efternamn[-1637], df_parl$Förnamn, stopwords_parliament$value) %>% tolower() %>% unique()

save(customstopwords, file="/home/petter/data/ThesisCode/Topic Model/RData/customstopwords.Rdata")

