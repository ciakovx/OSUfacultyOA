library("rscopus")
library("rcrossref")
library("stringr")
library("dplyr")
library("roadoi")
library("tidyr")

setwd("W:/Scholarly Communications/Scopus/OSUfacultyOA")
rscopus::set_api_key("MY API KEY")

# create the query
Query <- paste0("AF-ID(60006514) AND SUBJAREA(", subject_areas(), ") AND PUBYEAR = 2018 AND ACCESSTYPE(OA)")

for(i in 1:length(Query)){
  # this is the initial data pull per subject area, using the Query established above
  completeArticle <- scopus_search(
    query = Query[i]
    , view = c("COMPLETE")
    , count = 200)
  
  # first, we only want to proceed if there are more than zero total results
  if(completeArticle[["total_results"]] > 0) {
    
    # This gets the four main dataframes into a list
    Entries <- gen_entries_to_df(completeArticle[["entries"]])
    
    # This gets the main article data. We can't stop here because it only pulls the first author
    MainEntry <- gen_entries_to_df(completeArticle[["entries"]])[["df"]]
    
    # This gets Author data into a dataframe
    Authors <- Entries[["author"]]
    Affiliation <- Entries[["affiliation"]]
    
    # first filter to get only OSU authors
    osuauth <- Authors %>%
      filter(`afid.$` == "60006514")
    # then filter to get rid of duplicates entries (dupes based on same info in author name and entry_number)
    osuauth <- osuauth %>% filter(!duplicated(osuauth[c("authname", "entry_number")]))
    
    # then get only the author names
    osuauth <- osuauth %>%  
      select(entry_number, authname) %>%
      group_by(entry_number) %>%
      mutate(ind = row_number()) %>%  # have to do this so the spread works
      spread(entry_number, authname) %>%
      select(-ind)
    
    # turn it into a proper df
    x <- as.data.frame(t(as.matrix(osuauth)))  # transpose it
    names(x) <- paste0("auth", seq(nrow(osuauth)))  # names are based on number of authors
    x$entry_number <- names(osuauth)  # entry_number column is based on the column names from the spread
    
    # join the author data to the main entry data
    final <- left_join(MainEntry
                       , x
                       , "entry_number")
    
    # if there are 100+ authors, you have to use the abstract_retrieval function to get the full author data
    # coerce to integer first
    MainEntry$`author-count.$` <- as.integer(MainEntry$`author-count.$`)
    # then if any of the entries have author counts above 99, proceed to filter the df to those
    if(any(MainEntry$`author-count.$` > 99)){
      MainEntry_99auth <- MainEntry %>%
        filter(`author-count.$` > 99)
      # create empty list
      osuAuthList <- list()
      
      # loop through the 100+ authors df
      for(k in 1:nrow(MainEntry_99auth)){
        completeArticle2 <- abstract_retrieval(id = MainEntry_99auth$`prism:doi`[k]
                                               , identifier = "doi"
                                               , verbose = FALSE)
        # get the article data, then filter the authors to include only those with OSU affiliation IDF
        MainEntry2 <- gen_entries_to_df(completeArticle2[["content"]][["abstracts-retrieval-response"]][["authors"]][["author"]])
        OSUauthindex <- which(MainEntry2[["df"]][["affiliation.@id"]] == "60006514")
        OSUauthor <- unique(MainEntry2[["df"]][["ce:indexed-name"]][OSUauthindex])
        OSUauthor <- as.data.frame(t(data.frame(OSUauthor, stringsAsFactors = F)), stringsAsFactors = F)
        
        # there are some errors in the data, this accounts for that
        ifelse(length(OSUauthor) > 0
               , names(OSUauthor) <- paste0("auth", 1:length(OSUauthor))
               , OSUauthor$auth <- NA)
        
        # add in the entry_number and put it into the list
        OSUauthor$entry_number = as.character(MainEntry_99auth$entry_number[k])
        osuAuthList[[k]] <- OSUauthor
      }
      
      # get the list into a dataframe and join it to the main df data
      osuAuthListFinal <- osuAuthList %>%
        bind_rows() %>%
        select(entry_number, everything())
      
      final_99auth <- left_join(MainEntry_99auth
                                , osuAuthListFinal
                                , "entry_number")
      
      # get rid of the other entries and go forward
      final_auth <- final %>%
        mutate(`author-count.$` = as.integer(`author-count.$`)) %>%
        filter(`author-count.$` <= 99)
      final2 <- bind_rows(final_auth, final_99auth)
      
      # add subject area to the df
      subj <- subject_areas()[i]
      final2$subject.area <- rep(subj, nrow(final2))
      
      write.csv(final2, paste0("./data/results/subject/", subj, "_Entries.csv"), row.names = F)
    } else {
      
      # this is back if we don't have 100+ authors, just adding the subject areas and write the CSV
      subj <- subject_areas()[i]
      final$subject.area <- rep(subj, nrow(final))
      
      write.csv(final, paste0("./data/results/subject/", subj, "_Entries.csv"), row.names = F)
    } # closes else loop
  } # closes the total results > 0 loop
}




############################################
# Getting the data from all subject areas into one spreadsheet

osuAuthfiles <- list.files("./data/results/subject")
myfiles = lapply(osuAuthfiles, function(x) read.csv(file.path("./data/results/subject", x)
                                                    , stringsAsFactors = F
                                                    , colClasses = c("prism.issn" = "character"
                                                                     , "prism.eIssn" = "character"
                                                                     , "prism.issueIdentifier" = "character"
                                                                     , "article.number" = "character"
                                                                     , "X._fa" = "character")))
myfiles2 <- bind_rows(myfiles)
myfiles2 <- filter(myfiles2, !duplicated(dc.identifier))
# write.csv(myfiles2, "./data/results/osuAuthors.csv", row.names = F)

##########################
# analysis

osuOA <- read.csv("./data/results/osuAuthors.csv"
                  , stringsAsFactors = F)
# write.csv(osuOA, "./data/results/osuOAfinal.csv", row.names = F)

# checking for dupes with DOI
osuOADOI <- osuOA %>%
  group_by(prism.doi) %>%
  summarize("n" = n()) %>%
  arrange(desc(n))

# checking for duplicates
# r <- filter(osuOA, prism.doi == "10.1016/j.physletb.2018.10.021")
# length(unique(osuOA$dc.identifier))
# View(as.data.frame(names(osuOA)))
# osuOA <- distinct(osuOA, prism.doi, .keep_all = T)


authcols <- names(osuOA[, c(33:43)])

# publications per author
authors <- osuOA %>%
  select(authcols) %>%
  stack() %>%
  filter(!is.na(values)) %>%
  group_by(values) %>%
  summarize("number" = n()) %>%
  arrange(desc(number))

# publications per subject
osuSubjects <- osuOA %>%
  group_by(subject.area) %>%
  summarise("quantity" = n()) %>%
  arrange(desc(quantity))

# publications per journal
osuPublications <- osuOA %>%
  group_by(prism.publicationName) %>%
  summarise("quantity" = n()) %>%
  arrange(desc(quantity))

# authDetector <- names(osuOA[, which(str_detect(osuOA, "Zhang G."))])
# auth <- filter(osuOA, auth1 == "Zhang G." | auth2 == "Zhang G." | auth7== "Zhang G.")

# sort by number of citations
osuOA <- arrange(osuOA, desc(citedby.count))

# publications per funder
osuFunders <- osuOA %>%
  group_by(fund.sponsor) %>%
  summarise("quantity" = n()) %>%
  arrange(desc(quantity))

#
funderPoland <- filter(osuOA, fund.sponsor == "Ministerstwo Nauki i Szkolnictwa Wyzszego")  

# number of authors per article
authCount <- osuOA %>%
  group_by(author.count..) %>%
  summarize("number" = n()) %>%
  arrange(desc(number))

# removing high author counts (high energy physics articles)
authCount2 <- osuOA %>%
  filter(author.count.. < 11) %>%
  group_by(subject.area) %>%
  summarize("number" = n()) %>%
  arrange(desc(number))


