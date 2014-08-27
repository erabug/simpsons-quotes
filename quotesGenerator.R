library(XML); library(stringr)

#---------------------------------------#
#           Get & clean HTML            #
#---------------------------------------#

# read the html from the episode webpage
getHtml <- function(episodeCode) {
    con <- url(paste0("http://www.simpsonsarchive.com/episodes/", episodeCode, ".html"))
    htmlCode <- readLines(con)
    close(con)
    return(htmlCode)
}

# grab the title (and alt title, if exists) from the html
getTitle <- function(htmlCode) {
    location <- grep("<PRE>", htmlCode)[1] + 1
    start <- grep("<TITLE>", htmlCode, value=T)
    ifelse(length(start) == 0,
           title <- str_trim(strsplit(htmlCode[location], "Written")[[1]][1]),
           title <- str_trim(strsplit(start, "<TITLE>")[[1]][2]))
    altTitle <- htmlCode[location + 1]
    if (grepl("a.k.a.", altTitle)) {
        altTitle <- substr(altTitle, grepRaw("a.k.a", altTitle) + 7, nchar(altTitle) - 1)
        title <- append(title, altTitle)
    }
    return(title)
}

# strip and parse quotes section from the html
getQuotesHtml <- function(htmlCode) {
    headings <- grep("<H1>", htmlCode)
    start <- grep("Quotes and scene summary", htmlCode) + 2
    end <- headings[grep(start - 2, headings) + 1] - 2
    if (is.na(end)) end <- rev(grep("</EM>", htmlCode))[1] - 1
    quotesHtml <- htmlTreeParse(htmlCode[start:end], useInternalNodes=T)
    return(quotesHtml)
}

# check for multiples in a raw quote
checkMultiples <- function(rawQuote, episodeTitle) {
    if (length(grepRaw(episodeTitle, rawQuote, all=T)) > 1) {
        return("true")
    }
}

# find duds in a raw quote (those that have weird characters or are empty)
findDuds <- function(rawQuote) {
    if (str_trim(rawQuote) == "" | grepl("\\|", rawQuote)) return("dud")
}

# parses raw quotes, splits multiples and removes duds
cleanRawQuotes <- function(rawQuotes, episodeTitle) {
    duds <- grep("dud", lapply(rawQuotes, findDuds))
    if (length(duds) > 0) rawQuotes <- rawQuotes[-duds]
    multiples <- grep("true", lapply(rawQuotes, checkMultiples, episodeTitle))
    titleLength <- nchar(episodeTitle)
    while (length(multiples) > 0) {   
        quote <- rawQuotes[multiples[1]]
        titles <- grepRaw(episodeTitle, quote, all=T)    
        for (title in titles) {
            ifelse(title == titles[1], {
                newQuote <- substr(quote, 0, title + titleLength)
                rawQuotes <- replace(rawQuotes, multiples[1], newQuote)
            },{
                start <- titles[grep(title, titles) - 1] + titleLength
                newQuote <- substr(quote, start, title + titleLength)
                rawQuotes <- append(rawQuotes, newQuote, after = multiples[1])
            })
        }
        multiples <- grep("true", lapply(rawQuotes, checkMultiples, episodeTitle))
    }
    return(rawQuotes)
}

# strips out quotes from quotes html, checks them, returns list of clean quotes
parseQuotes <- function(htmlCode, episodeTitle) {
    quotesHtml <- getQuotesHtml(htmlCode)
    rawQuotes <- xpathSApply(quotesHtml, "//text()[not(ancestor::em)]", xmlValue) 
    cleanQuotes <- cleanRawQuotes(gsub("`", "'", rawQuotes), episodeTitle)
    return(cleanQuotes)
}

#---------------------------------------#
#         Split & clean quotes          #
#---------------------------------------#

# trims quote text
trim <- function(quote) {
    while (length(grepRaw("\n| --|^:|^''|\\\\", quote)) > 0) {
        quote <- str_trim(gsub("\n|(?<= ) | --|^:|^''|\\\\", "", quote, perl=T))
    }
    return(quote)
}

# trims speaker text
trimSpeaker <- function(quote) {
    while (length(grepRaw(" --|''$|'$|^''|^'|,$|\n| - ", quote)) > 0) {
        quote <- str_trim(gsub(" --|(?<= ) |''$|'$|^''|^'|,$|\n| - ", "", quote, perl=T))
    }
    return(quote)
}

# separates speakers within a quote, returns vector of quotes
splitSpeakers <- function(quote) {
    newLines <- grepRaw("\n", quote, all=T)
    semis <- grepRaw(": |:\n", quote, all=T)
    starts <- vector()   
    for (semi in semis) {
        start <- (grep(newLines[newLines > semi][1], newLines) - 1)[1]
        starts <- append(starts, start)  
    } 
    indlQuotes <- vector()
    starts <- unique(starts)
    for (start in starts) {
        nextStart <- grep(start, starts) + 1
        ifelse(is.na(starts[nextStart]), 
               end <- nchar(quote), 
               end <- newLines[starts[nextStart]])
        indl <- trim(substr(quote, newLines[start], end))
        indlQuotes <- append(indlQuotes, indl)
    }  
    ifelse(length(semis) == 0, return(""), return(indlQuotes))
}

# returns quote and speaker as a list
makeList <- function(quote, speaker) { 
    row <- list(quote = quote, speaker = speaker)
    return(row) 
}

# parses a raw quote, splits into quote and speaker, returns trimmed list
stripQuote <- function(string, episodeTitle) {
    quoteEnd <- grepRaw(" --| - ", string) #used to have fixed=T
    speakerEnd <- grepRaw(episodeTitle, string) - 1  
    if (length(grepRaw(episodeTitle, string)) == 0) {
        ifelse(grepl("''", string),
               speakerEnd <- rev(grepRaw("''", string, all=T))[2],
               speakerEnd <- nchar(string))
    }   
    quote <- substr(string, 0, quoteEnd)   
    ifelse(grepl(": ", quote) & 
               splitSpeakers(quote) != "" & 
               length(grepRaw(": ", substr(quote, 1, 50))) != 0, {
                   quote <- splitSpeakers(quote)
               }, quote <- trim(quote))    
    speaker <- trimSpeaker(substr(string, quoteEnd, speakerEnd))    
    makeList(quote, speaker)
}

# master function, returns clean list of quote lists from an episode
quotesFromEpisode <- function(episodeCode) {
    htmlCode <- getHtml(episodeCode)
    if (length(grep("<HTML>", htmlCode)) == 0) {
        stop("Episode HTML is not in proper format")
        next
    }
    episodeTitle <- getTitle(htmlCode)
    if (length(episodeTitle) > 1) {
        ep1 <- length(grep(episodeTitle[1], htmlCode))
        ep2 <- length(grep(episodeTitle[2], htmlCode))
        ifelse(ep1 > ep2, 
               episodeTitle <- episodeTitle[1], 
               episodeTitle <- episodeTitle[2])
    }
    quotes <- lapply(parseQuotes(htmlCode, episodeTitle), stripQuote, episodeTitle)
    return(quotes)
}

#---------------------------------------#
#          Quotes data frames           #
#---------------------------------------#

# determines the episode codes for a given season
getEpisodeCodes <- function(season) {
    codes <- c()
    if (season == 1) {prefix <- "7G"; num <- 13}
    if (season == 2) {prefix <- "7F"; num <- 22}
    if (season == 3) {prefix <- "8F"; num <- 22}
    if (season == 4) {prefix <- "9F"; num <- 20}
    for (i in 1:num) {
        if (i < 10) codes <- append(codes, paste0(prefix, 0, i))
        if (i > 9) codes <- append(codes, paste0(prefix, i)) 
    }
    if (season == 3) {
        remove <- "8F18"
        codes <- append(codes[! codes %in% remove], c("7F23", "7F24"))
    }
    if (season == 4) codes <- append(codes, c("8F18", "8F24"))
    return(codes)
}

# returns a two-column data frame from an episode quote list
makeDF <- function(quoteList) {
    row <- data.frame(cbind(quote = quoteList[1], speaker = unlist(quoteList[2])), 
                      stringsAsFactors = FALSE, row.names = NULL)
    return(row)
}

# generate quote lists for an entire season, return as a four-column data frame
makeSeasonDF <- function(season) {
    episodeCodes <- getEpisodeCodes(season)
    bigDF <- data.frame()
    for (episode in episodeCodes) {
        try({
            data <- quotesFromEpisode(episode)
            df <- data.frame()
            df <- do.call("rbind", lapply(data, makeDF))
            df$episode <- getTitle(getHtml(episode))[1]
            df$season <- paste("Season", season)
            bigDF <- rbind(bigDF, df[c("season", "episode", "quote", "speaker")])
        })
    }
    return(bigDF)
}

# binds together quotes data frames for given seasons
makeQuotesDF <- function(seasons) {
    quotesDF <- data.frame()
    for (season in seasons) {
        quotesDF <- rbind(quotesDF, makeSeasonDF(season))
    }
    return(quotesDF)
}

#---------------------------------------#
#           Random generator            #
#---------------------------------------#

# sample and print ten random quotes from a given quotes data frame
sampleFromSeason <- function(seasonQuotes) {
    sampleQ <- seasonQuotes[sample(nrow(seasonQuotes), size=10),]
    for (i in 1:10) {
        cat(unlist(sampleQ[i,3]), sep = "\n")
        cat("-- ", 
            if (sampleQ[i,4] != "") paste0(sampleQ[i,4], ", "), 
            "\"", sampleQ[i,2], "\" (", sampleQ[i, 1], ")", rep("\n",3), sep = "")
    }
}

#---------------------------------------#
#               Testing                 #
#---------------------------------------#

# create a DF of quotes from season X
quotesDF <- makeSeasonDF(2)

# create a DF of quotes from multiple seasons
quotesDF <- makeQuotesDF(1:2)

# sample from the quotes DF
sampleFromSeason(quotesDF)

# get a list of quote lists from episode
quotesFromEpisode("8F23")

# for testing
episodeCode <- "8F07"

# KNOWN ERRORS
# 7F04: Halloween special divided into three acts. Also a ton of \ for poetry.
# 7F14: 25, 28 TYPOS IN THE TITLES
# 7F18: 43
# 7F21: 33
# 8F22
# 8F23

# playing with JSON
library(RJSONIO)

# my version
x <- toJSON(quotesDF[1, ]) # just one row
x <- apply(quotesDF, 1, toJSON) # whole df
cat(x)

writeLines(x, "quotes.JSON")
