library(multilinguer)
library(rJava)
library(ggplot2)
library(plyr)
library(stringr)
library(hash)
library(tau)
library(Sejong)
library(RSQLite)
library(devtools)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(udpipe)
library(NLP)
library(tm)
library(cld3)
library(textcat)
library(lattice)
library(ggplot2)
#Open the file        
germany_eng = file('Germany_eng.txt', open = 'r')
line = readLines(germany_eng)
for (i in 1:length(line)){
  print(line[i])
}
close(germany_eng)

# Data Processing
line <- tolower(line)
line <- gsub("\\W", " ", line )
line <- removePunctuation(line)
line <- removeWords(line, stopwords('german'))
line <- removeWords(line, stopwords("english"))
line <- removeWords(line, c("is","'s", "am", "are", 'was', 'were', 'germany', 'people', 'merkel','the', 'The','be', 'and','that', 'will','with','said','for','schools','school','has', 'have', 'may','can', 'should', 'from', 'who', 'Corona', 'coronavirus','covid19','covid', 'virus','Coronavirus', 'they','she','he','students','student','children', 'child', 'kid','kids','could','would', 'must', 'may','might'))
line <- removeWords(line, "'s")

#text mining with the package 'udpipe'
ud_model_en <- udpipe_download_model(language = "english") #download English text mining file
ud_model_en <- udpipe_load_model(ud_model_en$file_model) #slicing only necessary parts
line_eng <- udpipe_annotate(ud_model_en, x = line, trace = T) #POS-Tagging for each word
line_eng <- as.data.frame(line_eng) #Convert it to data frame
line_eng_noun <- subset(line_eng, upos %in% 'NOUN') #subtract only the words which are noun
line_eng_noun <- txt_freq(x = line_eng$lemma) #analyzing the frequency of each word
line_eng_adj <- subset(line_eng, upos %in% 'ADJ') #subtract only the words which are adjective
line_eng_adj <- txt_freq(x = line_eng_adj$lemma) #analyzing the frequency of each word
line_eng_noun_top <- line_eng_noun %>%  subset(line_eng_noun$freq>=median(line_eng_noun$freq)*10) #Filtering the words which have frequency more than a criteria 
ggplot(line_eng_noun_top, aes(x=reorder(key, -freq), y = freq)) +geom_bar(fill = 'skyblue',position = 'dodge', stat ='identity') + theme(axis.text.x = element_text(angle = 45)) + ggtitle("Germay_news Analysis (English)") + labs(x ='count', y = 'word') #Creating a ggplot

Keywords_eng <- keywords_rake(x = line_eng, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                             relevant =line_eng$upos %in% c("NOUN"),
                             ngram_max = 3) #Rake(Rapid Automatic Keywords Extraction) #RAKE analyzing of the words
Keywords_eng$key <- factor(Keywords_eng$keyword, levels = rev(Keywords_eng$keyword)) 
Keywords_eng_barchart <- barchart(key ~rake, data= head(subset(Keywords_eng, freq >0.5), 30), col = "green", 
                                 main  = "Keywords_eng", xlab = "Rake")  #Creating a bar chart for RAKE analysis
palette <- brewer.pal(5, 'Set1') #Set a color palette for wordclouds
dev.new(width = 10000, height = 10000, unit = "px") #Seting a new page for wordclouds
Keywords_en_wordcloud <- wordcloud(words = Keywords_eng$key, freq = Keywords_eng$freq,min.freq = 1,random.order = FALSE, 
                                   random.color=T, colors = palette, rot.per = .5, max.words =300)  #wordclouds for the english words
dev.new(width = 1000, height = 1000, unit = "px") #Setting a new page for wordclouds
Noun_en_wordcloud <- wordcloud(words = line_eng_noun$key, freq=Keywords_eng$freq, 
                       random.order = F, random.color = T, min.freq = 0.01, 
                       rot.per = .5,colors =palette,max.words = 300) #

############

germany_ger = file('Germany_ger.txt', open = 'r')
line_ger = readLines(germany_ger)
for (i in 1:length(line_ger)){
  print(line_ger[i])
}
close(germany_eng)

line_ger <- tolower(line_ger)
line_ger <- gsub("\\W", " ", line_ger )
line_ger <- removePunctuation(line_ger)
line_ger <- stripWhitespace(line_ger)
line_ger <- removeWords(line_ger, stopwords('german'))
line_ger <-removeWords(line_ger, stopwords("english")) 
line_ger <- removeWords(line_ger, c('schulen','schul','kitas','dass',"corona", "2020", "ist", "bin",'lange','müssen','lang' ,'giffey',"sind", 'war', 'waren', 'deutschland', 'Deustche', 'merkel','bund', 'dürfen','schüler', 'schülerinnen','schülerin', 'Schule','ministerpräsident','insgesamt','04','school','has', 'have', 'may','can', 'should', 'from', 'who', 'Corona', 'coronavirus','covid19','covid', 'virus','Coronavirus', 'they','she','he','students','student','children', 'child', 'kid','kids','could','would', 'must', 'may','might','der', 'hessen','baden','1','193','8','11','besonder','schulöffnung', 'kind',"Kind","Kinder",'kinder','Symposion','20'))

ud_model_ger <- udpipe_download_model(language = "german") 
ud_model_ger <- udpipe_load_model(ud_model_ger$file_model)
line_de <- udpipe_annotate(ud_model_ger, x = line_ger, trace = T)
line_de <- as.data.frame(line_de)
line_de_noun <- subset(line_de, upos %in% 'NOUN')
line_de_noun <- txt_freq(x = line_de$lemma)
line_de_adj <- subset(line_de, upos %in% 'ADJ')
line_de_adj <- txt_freq(x = line_de_adj$lemma)

line_de_noun_top <- line_de_noun %>%  subset(line_de_noun$freq>=median(line_de_noun$freq)*25)
ggplot(line_de_noun_top, aes(x=reorder(key, -freq), y = freq)) +geom_bar(fill = 'skyblue',position = 'dodge', stat ='identity') + theme(axis.text.x = element_text(angle = 45)) + ggtitle("Germany_news Analysis (German)") + labs(x = 'Words', y = 'Count')

Keywords_ger <- keywords_rake(x = line_de, term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              relevant =line_de$upos %in% c("NOUN", "ADJ"),
                              ngram_max = 3) #Rake(Rapid Automatic Keywords Extraction)
Keywords_ger$key <- factor(Keywords_ger$keyword, levels = rev(Keywords_ger$keyword))
Keywords_ger_barchart <- barchart(key ~rake, data= head(subset(Keywords_ger, freq >3), 30), col = "green", 
                                  main  = "Keywords_German", xlab = "Rake") 
palette <- brewer.pal(5, 'Set1')
dev.new(width = 1000, height = 1000, unit = "in")
Keywords_ger_wordcloud <- wordcloud(words = Keywords_ger$key, freq = Keywords_ger$freq,min.freq = 1,random.order = FALSE, rot.per=0.5,
                                   random.color=T, colors = palette,max.words = 100) 
Noun_ger_wordcloud <- wordcloud(words = line_de_noun$key, freq = Keywords_ger$freq, min.freq =1, random.order = F, rot.per = 0.1, colors =palette )