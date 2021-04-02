library(rvest)
library(tidyr)
library(kableExtra)
library(clipr)

# developmental  For the textbook..the xml ".em"  appears to be the associated definition for these vocab words
 Ch1 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/01%3A_Introduction_to_Lifespan_Development')

 Ch2 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/02%3A_Heredity_Prenatal_Development_and_Birth')

 Ch3 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/03%3A_Infancy_and_Toddlerhood')

 Ch4 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/04%3A_Early_Childhood')

 Ch5 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/05%3A_Middle_and_Late_Childhood')

 Ch6 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/06%3A_Adolescence')

 Ch7 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/07%3A_Emerging_and_Early_Adulthood')

 Ch8 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/08%3A_Middle_Adulthood')

 Ch9 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/09%3A_Late_Adulthood')

 Ch10 <- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/10%3A_Death_and_Dying')

chaps <- list()
chaps <- list(Ch1,Ch2,Ch3,Ch4,Ch5,Ch6,Ch7,Ch8,Ch9,Ch10)

chapter_links <- list(1:length(chaps))
for(i in 1:length(chaps)){
  chapter_links[[i]]<- chaps[[i]]  %>% html_nodes('a') %>% html_attr('href')
}
for (i in 1:length(chapter_links)){
  chapter_links[[i]] <- chapter_links[[i]][-(1:17)]
  }

for (i in 1:length(chapter_links)){
  print(chapter_links[[i]][1])
}

# reduce last links so only sub chapters are included

chapter_links[[1]] <- chapter_links[[1]][1:10]
chapter_links[[2]] <- chapter_links[[2]][1:12]
chapter_links[[3]] <- chapter_links[[3]][1:13]
chapter_links[[4]] <- chapter_links[[4]][1:24]
chapter_links[[5]] <- chapter_links[[5]][1:14]
chapter_links[[6]] <- chapter_links[[6]][1:11]
chapter_links[[7]] <- chapter_links[[7]][1:16]
chapter_links[[8]] <- chapter_links[[8]][1:17]
chapter_links[[9]] <- chapter_links[[9]][1:24]
chapter_links[[10]] <- chapter_links[[10]][1:10]



terms <- vector(mode='list',length=length(chapter_links))
  for(i in 1:length(chapter_links[[3]])){

    read_html(chapter_links[[3]][i]) %>%
    html_nodes("strong") %>%
    html_text()-> terms[[i]]

    }


terms

save(terms,file='Ch3_lifespan_Vocab.RData')

lapply(terms, function(x) write.table( data.frame(x), 'Ch3_lifespan_Vocab.csv', append= T, sep=',' ))




columnNames <- c(rep(" ",length(chapter_links)))
terms <- vector(mode='list',length=length(chapter_links))
for(i in 1:length(chapter_links)){
  read_html(chapter_links[i]) %>%
    html_nodes("strong") %>%
    html_nodes("[class='glossary-term']")  %>%
    html_text() -> terms[[i]]
}
terms<- unlist(terms)
terms<- data.frame(matrix(terms,ncol=length(chapter_links)))
colnames(terms) <- columnNames
kbl(terms[,1]) %>% kable_classic(html_font = "Cambria",full_width=F,position="center",font_size=11)






# Learning objectives
Ch1<- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/01%3A_Introduction_to_Lifespan_Development')



  chapter_links[[i]]<- chaps[[i]]  %>% html_nodes('a') %>% html_attr('href')


  chapter_links <- list()
  for(i in 1:20){
    chapter_links[[i]]<- Ch1  %>% html_nodes('a') %>% html_attr('href')
  }
  for (i in 1:length(chapter_links)){
    chapter_links[[i]] <- chapter_links[[i]][-(1:17)]
  }

  chapter_links <- chapter_links[[1]][18:28]
  for (i in 1:length(chapter_links)){
    out <- read_html(chapter_links[[1]][1]) %>% html_nodes('*') %>% html_attr('class')
  }


learnObj<- read_html('https://socialsci.libretexts.org/Bookshelves/Human_Development/Book%3A_Lifespan_Development_-_A_Psychological_Perspective_(Lally_and_Valentine-French)/01%3A_Introduction_to_Lifespan_Development/1.08%3A_Research_Methods')

learnObj %>% html_nodes('p.boxtitle') %>% html_text()

learnObj %>% html_nodes(xpath="//p[@class='boxtitle']") %>% html_text()
