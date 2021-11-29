#install.packages('rsconnect')

library(dplyr)
library(tidytext)
library(ggplot2)
library(textstem)
library(readr)
library(stringr)

library(shiny)
library(DT)
library(rvest)
library(jsonlite)

library(rsconnect)

CountTech <- function(){
  
  jobtitles <- read.csv("Job_titles.csv", TRUE,",")
  technologylist <- read.csv("technologies_list.csv", TRUE,",")
  
  categories <- unique(technologylist$Category)
  
  countKeyword <- function(text_to_view,keyword){
    # a function to count how many times the keyword in the technology list
    # appears in the text-to-view. 
    text_to_view <- tolower(text_to_view)
    keyword <- tolower(keyword)
    ct = 0 # count of keyword
    if (nchar(keyword) < 5){
      ct = ct + str_count(text_to_view,
                          fixed(paste0(' ',keyword,'.'))
      )
      ct = ct + str_count(text_to_view,
                          fixed(paste0(' ',keyword,','))
      )
      ct = ct + str_count(text_to_view,
                          fixed(paste0(' ',keyword,' '))
      )
    } else if (str_count(keyword,fixed('/'))){
      kw = str_split(keyword,'/',simplify = T)
      for (i in kw){
        ct = ct + countKeyword(text_to_view,str_trim(i))
      }
    }
    else {
      ct = str_count(text_to_view,fixed(keyword))
    }
    ct
  }
  
  #search job
  searchJob <- function(job, n){
    # Create a Progress object
    #progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    #on.exit(progress$close())
    
    #progress$set(message = "Crawling:", value = 0)
    count <- ceiling(n/15)
    job_companies <- c()
    job_titles <- c()
    job_urls <- c()
    job_descs <- c()
    for(i in 0:(count-1)){
      #progress$inc((i)/count, detail = paste0("https://www.indeed.com/jobs?q=", URLencode(job), "&start=", i*15))
      print(paste0("https://www.indeed.com/jobs?q=", job, "&start=", i*15))
      page = read_html(paste0("https://www.indeed.com/jobs?q=", URLencode(job), "&start=", i*15))
      jobcards <- html_node(page, "#mosaic-provider-jobcards")
      job_links <- html_nodes(jobcards, 'a[id^="job"]')
      if (length(job_links)){
        titles <- rep(NA, length(job_links))
        names <-  rep(NA, length(job_links))
        urls <-  rep(NA, length(job_links))
        descs  <- rep(NA, length(job_links))
        for(k in 1:length(job_links)){
          link <- job_links[k]
          h2 <- html_node(link, "h2.jobTitle")
          spans <- html_nodes(h2, "span")
          titles[k] <- html_text(spans[length(spans)])
          names[k] <- link %>% html_node("span.companyName") %>% html_text()
          urls[k] <- paste0("https://www.indeed.com", html_attr(link, "href"))
        }
        for(j in 1:length(urls)){
          page2 <- read_html(urls[j])
          desc <- page2 %>% html_node("div#jobDescriptionText") %>% html_text()
          descs[j] <- desc
        }
        job_companies <- c(job_companies,  names)
        job_titles <- c(job_titles, titles)
        job_urls <- c(job_urls, urls)
        job_descs <- c(job_descs, descs)
      } else {
        job_descs <- ''
      }
    }
    job_descs # the returned value of searchJob
  }# definition of searchJob ends
  
  
  # table for counting freq. of all techs
  tech=unique(technologylist$Technology)
  count_tech_total=numeric(length(tech))
  for (job in jobtitles$Job_Title){  # Run it one by one by change the code to: for (job in jobtitles$Job_Title[1])
    desp_text <- searchJob(job,50) %>% paste(collapse = '')
    count_tech=numeric(length(tech))
    for (i in 1:length(tech)){
      count_tech[i] = countKeyword(desp_text,tech[i]) # count keyword in that job's descriptions
    }
    count_tech_total <- count_tech_total + count_tech
  }
  
  
  
  df = data.frame(tech,count_tech_total)
  df %>% write.csv('technologies_counted.csv',row.names = F)
  
  # from df to fill the freq. in technologylist
  for (i in categories){
    tech_in_cate = technologylist[technologylist$Category==i,'Technology']
    for (j in tech_in_cate){
      technologylist[technologylist$Category==i & technologylist$Technology==j,'Frequency']=df[df$tech==j,'count_tech_total']
    }
  }
  technologylist %>% write.csv('freq.csv')
  
  # plot the top 10 hot technologies
  df[order(-df$count_tech_total),][1:10,] %>% 
    ggplot(aes(x=reorder(tech,desc(count_tech_total)),y=count_tech_total))+ 
    geom_bar(stat = "identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1))+ 
    labs(title="Mentioned times of technologies in job descriptions", 
         x="Technology", y = "Frequency")
  
}



