library(shiny)
library(bs4Dash)
#library(googlesheets4)
library(tidyverse)
library(highcharter)
library(waiter)
library(DT)
library(tidytext)
#library(ggwordcloud)
library(jsonlite)
#library(hackeRnews)
#future::plan(future::multiprocess)
library(urltools)


shiny::shinyApp(
  ui = bs4DashPage(
  
    old_school = FALSE,
    sidebar_collapsed = TRUE,
    controlbar_collapsed = TRUE,
    title = "HN Content Analysis",
    navbar = bs4DashNavbar(
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = "bars",
      fixed = TRUE
      # leftUi = bs4DropdownMenu(
      #   show = TRUE,
      #   align = "left",
      #   status = "warning", 
      #   menuIcon = "envelope-open",
      #   src = NULL
      # ),
      # rightUi = bs4DropdownMenu(
      #   show = FALSE,
      #   status = "danger",
      #   src = "https://www.google.fr"
      # )
    ),
    sidebar = bs4DashSidebar(
      skin = "light",
      status = "primary",
      title = "HN Content Analysis",
      brandColor = "primary",
      #url = "https://www.google.fr",
      src = "https://assets.paystack.com/assets/img/logos/merchants/_150x150_crop_center-center_30_line/ycombinator-logo.png",
      elevation = 3,
      opacity = 0.8,
      #bs4SidebarUserPanel(
       # img = "https://image.flaticon.com/icons/svg/1149/1149168.svg", 
        #text = "Welcome!"
      
      #),
      
      bs4SidebarMenu(
        #bs4SidebarHeader("Header 1"),
        
        bs4SidebarMenuItem(
          "Content Stats",
          tabName = "content1",
          icon = "chart-area"
        ),
        bs4SidebarMenuItem(
          "Raw Entries",
          tabName = "item2",
          icon = "id-card"
        )
        
      )
    ),
  
    footer = bs4DashFooter(
      copyrights = a(
        href = "https://www.github.com/amrrs", 
        target = "_blank", "MEANguys"
      ),
      right_text = "2020"
    ),
    body = bs4DashBody(
      use_waiter(), # include dependencies
      tags$style(HTML(".inner {font-size: 1.5rem}")),
      bs4TabItems(
        bs4TabItem(
          tabName = "donation0",
          fluidRow(
            #bs4Box(
              #title = "Box 1",
              
            #)
            
          )
        ),
        bs4TabItem(
          tabName = "rawdata",
          fluidRow(
              bs4Jumbotron(
      title = "Raw Data from Hacker News",
      btn_name = 'Hacker News',
      href = "https://news.ycombinator.com/"
      ),
            
             # tags$iframe(src="https://docs.google.com/forms/d/e/1FAIpQLSfhw_l17P3Krh0QjYonMfiIv-eqcbwcmyuwPW0CKfb6Vne3aw/viewform?embedded=true",width="640",height="4713",frameborder="0",marginheight="0",marginwidth="0")
            #)
            
          )
        ),
      bs4TabItem(
          tabName = "logistics",
             
      
          fluidRow(
    bs4SocialCard(
      title = "Logistics Partner",
      #subtitle = "example-01.05.2018",
      src = "https://adminlte.io/themes/AdminLTE/dist/img/user4-128x128.jpg",
      "Some text here!",
      comments = tagList(
        lapply(X = 1:10, FUN = function(i) {
          cardComment(
            src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
            title = paste("Comment", i),
            date = "01.05.2018",
            paste0("The ", i, "-th comment")
          )
        })
      ),
      footer = "Here"
    ),
    bs4Card(
      title = "Discussions",
      status = "primary",
      userPost(
        id = 1,
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
        author = "Jonathan Burke Jr.",
        description = "Shared publicly - 7:30 PM today",
        "Lorem ipsum represents a long-held tradition for designers, 
       typographers and the like. Some people hate it and argue for 
       its demise, but others ignore the hate as they create awesome 
       tools to help create filler text for everyone from bacon 
       lovers to Charlie Sheen fans.",
        userPostTagItems(
          userPostTagItem(bs4Badge("item 1", status = "warning")),
          userPostTagItem(bs4Badge("item 2", status = "danger"))
        )
      ),
      userPost(
        id = 2,
        src = "https://adminlte.io/themes/AdminLTE/dist/img/user6-128x128.jpg",
        author = "Adam Jones",
        description = "Shared publicly - 5 days ago",
        userPostMedia(src = "https://adminlte.io/themes/AdminLTE/dist/img/photo2.png"),
        userPostTagItems(
          userPostTagItem(bs4Badge("item 1", status = "info")),
          userPostTagItem(bs4Badge("item 2", status = "danger"))
        )
      )
    )
  )
            
        ),
        bs4TabItem(
          tabName = "content1",
          fluidRow(
    bs4ValueBox(
      
      value = textOutput("total_resp"),

      subtitle = "Top HN Entries (90th Percentile)",
      status = "primary",
      icon = "hands-helping"
    ),

    bs4ValueBox(
      value = textOutput("min_time"),
      subtitle = "Earliest Date",
      status = "warning",
      icon = "android"
    ),
    bs4ValueBox(
      value = textOutput("max_time"),
      subtitle = "Latest Date",
      status = "warning",
      icon = "time"
    ),
    bs4ValueBox(
      value = textOutput("highest_score"),
      subtitle = "Highest Score",
      status = "success",
      icon = "time"
    )
  ),
          fluidRow(
            bs4Box(
              width = 6,
              #title = "What's the highest connectivity your smartphone has got?",
              highchartOutput("words")
            ),
            bs4Box(
              width = 6,
              #title = "What's the highest connectivity your smartphone has got?",
              highchartOutput("top_users")
            )),
          fluidRow(
            bs4Box(
              width = 12,
              #title = "Box 2",
              highchartOutput("domains")
            )
          ),
  fluidRow(
    bs4Box(
      width = 12,
      #title = "Box 2",
      highchartOutput("bigrams")
    )
  )
        ),
        bs4TabItem(
          tabName = "item2",
          bs4Box(
            width = 12,
            dataTableOutput("entries_all")
          )
        )
      )
    )
  ),
  server = function(input, output) {
    
    data('stop_words')
    
    w <- Waiter$new(id = c("top_users", 'domains','words'))
    
    entries <- reactive({
      
      w$show()
      
      #w$update(html = 'Loading Charts...')
      
      
      #articles <- fromJSON(readLines('hackathon/data/articles.json'),
       #                    simplifyVector = TRUE)
      
      
      articles <- fromJSON(readLines('hackathon/data/articles.json'),
                           simplifyVector = TRUE)
      
      
      articles_df <- as.data.frame(lapply(articles[colnames(articles)], as.character))
      
      #xtracting top stories
      
      articles_df <- articles_df %>% 
        filter(score >= quantile(as.numeric(articles_df$score),0.9))
      
      articles_df %>% 
        #mutate(domain_full = domain(articles_df$url)) %>%
        bind_cols(suffix_extract(domain(articles_df$url))) 
      
    })
    
    title_word_freq <- reactive({
      entries()  %>%
        select(c("title","score")) %>%
        mutate(linenumber = row_number()) %>%
        unnest_tokens(word, title) %>%
        filter(str_length(word) > 0 & !str_to_lower(word) %in% stop_words$word) %>%
        filter(!(str_detect(word,'[[:digit:]]') & str_length(word) ==2)) %>%
        count(word, sort = TRUE)
    })
    
    title_word2_freq <- reactive({
      entries()  %>%
        select(c("title","score")) %>%
        mutate(linenumber = row_number()) %>%
        unnest_tokens(word, title, token = "ngrams", n = 2) %>%
        filter(str_length(word) > 0 & !str_to_lower(word) %in% stop_words$word) %>%
        filter(!(str_detect(word,'[[:digit:]]') & str_length(word) ==2)) %>%
        count(word, sort = TRUE)
    })
    
    
    output$entries_all <- renderDataTable(entries())
    
    output$total_resp <- renderText(nrow(entries())) 
    
    output$min_time <- renderText(substr(min(entries()$time),0,10))
    
    output$max_time <- renderText(substr(max(entries()$time),0,10))
    
    output$highest_score <- renderText(max(entries()$score))
    

    
    
    
    output$top_users <- renderHighchart(
      entries() %>%
        count(by, sort = TRUE)  %>% #head(50)
        head(50) %>%
        hchart('column', hcaes(x = 'by', y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Most popular users (by top stories count)")
      
      
      
    )
    
    output$domains <- renderHighchart(
      entries() %>%
        count(host, sort = TRUE)  %>% #head(50)
        head(50) %>%
        hchart('column', hcaes(x = 'host', y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Most Domains (by top stories count)")
      
      
    )
    
    output$lastuse <- renderHighchart(
      entries() %>%
        count(`When did you last use the phone?`) %>%
        hchart('pie', 
               hcaes(x = `When did you last use the phone?`, y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "When did you last use the phone?")
      
      
    )
    
    
    
    output$blrplace <- renderHighchart(
      entries() %>%
        count(`Where do you live in Bangalore? (Select one of these Bangalore Wards) If you don't live in Bangalore Select "Not-Bangalore" Last option`) %>%
        hchart('column', hcaes(x = `Where do you live in Bangalore? (Select one of these Bangalore Wards) If you don't live in Bangalore Select "Not-Bangalore" Last option`, y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Where do you live in Bangalore?")
      
      
    )
    
    output$wifi <- renderHighchart(
      entries() %>%
        count(`Does the phone's WIFI work well?`) %>%
        hchart('column', hcaes(x = `Does the phone's WIFI work well?`, y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Does the phone's WIFI work well?")
      
      
    )
    
    output$suitable <- renderHighchart(
      entries() %>%
        count(`Given that you understand this cause, Do you think your smartphone is suitable for this?`) %>%
        hchart('column', hcaes(x = `Given that you understand this cause, Do you think your smartphone is suitable for this?`, y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Given that you understand this cause, Do you think your smartphone is suitable for this?")
      
      
    )
    
    output$words <- renderHighchart(
      
      title_word_freq() %>%
        head(50) %>%
        hchart('column', hcaes(x = 'word', y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Most frequently occuring words")
      
      
    )
    
    output$bigrams <- renderHighchart(
      
      title_word2_freq() %>%
        head(50) %>%
        hchart('column', hcaes(x = 'word', y = 'n')) %>% 
        hc_add_theme(hc_theme_google()) %>% 
        hc_title(text = "Most frequently occuring words")
      
      
    )
    
  }
)