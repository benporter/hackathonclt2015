setwd("/home/ben/Hackathon/HackathonCLT2015/data")

library(shiny)
library(shinydashboard)
library(rCharts)
library(dplyr)
library(ggplot2)
library(DT)

# preprocessing

#### Stores
store_hd <- read.delim(file = "calc1", header = FALSE, sep = "|")
colnames(store_hd) <- c("store","vists","customer_count","online_item_count", 
                        "total_item_count","unique_items", "sales","discount")


store_hd <- store_hd %>% mutate(online_proportion = online_item_count / total_item_count,
                                discount_proportion = discount / sales)


#### Customers
customer_hd <- read.delim(file = "calc2", header = FALSE, sep = "|")
colnames(customer_hd) <- c("homeStore", "customer", "tier", "sales", "discount", "online")
customer_hd_singlestore <- customer_hd %>% 
  filter(homeStore==4,tier %in% c(1,2,3,4)) %>%
  mutate(logdiscount = ifelse(discount<0,0,log(discount)),
         logsales = ifelse(discount<0,0,log(sales)))

#used in drop down menu of Customers tab
store_list <- customer_hd %>% 
  filter(tier %in% c(1,2,3,4)) %>%
  select(homeStore) %>% distinct(homeStore)

#### Transactions

# read in base dataset, 217 customers, all of their transactions
trans_hd <- read.delim(file = "calc3", header = FALSE, sep = "|")
colnames(trans_hd) <- c("customer", "tier", "homestore","homestorelat", "homestorelon", "distance",
                        "receipt", "datetime",  "onlineflag",
                        "store","storelat","storelon", "upc","desc", "mupc", 
                        "subcat","subcat_num","cat","cat_num","dept", "dept_num", "sales","discount","quantity")

# remove the timestamp component
trans_hd$date <- as.Date(strptime(trans_hd$datetime, format="%Y-%m-%d"))    

ui <- dashboardPage(
  
  dashboardHeader(title = "HT Clairvoyant"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stores", tabName = "stores_tab", icon = icon("map-marker")),
      menuItem("Customers", tabName = "customers_tab", icon = icon("users")),
      menuItem("Products", tabName = "products_tab", icon =  icon("shopping-cart")),
      menuItem("Profitability", tabName = "profitability_tab", icon =  icon("dollar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "stores_tab",
              h2("Stores by Online Channel Usage and Discount Rate"),
              showOutput("store_disc_online_scatter", "nvd3")
              ), 
      
      tabItem(tabName = "customers_tab",
              selectInput("store_selection", "Store:",
                          store_list$homeStore),
              
              h2("Customer Count by Tier and Online Usage"),
              showOutput("tier_online_plot_nvd3", "nvd3"),
              br(), h2("Proportion of Customers by Tier and Online Usage"),
              showOutput("tier_online_plot_nvd3_stacked", "nvd3")
              ),
      tabItem(tabName = "products_tab",
              h2("Targetted Offers"),br(),
              textInput("target_date_choice", "Model (Today):", "2015-03-01"),
              sliderInput("false_pos_choice", "False Postive Filter", min=0.0, max=1.0, value=0.20, step = 0.01),
              plotOutput("false_pos_plot"),
              DT::dataTableOutput("tbl_product"),
              DT::dataTableOutput("tbl_customer_listing")         
              ),
      tabItem(tabName = "profitability_tab",
              h2("Refine Assumptions"),
              fluidRow(
                h3("Revenue"),
                box(sliderInput("acceptance_rate", label="Customer Probability of Accepting", min=0.0, max=1.0, value=0.1, step = 0.01),
                    numericInput("fee", label = "Delivery Fee, $", value = 5,step=1),
                    numericInput("avgprofit", label = "Average Profit on Products, %", value = .05,step = 0.01)                  
                    ) 
              ),
              fluidRow(
                h3("Expense"),
                box(numericInput("fuel", label = "Fuel, $", value = 2.30,step=0.1),
                    numericInput("distance", label = "Distance Factor", value = .5,step=0.1),
                    numericInput("hourlywage", label = "Hourly Wage, $", value = 15,step = 0.01),                  
                    numericInput("hours", label = "Hours Per Customer Served, $", value = 0.2,step = 0.1) 
                ) 
              ),
              fluidRow(
                h3("Per Trip Profitability"),
                dataTableOutput("profit")
              )
              )
      )
  )
)


server <- function(input, output) {
  
  output$profit<- renderDataTable({
    
    df <- false_pos_customer() %>% filter(false_pos==0)
    joinme <- sorted_trans() %>% group_by(customer,distance) %>%
      summarise(sales=sum(sales)) 
    df <- df %>% left_join(joinme,by="customer") %>%
      select(customer, distance, `Target Product Proportion` = proportion_in_target, sales)
    
    num_customers <- df %>% summarise(n=n_distinct(customer))
    total_distance <- df %>% summarise(sum=sum(distance))
    
    rev <- (input$acceptance_rate * num_customers$n ) * input$fee
    expense_driving <- (total_distance$sum * input$distance) * input$fuel
    expense_people <- (num_customers * input$hours) * input$hourly
    
    profit <- rev - expense_driving - expense_people
    profit_df <- datatable(data.frame("Profit"=profit))
    profit_df 
  })
  
  
   target_date <- reactive({ 
     #as.Date("2015-03-01")  
     as.Date(input$target_date_choice)  
     }) 
  

   sorted_trans <- reactive({ 
     sorted_trans <- trans_hd %>% filter(tier %in% c(1,2,3,4),
                                       nchar(as.character(desc)) < 100,
                                       nchar(as.character(dept)) < 100) %>% #remove bad records
     arrange(customer,upc,datetime) %>%
     group_by(customer,upc) %>%
     mutate(timebetweenpurchase = date - lag(date)) %>% # create the time between purchaes variable
     filter(timebetweenpurchase > 0, #remove same day purchase and 1 time purchases
            date < target_date())      # remove every after the target, that hasn't happened yet
     sorted_trans
   }) 
   
   #upc level summary stats
   upc_summ <- reactive({ 
     upc_summ <- sorted_trans() %>% group_by(customer,upc) %>%
     summarise(max_date = max(date),
               mean = mean(timebetweenpurchase),
               sd = sd(timebetweenpurchase),
               n=n()) %>%
      arrange(-n)
      return(upc_summ)
   }) 
   
   
   # add UPC summary level stats to transactional table
   sorted_trans_2 <- reactive({
    sorted_trans_2 <- sorted_trans() %>% left_join(upc_summ(),by=c("customer","upc")) %>%
     mutate(days_since_last = target_date() - max_date,
            sd_clean = ifelse(is.na(sd),0,sd),
            threshold = mean + 2*sd_clean,
            target_product = days_since_last > threshold)
    sorted_trans_2
   })
  
   false_pos_customer <- reactive({
     # false_pos_choice
     # control for false positives, customer on vacation or cheating on you with Publix
     false_pos_customer <- sorted_trans_2() %>% group_by(customer) %>%
     summarise(proportion_in_target = sum(target_product) / n(),
               false_pos = ifelse(proportion_in_target>input$false_pos_choice,1,0))
     false_pos_customer
   })
   
   output$false_pos_plot <- renderPlot({
     df <- false_pos_customer()
     hist(x = df$proportion_in_target, breaks=10, 
               main = "Distribution of High Volume Products \nExceeding the Threshold",
               xlab="Proportion in of Products Exceeding Threshold",
               xlim=c(0,1), col="lightblue") 
     abline(v=input$false_pos_choice,col="orange",lty=1,lwd=5)
   })
   
   
#   # list of products to solicit, with count of customer to hit up,
#   # with false positives removed
  products_to_customers <- reactive({
     products_to_customers <- sorted_trans_2() %>%
     inner_join(false_pos_customer(),by="customer") %>%
     filter(false_pos==0) %>% #remove false positive customers
     group_by(upc,desc,cat,dept) %>%
     summarise(customers = n_distinct(customer)) %>%
     filter(customers > 5) %>% ungroup() 
   
   products_to_customers <- products_to_customers %>% arrange(-customers)
   products_to_customers
  })

  output$tbl_product = DT::renderDataTable({
     DT::datatable(products_to_customers(),
                                 caption = 'Products with Highest Liklihood')
  })

  output$tbl_customer_listing = DT::renderDataTable({
    df <- false_pos_customer() %>% filter(false_pos==0)
    joinme <- sorted_trans() %>% group_by(customer,distance) %>%
               summarise(sales=sum(sales)) 
    df <- df %>% left_join(joinme,by="customer") %>%
      select(customer, distance, `Target Product Proportion` = proportion_in_target, sales) %>%
      mutate(sales = sales/100)
    DT::datatable(df,
                  caption = 'Customers with Highlest Likelikehood, Removing False Positives',
                       options = list(order = list(list(2,"dsc"))) # order the first col ascending, second descending
                       )
  
  })

  customer_multibar <- reactive({
        
    customer_multibar <- customer_hd %>% 
      filter(homeStore==input$store_selection,
             tier %in% c(1,2,3,4)) %>%
      group_by(tier,online) %>%
      summarise(customer_count = n_distinct(customer))
    
    customer_multibar
  })
  
  output$store_disc_online_scatter = renderChart({  
    colnames(store_hd)
    nvd3_chart <- nPlot(online_proportion ~ discount_proportion, 
                        #group = "Pclass", 
                        data=store_hd, type = "scatterChart")
    nvd3_chart$addParams(width = 800, height = 600, dom = "store_disc_online_scatter") # dom corresponds to first argument in showOutput() in ui
    nvd3_chart$chart(tooltipContent = "#! function(key, x, y, e){ return 'Store: ' + e.point.store} !#")
    nvd3_chart$yAxis( axisLabel = "Proportion of Items Sold Via Online Channel" ,tickFormat = "#!d3.format('.1%')!#" )
    nvd3_chart$xAxis( axisLabel = "Proportion of Discount" ,tickFormat = "#!d3.format('.1%')!#")

    return(nvd3_chart)
    
  }) 
  
  
  output$tier_online_plot_nvd3 = renderChart({  
    
    ##############################################################
    # http://rcharts.readthedocs.org/en/latest/nvd3/create.html  #
    ##############################################################

    nvd3_chart <- nPlot(customer_count ~ tier, group = "online", data=customer_multibar(), type = "multiBarChart")
    nvd3_chart$set(title="Count of Customers by Tier and Online Usage")
    nvd3_chart$yAxis( axisLabel = "Customer Count" , width=40 )
    nvd3_chart$xAxis( axisLabel = "tier" )
    
    nvd3_chart$addParams(width = 800, height = 400, dom = "tier_online_plot_nvd3") # dom corresponds to first argument in showOutput() in ui
    return(nvd3_chart)
    
  })  
  
  output$tier_online_plot_nvd3_stacked = renderChart({ 
    
    nvd3_chart <- nPlot(customer_count ~ tier, group = "online", data=customer_multibar() ,type = 'stackedAreaChart', id = 'chart')
    nvd3_chart$addParams(width = 1000, height = 500, dom = "tier_online_plot_nvd3_stacked") # dom corresponds to first argument in showOutput() in ui
    return(nvd3_chart)
    
  })
  
  output$stores_plot_ggplot <- renderPlot({
    g <- ggplot(customer_hd_singlestore, aes(x=logdiscount, y=logsales)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(se=FALSE)
    return(g)
  })
  


}

shinyApp(ui, server)



