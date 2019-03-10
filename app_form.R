library(shiny)
library(tidyverse)
library(rdrop2)

outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

# Define the fields we want to save from the form
fields <- c("name1", "name2", "email", "twitter","twitter10", "github", "github10",
            "blog", "blog10", "linkedin", "linkedin10", "other", "other10",
            "experience", "expother", "industry", "attended", "relevant", "relevantother",
            "experience2", "exp2other", "accomplishments", "why", "why2", "magnify",
            "experience3", "exp3other", "condition", "diversity", "diversity2", "travel",
            "travel2", "codeofconduct")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    tags$style(".form-group.shiny-input-container { width: 600px; }"),
    img(src='logo.png', align = "left", height = "200px"), br(),br(),
    titlePanel("Chicago R Unconference Application"),
    column(10,offset=1,div(style = "height:250px;",
    DT::dataTableOutput("responses", width = 300), tags$hr(),
    textInput("name1", "First Name", "", width = "400px"),
    textInput("name2", "Last Name", "", width = "400px"),
    textInput("email", "Email", "", width = "400px"),
    br(),
    
    #ONLINE/LINKS
    h2("Online Presence"),
    h5("Please provide any relevant social media account information that you 
       would like to help us gauge your involvement in the R community. 
       Additionally, we may ask you to use these accounts on our website if you are selected to attend."),
    radioButtons("twitter10", "Do you have a Twitter account?", choices = c("Yes","No"), selected = character(0)), 
    conditionalPanel(condition = "input.twitter10 == 'Yes'", textInput("twitter", "Twitter Handle:", "")),
    radioButtons("github10", "Do you have a Github account?", choices = c("Yes","No"), selected = character(0)), 
    conditionalPanel(condition = "input.github10 == 'Yes'", textInput("github", "GitHub Username:", "")),
    radioButtons("blog10", "Do you have a blog?", choices = c("Yes","No"), selected = character(0)), 
    conditionalPanel(condition = "input.blog10 == 'Yes'", textInput("blog", "Blog URL:", "")),
    radioButtons("linkedin10", "Do you have a LinkedIn account?", choices = c("Yes","No"), selected = character(0)), 
    conditionalPanel(condition = "input.linkedin10 == 'Yes'", textInput("linkedin", "LinkedIn URL:", "")),
    radioButtons("other10", "Any other links?", choices = c("Yes","No"), selected = character(0)), 
    conditionalPanel(condition = "input.other10 == 'Yes'", textInput("other", "Any other links:", "")),
    br(),
    
    #BACKGROUND
    h2("Background"),
    radioButtons("experience", "What is your current occupation?", 
                 choices = c("Undergraduate student", "Masters student", "Doctoral student",
                                             "Academic (including professors, post-doctoral students)",
                                             "Other researchers", "Analytics professional - public sector", 
                                             "Analytics professional - private sector", "Other"), selected = character(0)),
    conditionalPanel(condition = "input.experience == 'Other'", textInput("expother", "Please describe:", "")),
    textInput("industry", "What industry or fields are you most interested in applying R to?:", "", width = "600px"),
    radioButtons("attended", "Have you attended an unconference before (e.g. an rOpenSci unconf)?", 
                 choices = c("Yes", "No"), selected = character(0)),             
    checkboxGroupInput("relevant", "Please check as many boxes as are relevant to your past R usage:", 
                       choices = c("I have used R for coursework", "I have used R for academic research (including theses)",
                                   "I have used R for personal projects", "I have used R for work in industry",
                                   "I have used R for work in government", "I have used R for work in a nonprofit",
                                   "Other"), selected = NULL),
    conditionalPanel(condition = "input.relevant.includes('Other')", 
                     textInput("relevantother", "Please describe:", "")),
    checkboxGroupInput("experience2", "Please check the box to indicate experience with any of the following:", 
                       choices = c("Used git individually", "Used git to collaborate with others",
                                   "Used a web-based version control site (e.g. Github, Gitlab, BitBucket) individually", 
                                   "Used a web-based version control site (e.g. Github, Gitlab, BitBucket) to collaborate with others",
                                   "Made an open-source contribution to an existing project on GitHub",
                                   "Written an R script",
                                   "Made an R project",
                                   "Developed an R package for personal use",
                                   "Release an R package publicly",
                                   "Written unit tests for an R package",
                                   "Taught or helped others to do any or all of the above",
                                   "Other"), selected = NULL),
    conditionalPanel(condition = "input.experience2.includes('Other')", textInput("exp2other", "Please describe:", "")),
    textAreaInput("accomplishments", "Briefly describe any personal or community-related R accomplishment that 
              you are most proud of. If possible, please include any relevant links.", "", 
                  width = "600px", resize = "vertical"),
    br(),
    
    #INTERESTS
    h2("Interests"),
    textAreaInput("why", "Why do you want to attend this unconf?", "", width = "600px"),
    # h5("Describe what you see as your long-term goals, and how will attending this unconf help you reach them."),
    textAreaInput("why2", "What do you specifically hope to learn or accomplish at this 
              unconf (e.g. develop a package, submit your first pull request)?", "", width = "600px"),
    textAreaInput("magnify", "How can you help us magnify the impact of this unconf by sharing what you learned?",
                  "", width = "600px"),
    checkboxGroupInput("experience3", "Select which type of project(s) you would be most interested to work on:", 
                       choices = c("Tutorials for using existing packages", 
                                   "Vignettes or documentation enhancements to contribute to an open-source project",
                                   "Code contribution to existing open-source project", 
                                   "Preliminary development of new package",
                                   "Other"), selected = NULL),
    conditionalPanel(condition = "input.experience3.includes('Other')", textInput("exp3other", "Please describe:", "")),
    br(),
    
    #CONDITIONS
    h2("Conditions"),
    radioButtons("condition", "We intend to maintain a strong social media presence (e.g. Twitter) 
                 throughout the unconf, including sharing photos and videos. Do we have your 
                 permission to share photos and videos which include you in any unconf posts?", 
                 choices = c("I agree", "I do not agree"), selected = character(0)),
    radioButtons("diversity", "We are committed to providing a welcoming community for all participants. 
                 One priority is to foster diversity in open source contribution. 
                 Thus, we strongly encourage applications from women, people of colour, 
                 LGBTQI, differently-abled individuals, and any other underrepresented minorities. 
                 If you identify with any of these groups and wish to share that information with us, 
                 please use the answers below.", 
                 choices = c("I consider myself a part of one of these groups", 
                             "I do not consider myself a part of these groups", "I prefer not to specify"), 
                 selected = character(0)),
    conditionalPanel(condition = "input.diversity == 'I consider myself a part of one of these groups'",
                  textAreaInput("diversity2", "If you would like, please provide more information about 
                  the groups you identify as a member of above.",
                  "", width = "600px")),
    radioButtons("travel", "Do you need travel support to attend this unconf?", 
                 choices = c("Yes, I will need travel and lodging", "Just travel", "Just lodging",
                             "I don't need any support"), selected = character(0)),
    #not working?
    conditionalPanel(condition = "input.travel == 'Yes, I will need travel and lodging' || input.travel == 'Just travel' || input.travel == 'Just lodging'",
                     textAreaInput("travel2", "If you require travel support, please provide a line-item summary of your costs.",
                                   "", width = "600px")),
    br(),
    br(),
    p("Please read the Code of Conduct for this event, which can be found",
       a("here.", href="https://chirunconf.github.io/coc/"),
       " Do you agree with the terms and conditions?", style = "font-weight: bold; margin:0; line-height: 0px;"),
    radioButtons("codeofconduct", label = "", 
                 choices = c("I agree", "I do not agree"), selected = character(0)), 
    br(),
    h4("Submit application below:"),
    actionButton("submit", "Submit"),
    br(),
    br(),
    br(),
    br()
  ))),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
  }
)
