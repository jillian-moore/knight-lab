# MODULE 4: CONTEXT TAB
# About Us and Research Context

# UI ----
contextTabUI <- function(id) {
  ns <- NS(id)
  
  # Encode images to base64
  melissa_img <- base64enc::base64encode(here::here("www/melissa headshot.jpeg"))
  jillian_img <- base64enc::base64encode(here::here("www/jillian headshot.jpeg"))
  sophia_img <- base64enc::base64encode(here::here("www/sophia headshot.jpeg"))
  keya_img <- base64enc::base64encode(here::here("www/keya headshot.jpg"))
  eunice_img <- base64enc::base64encode(here::here("www/eunice headshot.jpeg"))
  jill_img <- base64enc::base64encode(here::here("www/jill headshot.jpg"))
  
  tagList(
    tags$head(
      tags$style(HTML("
        .context-container {
          max-width: 1400px;
          margin: 0 auto;
          padding: 40px 20px;
        }
        
        .hero-section {
          background: linear-gradient(135deg, #dd5600 0%, #ff7a33 50%, #ffa914 100%);
          color: white;
          padding: 80px 40px;
          border-radius: 20px;
          margin-bottom: 60px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.2);
          text-align: center;
        }
        
        .hero-section h1 {
          font-size: 3.5rem;
          margin-bottom: 30px;
          font-weight: 700;
          text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
        }
        
        .hero-grid {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 40px;
          max-width: 1000px;
          margin: 0 auto;
        }
        
        .hero-card {
          background: rgba(255,255,255,0.15);
          backdrop-filter: blur(10px);
          padding: 30px;
          border-radius: 15px;
          border: 2px solid rgba(255,255,255,0.3);
          transition: transform 0.3s ease;
        }
        
        .hero-card:hover {
          transform: translateY(-10px);
          background: rgba(255,255,255,0.25);
        }
        
        .hero-card h3 {
          color: white;
          font-size: 1.8rem;
          margin: 0 0 15px 0;
          font-weight: 600;
        }
        
        .hero-card p {
          color: white;
          font-size: 1.1rem;
          line-height: 1.7;
          margin: 0;
        }
        
        @media (max-width: 768px) {
          .hero-grid {
            grid-template-columns: 1fr;
          }
          .hero-section h1 {
            font-size: 2.5rem;
          }
        }
        
        .section-header {
          text-align: center;
          margin-bottom: 60px;
        }
        
        .section-header h2 {
          color: #dd5600;
          font-size: 3rem;
          margin-bottom: 15px;
          font-weight: 700;
          position: relative;
          display: inline-block;
        }
        
        .section-header h2::after {
          content: '';
          position: absolute;
          bottom: -10px;
          left: 50%;
          transform: translateX(-50%);
          width: 100px;
          height: 4px;
          background: linear-gradient(90deg, #dd5600, #ffa914);
          border-radius: 2px;
        }
        
        .section-header p {
          color: #666;
          font-size: 1.2rem;
          margin-top: 25px;
        }
        
        .bio-grid {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          gap: 40px;
          margin-bottom: 60px;
        }
        
        @media (max-width: 1024px) {
          .bio-grid {
            grid-template-columns: repeat(2, 1fr);
          }
        }
        
        @media (max-width: 768px) {
          .bio-grid {
            grid-template-columns: 1fr;
          }
        }
        
        .author-bio--card {
          background: white;
          border-radius: 20px;
          box-shadow: 0 5px 20px rgba(0,0,0,0.1);
          padding: 40px 30px;
          text-align: center;
          transition: all 0.4s ease;
          height: 100%;
          display: flex;
          flex-direction: column;
          position: relative;
          overflow: hidden;
        }
        
        .author-bio--card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 5px;
          background: linear-gradient(90deg, #dd5600, #ffa914);
          transform: scaleX(0);
          transition: transform 0.4s ease;
        }
        
        .author-bio--card:hover::before {
          transform: scaleX(1);
        }
        
        .author-bio--card:hover {
          transform: translateY(-10px);
          box-shadow: 0 15px 40px rgba(0,0,0,0.15);
        }
        
        .author-bio--card figure {
          margin: 0 0 25px 0;
          position: relative;
        }
        
        .author-bio--card figure::after {
          content: '';
          position: absolute;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          width: 140px;
          height: 140px;
          border-radius: 50%;
          background: linear-gradient(135deg, #dd5600, #ffa914);
          z-index: -1;
          opacity: 0;
          transition: opacity 0.4s ease;
        }
        
        .author-bio--card:hover figure::after {
          opacity: 0.2;
        }
        
        .author-bio--card figure img {
          width: 130px;
          height: 130px;
          border-radius: 50%;
          object-fit: cover;
          border: 4px solid #f1f3f2;
          transition: border-color 0.4s ease;
          position: relative;
          z-index: 1;
        }
        
        .author-bio--card:hover figure img {
          border-color: #dd5600;
        }
        
        .author-content {
          flex: 1;
          display: flex;
          flex-direction: column;
        }
        
        .author-content h3 {
          margin: 0 0 5px 0;
          color: #333;
          font-size: 1.5rem;
          font-weight: 600;
        }
        
        .author-content h3 a {
          color: #333;
          text-decoration: none;
          transition: color 0.3s ease;
        }
        
        .author-content h3 a:hover {
          color: #dd5600;
        }
        
        .author-content h4 {
          margin: 0 0 20px 0;
          color: #ffa914;
          font-size: 0.9rem;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
        
        .author-content p {
          margin-bottom: 25px;
          font-size: 0.95rem;
          line-height: 1.7;
          color: #555;
          flex: 1;
        }
        
        .list--social {
          list-style: none;
          padding: 0;
          margin: 0;
          display: flex;
          gap: 12px;
          justify-content: center;
        }
        
        .list--social a {
          color: #666;
          font-size: 1.3rem;
          transition: all 0.3s ease;
          display: flex;
          align-items: center;
          justify-content: center;
          width: 45px;
          height: 45px;
          border-radius: 50%;
          background-color: #f1f3f2;
        }
        
        .list--social a:hover {
          color: white;
          background: linear-gradient(135deg, #dd5600, #ffa914);
          transform: scale(1.15) rotate(5deg);
        }
        
        .research-section {
          background: linear-gradient(135deg, #f1f3f2 0%, #e8eae9 100%);
          padding: 60px 40px;
          border-radius: 20px;
          margin-top: 0px;
          margin-bottom: 80px;
          box-shadow: 0 5px 20px rgba(0,0,0,0.05);
        }
        
        .research-section p {
          font-size: 1.1rem;
          line-height: 1.9;
          color: #333;
          margin-bottom: 25px;
        }
        
        .research-section ol {
          font-size: 1.1rem;
          line-height: 1.8;
          color: #333;
          margin: 30px 0 30px 40px;
          counter-reset: item;
          list-style: none;
        }
        
        .research-section ol li {
          margin-bottom: 15px;
          position: relative;
          padding-left: 40px;
          counter-increment: item;
        }
        
        .research-section ol li::before {
          content: counter(item);
          position: absolute;
          left: 0;
          top: 0;
          background: linear-gradient(135deg, #dd5600, #ffa914);
          color: white;
          width: 30px;
          height: 30px;
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          font-weight: 700;
          font-size: 0.9rem;
        }
        
        .highlight-box {
          background: white;
          padding: 30px;
          border-radius: 15px;
          border-left: 5px solid #ffa914;
          margin: 30px 0;
          box-shadow: 0 3px 10px rgba(0,0,0,0.05);
        }
      "))
    ),
    
    div(class = "context-container",
        # Hero Section
        div(class = "hero-section",
            h1("Local News Lens"),
            div(class = "hero-grid",
                div(class = "hero-card",
                    h3("Who?"),
                    p("We are a team of five students at the Knight Lab Studio, a community at Northwestern University's Medill School dedicated to improving journalism through technological innovation.")
                ),
                div(class = "hero-card",
                    h3("What?"),
                    p("The Local News Lens is a newsroom-serving dashboard that tracks local coverage by geography, topic and census data. Built in RStudio with Shiny, it maps coverage across Chicago neighborhoods and includes filters for topics (e.g. art, science), census metrics (e.g. race, income), and time period (e.g. past six months).")
                ),
                div(class = "hero-card",
                    h3("Why?"),
                    p("We approached this project with one primary goal: promoting equity in local news through accessible visualization of coverage patterns.")
                )
            )
        ),
        
        # Research Context Section (moved before bios)
        div(class = "research-section",
            p("With the Local News Lens, our goal was to create a tool grounded in data and reporting trends to help newsrooms identify real coverage gaps. Our initial partnership with Block Club Chicago rooted the project within the city's unique media landscape, but the patterns we found — underreporting and overreporting across neighborhoods — reflect challenges faced by newsrooms everywhere, from big cities to small towns."),
            
            p("Research highlights these inequities. Namely, takeaways from two studies — one from ", 
              tags$a(href = "https://mediaengagement.org/", target = "_blank", 
                     style = "color: #dd5600; text-decoration: underline; font-weight: 600;",
                     "UT Austin's Center for Media Engagement"),
              " and another from the ",
              tags$a(href = "https://sociology.stanford.edu/", target = "_blank",
                     style = "color: #dd5600; text-decoration: underline; font-weight: 600;",
                     "Department of Sociology at Stanford's School of Humanities and Sciences"),
              " — shaped how we tagged and mapped our data for the Local News Lens. Based on their methods, we chose to highlight three areas:"),
            
            tags$ol(
              tags$li("Article topic"),
              tags$li("Geography/neighborhood designation"),
              tags$li("Demographic information")
            ),
            
            p("While many newsrooms already have access to this wide variety of data, few can easily visualize how they interact. The Local News Lens helps editors and reporters view these connections clearly, supporting more equitable storytelling and smarter resource allocation."),
            
            p("Our aim isn't simply to internally flag bias, but to reveal the structural and financial constraints that shape what reporters cover. At nonprofit newsrooms like Block Club Chicago, for example, our tool might provide clear, data-driven visuals that illustrate coverage gaps and help make the case to potential funders. By highlighting these systemic factors, we aim to empower newsrooms to make data-informed editorial decisions, thereby strengthening equitable journalism and local news coverage overall.")
        ),
        
        # Team Bios Section
        div(
          div(class = "section-header",
              h2("Meet Our Team"),
              p("Passionate journalists and technologists working to improve local news equity")
          ),
          
          # First Row: Melissa, Jillian, Sophia
          div(class = "bio-grid",
              # Melissa Dai
              div(class = "author-bio--card",
                  tags$figure(
                    tags$img(src = paste0("data:image/jpeg;base64,", melissa_img), alt = "Melissa Dai")
                  ),
                  div(class = "author-content",
                      tags$a(href = "https://www.linkedin.com/in/melissadai/", target = "_blank",
                             h3("Melissa Dai")),
                      h4("Chief Operating Officer"),
                      p("Melissa is a senior at Northwestern University studying journalism, psychology and integrated marketing communications. An aspiring investigative journalist, she has experience with in-depth reporting at both the national and local levels. She brings a passion for equitable coverage and amplifying voices from underreported communities."),
                      tags$ul(class = "list--social",
                              tags$li(tags$a(href = "https://www.linkedin.com/in/melissadai/", target = "_blank", icon("linkedin"))),
                              tags$li(tags$a(href = "mailto:melissadai2026@u.northwestern.edu", icon("envelope")))
                      )
                  )
              ),
              
              # Jillian Moore
              div(class = "author-bio--card",
                  tags$figure(
                    tags$img(src = paste0("data:image/jpeg;base64,", jillian_img), alt = "Jillian Moore")
                  ),
                  div(class = "author-content",
                      tags$a(href = "https://www.linkedin.com/in/jillian-moore26/", target = "_blank",
                             h3("Jillian Moore")),
                      h4("Chief Technology Officer"),
                      p("Jillian is a senior at Northwestern University double majoring in Economics and Journalism with a minor in Data Science. She specializes in integrating data science and analytics into investigative journalism projects and tools for reporters across the U.S. She brings her passion for transparency and accessibility of public data to the Local News Lens."),
                      tags$ul(class = "list--social",
                              tags$li(tags$a(href = "https://www.linkedin.com/in/jillian-moore26/", target = "_blank", icon("linkedin"))),
                              tags$li(tags$a(href = "https://github.com/jillian-moore", target = "_blank", icon("github"))),
                              tags$li(tags$a(href = "mailto:jillianmoore2004@gmail.com", icon("envelope")))
                      )
                  )
              ),
              
              # Sophia Zhang
              div(class = "author-bio--card",
                  tags$figure(
                    tags$img(src = paste0("data:image/jpeg;base64,", sophia_img), alt = "Sophia Zhang")
                  ),
                  div(class = "author-content",
                      tags$a(href = "https://www.linkedin.com/in/s0phiazhang", target = "_blank",
                             h3("Sophia Zhang")),
                      h4("Chief Information Officer"),
                      p("Sophia is a senior at Northwestern University studying journalism, English literature and psychology, and also a Chicago native. She is excited to see her experience in critical analysis, in-depth research, and communications converge with a desire for prioritizing equity. She brings her passion for hyperlocal engagement and UI/UX to the Local News Lens."),
                      tags$ul(class = "list--social",
                              tags$li(tags$a(href = "https://www.linkedin.com/in/s0phiazhang", target = "_blank", icon("linkedin"))),
                              tags$li(tags$a(href = "mailto:sophiazhang2027@u.northwestern.edu", icon("envelope")))
                      )
                  )
              )
          ),
          
          # Second Row: Keya, Eunice, Jill
          div(class = "bio-grid",
              # Keya Chaudhuri
              div(class = "author-bio--card",
                  tags$figure(
                    tags$img(src = paste0("data:image/jpeg;base64,", keya_img), alt = "Keya Chaudhuri")
                  ),
                  div(class = "author-content",
                      tags$a(href = "https://www.linkedin.com/in/keyasc/", target = "_blank",
                             h3("Keya Chaudhuri")),
                      h4("Chief Design Officer"),
                      p("Keya is a junior at Northwestern University studying Learning Sciences, Journalism, and Spanish. She brings her passion for visual storytelling and community-oriented approaches to equity to the Local News Lens project."),
                      tags$ul(class = "list--social",
                              tags$li(tags$a(href = "https://www.linkedin.com/in/keyasc/", target = "_blank", icon("linkedin"))),
                              tags$li(tags$a(href = "mailto:keyasc@u.northwestern.edu", icon("envelope")))
                      )
                  )
              ),
              
              # Eunice Lee
              div(class = "author-bio--card",
                  tags$figure(
                    tags$img(src = paste0("data:image/jpeg;base64,", eunice_img), alt = "Eunice Lee")
                  ),
                  div(class = "author-content",
                      h3("Eunice Lee"),
                      h4("Chief Intelligence Officer"),
                      p("Eunice is a junior at Northwestern University studying Journalism and Data Science. She is passionate about data-driven storytelling and survey research. She is especially interested in integrating technology and storytelling, and using creativity and multimedia reporting to bring insight to complicated issues."),
                      tags$ul(class = "list--social",
                              tags$li(tags$a(href = "https://www.linkedin.com/in/eunicelee/", target = "_blank", icon("linkedin"))),
                              tags$li(tags$a(href = "mailto:eunsoolee2027@u.northwestern.edu", icon("envelope")))
                      )
                  )
              ),
              
              # Jill Blackman
              div(class = "author-bio--card",
                  tags$figure(
                    tags$img(src = paste0("data:image/jpeg;base64,", jill_img), alt = "Jill Blackman")
                  ),
                  div(class = "author-content",
                      tags$a(href = "https://www.linkedin.com/in/jillblackman/", target = "_blank",
                             h3("Jill Blackman")),
                      h4("Chief Faculty Advisor"),
                      p("Jill is the team's faculty advisor at Northwestern's Knight Lab Studio. She is passionate about storytelling and shares her enthusiasm and knowledge as a lecturer at Medill with a specialization in digital storytelling."),
                      tags$ul(class = "list--social",
                              tags$li(tags$a(href = "https://www.linkedin.com/in/jillblackman/", target = "_blank", icon("linkedin"))),
                              tags$li(tags$a(href = "mailto:jill.blackman@northwestern.edu", icon("envelope")))
                      )
                  )
              )
          )
        )
    )
  )
}

# SERVER ----
contextTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed for static content
  })
}