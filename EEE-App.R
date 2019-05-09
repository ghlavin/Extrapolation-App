library(shiny)
library(shinythemes)
library(ggplot2)

ui <- navbarPage(
  "",
  theme = shinytheme("flatly"),
  tabPanel("Home",
           mainPanel(
             br(),
             br(),
             br(),
             br(),
             br(),
             h1("EXTRAPOLATION APP", align = "center"),
             p(
               "to recalculate the results in ",
               a("Evidence, eminence, and extrapolation. ",
                 href = "http://onlinelibrary.wiley.com/doi/10.1002/sim.6865/pdf"),
               align = "center"
             )
           )),
  # tabPanel("Manual",
  #          verbatimTextOutput("Manual")),
  tabPanel(
    "Calculate",
    titlePanel("Adjust the significance level, based on prior information"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "a_adult",
          label = "Alpha level of the adult study program",
          min = 0,
          max = 1,
          value = 0.025,
          step = 0.005
        ),
        sliderInput(
          "pow_adult",
          "Power of the adult study program",
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01
        ),
        checkboxInput("calculate_ppv_a", label =
                        "Calculate the PPV in adults?", value = FALSE),
        conditionalPanel(
          condition = "input.calculate_ppv_a == false",
          numericInput(
            "ppv_adults",
            "PPV of the adult study program",
            min = 0,
            max = 1,
            value = 0.973,
            step = 0.001
          )
        ),
        conditionalPanel(
          condition = "input.calculate_ppv_a == true",
          sliderInput(
            "one.minus.r_adult",
            " Prior belief in the alternative hypothesis at the start of Phase III in adults",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01
          )
        ),
        checkboxInput("check_equal_ppv", label =
                        "Are PPV, alpha level and power of the adult study program considered to be the conventional values?", value =
                        TRUE),
        conditionalPanel(
          condition = "input.check_equal_ppv == false",
          sliderInput(
            "a",
            label = "Conventional alpha level",
            min = 0,
            max = 1,
            value = 0.025,
            step = 0.005
          ),
          sliderInput(
            "pow_conventional",
            "Conventional power",
            min = 0,
            max = 1,
            value = 0.8,
            step = 0.01
          ),
          checkboxInput("calc_ppv_children", label =
                          "Calculation of the conventional PPV?", value = FALSE),
          conditionalPanel(
            condition = "input.calc_ppv_children == false",
            numericInput(
              "ppv_children",
              "Conventional PPV targeted for children",
              min = 0,
              max = 1,
              value = 0.973,
              step = 0.001
            )
          ),
          conditionalPanel(
            condition = "input.calc_ppv_children == true",
            sliderInput(
              "one.minus.r",
              "Conventional belief 1-r in the alternative hypothesis at the start of Phase III",
              min = 0,
              max = 1,
              value = 0.5,
              step = 0.01
            )
          )
        ),
        sliderInput(
          "one.minus.q",
          "Belief 1-q in the alternative hypothesis in children, if extrapolation is not considered to be possible.",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.01
        ),
        sliderInput(
          "power_children",
          "Power of the paediatric study",
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01
        ),
        br(),
        br(),
        helpText("1) Define the alpha level and power of the adult study program."),
        helpText(
          "2) Specify the positive predictive value PPV of the adult study program, or calculate it by setting the prior belief in the alternative hypothesis before start of Phase III in adults ."
        ),
        helpText(
          "3) Prespecify the PPV, the alpha level, and the power that corresponds to a conventional study program for this specific indication this indication and age group, either by:"
        ),
        helpText("  3.a) equating them with the corresponding values in adults"),
        helpText(
          "  3.b) setting, alpha, the power, and and the PPV for conventional study programs"
        ),
        helpText(
          "  3.c) setting, alpha, the power, and and the prior belief in efficacy before start of phase III for conventional studies"
        ),
        helpText(
          "4) Set the belief 1-q in the alternative hypothesis in children for the case that extrapolation is not considered to be possible."
        ),
        helpText("5) Set the power of the paediatric study.")
      ),
      mainPanel(tabsetPanel(
        id="plottab",
        tabPanel(
          "Adjusted significance level",
          plotOutput(
            outputId = "plot",
            width = "700px",
            height = "700px"
          ),
          helpText(
            "Calculation of the adjusted significance level based on prior assumptions."
          )
        ),
        
        tabPanel(
          "Power",
          plotOutput(
            outputId = "plot2",
            width = "700px",
            height = "700px"
          ),
          sliderInput(
            "s",
            "scepticism s",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.01
          ),
          sliderInput(
            "std.eff.design",
            "Std. effect used for Power Calculation",
            min = 0,
            max = 1.5,
            value = 1,
            step = 0.01
          )
        )
      ))
    )
  )
)


server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$calculate_ppv_a == FALSE) {
      ppv_adult <- input$ppv_adults
      a_adult <- input$a_adult
      pow_adult <- input$pow_adult
    } else {
      a_adult <- input$a_adult
      pow_adult <- input$pow_adult
      one.minus.r_adult <- input$one.minus.r_adult
      ppv_adult <-
        pow_adult * one.minus.r_adult / (pow_adult * one.minus.r_adult + a_adult * (1 -
                                                                                      one.minus.r_adult))
    }
    if (input$check_equal_ppv == TRUE) {
      ppv_conv <- ppv_adult
      a_conv <- a_adult
    } else{
      if (input$calc_ppv_children == FALSE) {
        ppv_conv <- input$ppv_children
        a_conv <- input$a
        pow_conv <- input$pow_conventional
      } else {
        a_conv <- input$a
        pow_conv <- input$pow_conventional
        one.minus.r_conv <- input$one.minus.r
        ppv_conv <-
          pow_conv * one.minus.r_conv / (pow_conv * one.minus.r_conv + a_conv * (1 -
                                                                                   one.minus.r_conv))
      }
    }
    one.minus.q <- input$one.minus.q
    power_children <- input$power_children
    
    s <- seq(0.0001, 0.9999, 0.0001)
    
    prior_H1 <- (ppv_adult) * (1 - s) + (one.minus.q) * s
    a_new <-
      power_children * prior_H1 * (1 - ppv_conv) / ((1 - prior_H1) * ppv_conv)
    a_new <- apply(cbind(a_new, 0.5), 1, min)
    datafr <- data.frame(s, a_new)
    z <-
      ggplot(data = datafr, aes(x = s, y = a_new)) + geom_line(size = 1.5, color =
                                                                 "#2E3D54") +
      xlim(0, 1) + ylim(0, 1) + xlab("s") + ylab(expression(alpha[adj])) + theme_minimal() +
      geom_hline(yintercept = a_conv,
                 linetype = "dashed",
                 size = 1.5)
    
    print(z)
  })
  output$plot2 <- renderPlot({
    if (input$calculate_ppv_a == FALSE) {
      ppv_adult <- input$ppv_adults
      a_adult <- input$a_adult
      pow_adult <- input$pow_adult
    } else {
      a_adult <- input$a_adult
      pow_adult <- input$pow_adult
      one.minus.r_adult <- input$one.minus.r_adult
      ppv_adult <-
        pow_adult * one.minus.r_adult / (pow_adult * one.minus.r_adult + a_adult * (1 -
                                                                                      one.minus.r_adult))
    }
    if (input$check_equal_ppv == TRUE) {
      ppv_conv <- ppv_adult
      a_conv <- a_adult
      pow_conv <- pow_adult
    } else{
      if (input$calc_ppv_children == FALSE) {
        ppv_conv <- input$ppv_children
        a_conv <- input$a
        pow_conv <- input$pow_conventional
      } else {
        a_conv <- input$a
        pow_conv <- input$pow_conventional
        one.minus.r_conv <- input$one.minus.r
        ppv_conv <-
          pow_conv * one.minus.r_conv / (pow_conv * one.minus.r_conv + a_conv * (1 -
                                                                                   one.minus.r_conv))
      }
    }
    one.minus.q <- input$one.minus.q
    power_children <- input$power_children
    
    s <- input$s
    
    std.eff.design <- input$std.eff.design
    
    true.effect <- seq(-0.5, 1.5, 0.01)
    
    n.func <- function(a.adj, b, std.eff.design) {
      2 * (qnorm(1 - a.adj) + qnorm(1 - b)) ^ 2 / (std.eff.design) ^ 2
    }
    pow.func <- function(eff, a.adj, n) {
      return(1 - pnorm((qnorm(1 - a.adj) * sqrt(2 / n)), mean = eff, sd = sqrt(2 /
                                                                                 n)))
    }
    
    prior_H1 <- (ppv_adult) * (1 - s) + (one.minus.q) * s
    a_new <-
      power_children * prior_H1 * (1 - ppv_conv) / ((1 - prior_H1) * ppv_conv)
    a_new <- apply(cbind(a_new, 1), 1, min)
    n <- n.func(a_new, 1 - power_children, std.eff.design)
    pow.vals <- pow.func(true.effect, a_new, n)
    
    n.conv <-
      2 * (qnorm(1 - a_conv) + qnorm(power_children)) ^ 2 / (std.eff.design) ^
      2
    n.new <-
      2 * (qnorm(1 - a_new) + qnorm(power_children)) ^ 2 / (std.eff.design) ^
      2
    datafr2 <- data.frame(true.effect, pow.vals)
    
    z2 <-
      ggplot(data = datafr2, aes(x = true.effect, y = pow.vals)) + geom_line(size =
                                                                               1.5, color = "#2E3D54") +
      xlim(-0.5, 1.5) + ylim(0, 1) + xlab("True standardized effect") + ylab("Rejection probability") +
      annotate(
        "text",
        x = 0.05,
        y = 0.95,
        label = paste("conv.ssize ==", ceiling(n.conv)),
        parse = TRUE
      ) +
      annotate(
        "text",
        x = 0.05,
        y = 0.90,
        label = paste("adj.ssize ==", ceiling(n.new)),
        parse = TRUE
      ) +
      annotate(
        "text",
        x = 0.05,
        y = 0.85,
        label = paste("rel.ssize ==", round(n.new / n.conv, 2)),
        parse = TRUE
      ) +
      geom_hline(yintercept = pow_conv,
                 linetype = "dashed",
                 size = 1.5) + geom_vline(xintercept = 1,
                                          col = "gray",
                                          size = 0.5) +
      theme_minimal()
    print(plot(z2))
  })
}

shinyApp(ui = ui, server = server)
