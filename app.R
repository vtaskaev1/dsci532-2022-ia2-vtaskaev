library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(purrr)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(tidyr)

tsunami_events <- read.csv("data/processed/tsunami-events.csv")
country_codes <- read.csv("data/processed/country_codes.csv")

years = unique(tsunami_events[['year']])
countries = sort(unique(tsunami_events[['country']]))

app = Dash$new(external_stylesheets = dbcThemes$QUARTZ)

navbar = dbcNavbar(
    dbcContainer(
        list(
            htmlA(
                dbcRow(
                    list(
                        dbcCol(dbcNavbarBrand('Tsunami Events Dashboard'))
                    ),
                    align = 'center',
                    className = 'g-0'
                )
            ),
            dbcNavbarToggler(id = 'navbar-toggler', n_clicks = 0),
            dbcCollapse(
                id = 'navbar-collapse',
                is_open = FALSE,
                navbar = TRUE,
            )
        )
    ),
    color = 'dark',
    dark = TRUE
)

scatter_plot_card <- dbcCard(
    dbcCardBody(list(
        htmlH6('Earthquake Magnitude versus Total Deaths'),
        dccGraph(id = 'scatter_plot')
    )
    )
)

app$layout(dbcContainer(
    list(
        navbar,
        dbcRow(list(
            dbcCol(list(
                htmlH5('Years and Countries Selection', className='form-label'),
                htmlHr(),
                htmlH6('Years of Interest (1802 - 2022)', className='form-label'),
                dccRangeSlider(
                    id = 'year_slider',
                    min=min(tsunami_events$year),
                    max=max(tsunami_events$year),
                    value= list(min(tsunami_events$year), max(tsunami_events$year)),
                    allowCross=FALSE,
                    marks = list(
                        "1802" = "1802",
                        "1850" = "1850",
                        "1900" = "1900",
                        "1950" = "1950",
                        "2000" = "2000",
                        "2022" = "2022"),
                ),
                htmlBr(),
                htmlBr(),
                htmlH6('Countries of Interest', className='form-label'),
                dccDropdown(
                    id = 'country_select',
                    multi = TRUE,
                    value = list(),
                    options = countries,
                    className = 'text-dark')
            ), width = 4),
            dbcCol(list(
                scatter_plot_card
            ),
            width = 8
            )
        ))
    )
))



app$callback(
    output('scatter_plot', 'figure'),
    list(input('year_slider', 'value'),
         input('country_select', 'value')),
    function(years, countries) {
        create_scatter_plot(years[1], years[2], countries)
    }
)

create_scatter_plot <- function(year_start, year_end, countries) {
    if (as.integer(year_start) > as.integer(year_end)) {
        stop("Invalid value for year start and/or year end")
    }
    
    if (typeof(countries) != "list") {
        stop("Invalid value for countries")
    }
    
    if (length(countries) > 0) {
        tsunami_events <- tsunami_events %>%
            filter(country %in% countries)
    }
    
    tsunami_events <- tsunami_events %>%
        filter(year >= year_start,
               year <= year_end)
    
    
    # if (length(countries > 10)) {
    #   countries_subset <- tsunami_events |> 
    #     filter(country %in% countries) |>
    #     arrange(desc(earthquake_magnitude)) |> 
    #     pull(country) |> 
    #     unique() |> 
    #     head(10)
    # }
    # else if (length(countries > 10)) {
    #   countries_subset <- tsunami_events |> 
    #     arrange(desc(earthquake_magnitude)) |> 
    #     pull(country) |> 
    #     unique() |> 
    #     head(10)
    # }
    # else {
    #   countries_subset <- countries
    # }
    
    p <- ggplot(tsunami_events) +
        aes(x = earthquake_magnitude,
            y = total_deaths,
            color = country) +
        geom_point() +
        scale_y_log10() +
        ggthemes::scale_color_tableau()
    ggplotly(p)
}

app$run_server(host = '0.0.0.0')