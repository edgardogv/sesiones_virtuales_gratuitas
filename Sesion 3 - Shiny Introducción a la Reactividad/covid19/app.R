# Shiny: Introducción a la Reactividad
# Autor: Edgardo Gutiérrez vega
# Cargando Paquetes
library(DT)
library(shiny)
library(data.table)
library(ggplot2)
library(plotly)
library(shinythemes)

# Cargando funcion
source("casosNuevosCOVID19.R")

# Todos los paises o regiones
paises_regiones <- c('Afghanistan','Albania','Algeria','Andorra','Angola','Antigua and Barbuda','Argentina','Armenia','Australia','Austria','Azerbaijan','Bahamas','Bahrain','Bangladesh','Barbados','Belarus','Belgium','Belize','Benin','Bhutan','Bolivia','Bosnia and Herzegovina','Botswana','Brazil','Brunei','Bulgaria','Burkina Faso','Burma','Burundi','Cabo Verde','Cambodia','Cameroon','Canada','Central African Republic','Chad','Chile','China','Colombia','Comoros','Congo (Brazzaville)','Congo (Kinshasa)','Costa Rica','Croatia','Cuba','Cyprus','Czechia','Denmark','Diamond Princess','Djibouti','Dominica','Dominican Republic','Ecuador','Egypt','El Salvador','Equatorial Guinea','Eritrea','Estonia','Eswatini','Ethiopia','Fiji','Finland','France','Gabon','Gambia','Georgia','Germany','Ghana','Greece','Grenada','Guatemala','Guinea','Guinea-Bissau','Guyana','Haiti','Holy See','Honduras','Hungary','Iceland','India','Indonesia','Iran','Iraq','Ireland','Israel','Italy','Jamaica','Japan','Jordan','Kazakhstan','Kenya','Korea, South','Kosovo','Kuwait','Kyrgyzstan','Laos','Latvia','Lebanon','Lesotho','Liberia','Libya','Liechtenstein','Lithuania','Luxembourg','Madagascar','Malawi','Malaysia','Maldives','Mali','Malta','Mauritania','Mauritius','Mexico','Moldova','Monaco','Mongolia','Montenegro','Morocco','Mozambique','MS Zaandam','Namibia','Nepal','Netherlands','New Zealand','Nicaragua','Niger','Nigeria','North Macedonia','Norway','Oman','Pakistan','Panama','Papua New Guinea','Paraguay','Peru','Philippines','Poland','Portugal','Qatar','Romania','Russia','Rwanda','Saint Kitts and Nevis','Saint Lucia','Saint Vincent and the Grenadines','San Marino','Sao Tome and Principe','Saudi Arabia','Senegal','Serbia','Seychelles','Sierra Leone','Singapore','Slovakia','Slovenia','Somalia','South Africa','South Sudan','Spain','Sri Lanka','Sudan','Suriname','Sweden','Switzerland','Syria','Taiwan*','Tajikistan','Tanzania','Thailand','Timor-Leste','Togo','Trinidad and Tobago','Tunisia','Turkey','Uganda','Ukraine','United Arab Emirates','United Kingdom','Uruguay','US','Uzbekistan','Venezuela','Vietnam','West Bank and Gaza','Western Sahara','Yemen','Zambia','Zimbabwe')

# Creando UI
ui <- navbarPage("COVID-19",
                 tabPanel("Exploracion de Datos",
                          fluidPage(
                              theme = shinytheme("cerulean"),
                              column(2,
                                     fileInput("archivo_csv", "Archivo:", accept = ".csv"),
                                     selectInput("ejeY", "Eje Y",
                                                 selected = "Casos",
                                                 choices = c("Value", "Nuevos")),
                                     selectInput("paises", "Paises", 
                                                 selected = "Costa Rica",
                                                 choices = paises_regiones,
                                                 multiple = T)
                              ),
                              column(5,
                                     h3("Casos por País en el Tiempo"),
                                     br(),
                                     plotlyOutput("grafico"),
                              ),
                              column(5,
                                     h3("Datos Originales"),
                                     br(),
                                     DTOutput("tabla")
                              )
                          )
                 )
)

# Creando servidor
server <- function(input, output){
    
    # Creando tabla
    datos_procesados <- reactive({
        # Leyendo lista de archivo
        archivo <- input$archivo_csv
        # Validando que existe un archivo
        validate(need(!is.null(archivo), "Cargue el archivo"))
        # Leyendo datos
        datos <- read.csv(archivo$datapath, comment.char = "#")
        # Filtrando a paises seleccionados
        datos_filtrados <- datos[datos$Country.Region %in% input$paises, ]
        # Convirtiendo columna fecha a formato fecha
        datos_filtrados$Date <- as.Date(datos_filtrados$Date)
        # Agrupando datos
        covid19_agrupado <- aggregate(Value ~ Country.Region + Date, datos_filtrados, FUN = sum)
        # Generando casos nuevos por país
        datos_lista <- casosNuevosCOVID19(covid19_agrupado)
        # Creando un solo data frame
        datos_listos <- as.data.frame(rbindlist(datos_lista))
        # Especificando valor a retornar
        return(datos_listos)
    })
    
    # Tabla para mostrar
    output$tabla <- renderDT({
        datos_procesados()
    }) 
    
    # Creando gráfico
    output$grafico <- renderPlotly({
        # Guardando gráfico en un objeto
        g <- ggplot(datos_procesados(), aes_string(x="Date", y=input$ejeY, group= "Country.Region")) +
            geom_line(aes(linetype = Country.Region, color=Country.Region))+
            geom_point(aes(color=Country.Region))+
            theme(legend.position="top")+
            labs(x = "Fecha", y = "Casos", col = "Paises", linetype = "")
        # Transformando el gráfico a plotly
        return(ggplotly(g))
    })
}

# Ejecución de la aplicación
shinyApp(ui = ui, server = server)