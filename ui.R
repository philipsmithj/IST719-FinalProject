
EnsurePackage<-function(x){
  x<-as.character(x)
  if(!require(x,character.only=TRUE)){
    install.packages(pkgs=x,
                       repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

#Install and Load Packages
EnsurePackage("ggplot2")
EnsurePackage("reshape2")
EnsurePackage("plyr")
EnsurePackage("maps")
EnsurePackage("mapproj")
EnsurePackage("shiny")
EnsurePackage("shinyapps")

library(shinyapps)
library(ggplot2)
library(reshape2)
library(plyr)
library(maps)
library(mapproj)
library(shiny)

###---Functions---###

#State Full Name
stateFullNames<-function(df){
  stateFull<-vector()
  for (i in 1:length(df$State)){
    if(is.na(state.name[match(df$State[i],state.abb)])){break}
    stateFull[i]<-state.name[match(df$State[i],state.abb)]}
  df$StateFullName<-stateFull
  return(df)}

#Fit Categories with Descriptions
catDescript<-function(df){
  desc<-vector()
  for (i in 1:length(df$Category)){
    desc[i]<-as.character(code$Description[match(df$Category[i], code$Item.Code)])}
  df$Description<-desc
  return(df)}

#Add DOL info for year
dol<-function(){
  u<-vector()
  for (i in 1:length(taxes$Year)){
    u<-Unemp$Rate[match(taxes$Year, Unemp$Year)]}
  taxes$Unemployment<-u
  return(taxes)}

###No longer used## - Add Population Estimates for 2013, stateFullNames()needs to run prior
popEst<-function(df){
  pop<-vector()
  for (i in 1:length(df$StateFullName)){}
  df$PopEst<-pop
  return(df)}

# Melt and name the raw data
mold<-function(tax,year){
  #Melt into proper format
  taxes<-melt(tax, rm.na=TRUE)
  #Rename col headers
  colnames(taxes)=c("Category", "State", "Revenue")
  #Add State's Full Names
  taxes<-stateFullNames(taxes)
  taxes$Year<-year
  return(taxes)}


#Define plot based on user input
userPlot<-function(theseStates, yearRange, cats) {
  v<-taxes
  v<-subset(v, !is.na(match(v$StateFullName,theseStates)))
  v<-subset(v, Year<=max(yearRange))
  v<-subset(v, Year>=min(yearRange))
  v<-subset(v, !is.na(match(v$Description,cats)))
  ptype<-ggplot(v)+
    geom_tile(aes(as.factor(Year),State, fill=Revenue/Population),color="lightgray")+
    coord_polar(theta='y')+
    facet_grid(.~Description)+
    scale_fill_gradient(low="khaki",high="darkred")+
    labs(fill="Revenue per person in $1K", x="Year (each ring)", title="Revenue per person (Revenue/Population) by selected State and Category")+
    theme(axis.text=element_text(color="black"))
  return(ptype)}

natlComPlot<-function(yearRange, cats){
  u<-subset(taxes, !is.na(match(taxes$Description,cats)))
  u<-subset(u, (min(yearRange)<=Year)&(Year<=max(yearRange)))
  pl<-qplot(as.factor(Year),Revenue/Population,data=u,color=Description,
            geom="smooth",group=Description,
            main="National Totals for selection",
            xlab="Year",
            ylab="Revenue per person in $1K")+
    labs(color="Tax Category")
  return(pl)}

popMap<-function(aYear){
  states<-map_data("state")
  ttaxes<-taxes
  colnames(ttaxes)[1]<-"region"
  ttaxes$region<-tolower(ttaxes$region)
  aPop<-subset(ttaxes, Year==aYear)
  aPop<-subset(aPop, Category=="T01")
  myMap<-merge(states,aPop, by="region")
  myMap<-myMap[order(myMap$order),]
  #thisPl<-qplot(long, lat, data=myMap, group=group, fill=Population, geom="polygon", xlab="",ylab="")
  thisPl<-ggplot(myMap, aes(x=long, y=lat, fill=Population/1000000, group=group))+geom_polygon()
  thisPl<-thisPl+scale_fill_gradient2(midpoint=(mean(myMap$Population)/1000000)+4,
                                      low="white",
                                      mid="darkblue",
                                      high="red",
                                      labels=comma,
                                      name="Population in millions")
  thiPl<-thisPl+coord_map()
  thisPl<-thisPl+coord_fixed()
  thisPl<-thisPl+labs(title="Lower 48 population for selected year")
  thisPl<-thisPl+theme(axis.text=element_text(color="white"),
                       axis.title=element_text(color="white"),
                       axis.ticks=element_line(color="white"),
                       panel.background=element_rect(fill="steelblue"))
  return(thisPl)}

stacked<-function(aYear,wPop,cats){
  d<-subset(taxes, Year==aYear)
  c<-vector()
  for (i in 1:length(d$Description)){
   c[i]<-cats[match(d$Description[i],cats)]}
  d$disp<-c
  if(wPop){
    g<-ggplot(d, aes(StateFullName,Revenue/Population))+
      labs(y="Revenue per person in $1 thousands")}
  if(!wPop){
    g<-ggplot(d, aes(StateFullName,Revenue/1000))+
      labs(y="Revenue in $1 millions")}
  stack<-g+geom_bar(stat="identity", aes(fill=disp,position="stack"))+
    x90+
    labs(fill="Tax Category",x="State",title="Selective category highlight of States' annual revenue")+
    scale_y_continuous(labels=comma)+
    theme(panel.grid.major=element_line(size=0.5,color="lightgray"),
          axis.ticks=element_line(color="white"))
  return(stack)}

###---Execution---###

#Get Data
code<-read.csv(file="codes.csv", sep=",")
t14<-read.csv(file="14staxcd.csv", sep=",")
t13<-read.csv(file="13staxcd.csv", sep=",")
t12<-read.csv(file="12staxcd.csv", sep=",")
t11<-read.csv(file="11staxcd.csv", sep=",")
t10<-read.csv(file="10staxcd.csv", sep=",")
t09<-read.csv(file="09staxcd.csv", sep=",")
t08<-read.csv(file="08staxcd.csv", sep=",")
t07<-read.csv(file="07staxcd.csv", sep=",")
t06<-read.csv(file="06staxcd.csv", sep=",")
t05<-read.csv(file="05staxcd.csv", sep=",")
p<-read.csv(file="pop.csv", sep=",")
colnames(p)<-c("StateFullName",1999:2014)
p<-melt(p,rm.na=TRUE, measure.vars = as.character(1999:2014))
colnames(p)<-c("StateFullName", "Year", "Population")

#Figures from Department of Labor, mean annual unemployment estimates
Year<-c(2005:2014)
Rate<-c(5.083,4.608,4.616,5.8,9.28,9.608,8.94,8.06, 7.366,6.15)
Unemp<-data.frame(Year, Rate)

t14<-mold(t14,2014)
t13<-mold(t13,2013)
t12<-mold(t12,2012)
t11<-mold(t11,2011)
t10<-mold(t10,2010)
t09<-mold(t09,2009)
t08<-mold(t08,2008)
t07<-mold(t07,2007)
t06<-mold(t06,2006)
t05<-mold(t05,2005)

taxes<-merge(t14,t13, all=TRUE)
taxes<-merge(taxes, t12, all=TRUE)
taxes<-merge(taxes, t11, all=TRUE)
taxes<-merge(taxes, t10, all=TRUE)
taxes<-merge(taxes, t09, all=TRUE)
taxes<-merge(taxes, t08, all=TRUE)
taxes<-merge(taxes, t07, all=TRUE)
taxes<-merge(taxes, t06, all=TRUE)
taxes<-merge(taxes, t05, all=TRUE)
taxes<-catDescript(taxes)
taxes<-merge(p,taxes, all=TRUE)
taxes$Year<-as.integer(as.numeric_version(taxes$Year))
taxes<-subset(taxes, Year>=2005)
taxes$StateFullName<-as.character(taxes$StateFullName)
taxes<-dol()
taxes$Unemployment<-taxes$Unemployment/100
taxes<-subset(taxes, !(is.na(taxes$Description)))

#Some smaller sets within 
income<-subset(taxes, Description=="Individual Income Taxes")

#Text for About page
t<-"Ten Years of State Taxes 2005 - 2014"

## Dress-up##
# X axis text 90deg
x90<-theme(axis.text.x=element_text(angle=90))
# Thousands seperator, no scientific
comma<-function(x) format(x,scientific=FALSE,big.mark = ",")

narr<-h5("There are only a few things you can be sure of in life. 
         As the saying goes, there's death, then there's taxes. 
         For this project, I wanted to conduct a national comparison between states' taxes. 
         While we're all under the same set of rules for federal taxes, there are 
         differences in each state that might make it worthwhile to weigh these differences 
         when choosing a state to focus on while job hunting. 
         First, I would like you to look at the overview section to get a feel for 
         what is going on at a national scale over the last ten years. 
         Notice the steady increase? 
         Can you find the recession? 
         Has the nation bounced back? 
         I encourage you to use the comparison tool to explore the data to come to your own conclusions and perhaps form your own hypothesis. ")

aud1<-h4("Audiences for this project are:")
aud2<-h5("1. American Public so that they can be informed on what their state is collecting and how.")
aud3<-h5("2. Public Administrators")
aud4<-h5("3. Expanding or relocating companies and organizations")
aud5<-h5("4. Anyone estimating the cost of living (like a grad student looking for a job and a place to put down roots)")

q1<-h4("Questions to be answered:")
q2<-h5("1. What is the highest earner for a state?")
q3<-h5("2. What is the revenue per person for a state or group of states and how do they compare to one another?")
q4<-h5("3. What happened during the recession ~2008?")
q5<-h5("4. How do tax rates compare over the years? Are we paying more?")
q6<-h5("5. Are there any indicators for economic health such as increased revenue from sales tax?")

myName<-h4("Compiled and processed by Philip J. smith")

s<-h3("Sources")
s1<-h5("Tax data sourced from the Unitied States Census Bureau: 
      http://www.census.gov/govs/statetax/")
s2<-h5("Population data sourced from the Unitied States Census Bureau: 
       http://www.census.gov/popest/data/historical/index.html")
s3<-h5("Unemployment data sourced from the Unitied States Department of Labor: 
       http://data.bls.gov/pdq/SurveyOutputServlet")

n1<-h3("Notes")
n2<-h5("All 'Revenue' values are in $1K [or $1 * 1000]") 

#Static Plots

overviewPlot<-qplot(StateFullName,Description,data=taxes,geom="point",size=Revenue/1000000,color=Population,
                    main="Total Revenue by Category by State (Totaled from 2005 to 2014)",
                    xlab="State",
                    ylab="Tax Category") +x90
overviewPlot<-overviewPlot+scale_color_gradient(low="darkblue", high="red", labels=comma)+
  scale_size(labels=comma, name="Decade's Revenue in $1 billion")


overTime<-qplot(as.factor(Year),Revenue/1000,data=taxes,group=1,geom="smooth", method="gam", formula=y~s(x,bs="cs"),
                main="Raw Revenue Growth (all states) from 2005 to 2014 (mean centered, +/- 1 standard deviation)",
                xlab="Year",
                ylab="Total Revenue in $1 million")+
  scale_y_continuous(labels=comma)+
  annotate("text", x=5.25,y=625,label="Continued growth despite narrowing deviations")

##Start Building Application
#Est Shiny objects
shinyUI(navbarPage(title="Ten Years of State Taxes 2005 - 2014",
                   tabPanel(title="About",h2(t),narr,
                            aud1,aud2,aud3,aud4,aud5,div(),div(),
                            q1,q2,q3,q4,q5,q6,div(),div(),
                            #n1,n2,div(),
                            s,s1,s2,s3,div(),div(),
                            myName),
                   tabPanel("Overview",
                            h5("These plots are used to find our high and low earners. 
                               In the following graphs you can explore why."),
                            plotOutput("overPlot"),
                            plotOutput("timePlot")),
                   tabPanel("Population's Affect",
                            sidebarLayout(sidebarPanel(
                              sliderInput("sYear", "Population Year", min(taxes$Year),max(taxes$Year),
                                          value=2014, step=1),
                              checkboxInput("withPop", "Factor in Population (Revenue/Population)"),
                              checkboxGroupInput("checkGroup", "Categories", unique(taxes$Description),
                                                 selected=unique(taxes$Description)[1])),
                              mainPanel(h4("Use these to tailor categories that apply to your situation."),
                                        h5("--Fullscreen is recommended--"),
                                plotOutput("popPlot"),
                                plotOutput("stackPlot")))),
                   tabPanel("Comparison",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("year","Year",min(taxes$Year),max(taxes$Year),value=c(2008,2011),step=1),
                                selectInput("states", "States (select at least one)",taxes$StateFullName,multiple = TRUE,selected="Alabama"),
                                checkboxGroupInput("catGroup", "Categories (select at least one, up to 6)", unique(taxes$Description),selected=unique(taxes$Description)[1])
                              ),
                              mainPanel(h3("Comparison"),
                                        h5("Recommend this be viewed at full screen"),
                                        plotOutput("thisPlot"),
                                        plotOutput("natPlot")
                                        
                              )
                            )
                   )
)
)