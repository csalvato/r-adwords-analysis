#' Create plot of cohorts who converted from the keyword "Paleo Meals"
#'
#' Takes in an AdWords or Bing event log.
#'
#' @param keywords_elog A data frame containing keywords and transaction event log data.
#' @export
#' @examples
#' paleo_cohort_views(keywords_elog)

paleo_cohort_views <- function(keywords_elog) {
    paleo_meals_cohort <- filter(keywords_elog,keyword == "paleo meals")

    ##assign ordinal number to weeks
    week_number <- numeric()
    for(i in 1: length(paleo_meals_cohort$week)) {
            week_number[i] <- which(unique(paleo_meals_cohort$week)==paleo_meals_cohort$week[i])
    }
    paleo_meals_cohort <- cbind(paleo_meals_cohort,week_number)

    ##assign each user_id to a 'cohort_week' based on week of first order using the 'paleo meals' keyword
    ##all future occurrences of that user_id are labelled with the original cohort_week
    ##if no user_id for a given transaction, cohort_week = week_number

    paleo_meals_cohort <- mutate(paleo_meals_cohort,cohort_week = rep(0,length(paleo_meals_cohort$week)))

    for(i in 1:length(unique(paleo_meals_cohort$week_number))) {
            users <- paleo_meals_cohort[which(paleo_meals_cohort$week_number==i),"user_id"]
            
            for(k in 1:length(paleo_meals_cohort$user_id)) {
                    if(paleo_meals_cohort$cohort_week[k] > 0) {next} 
                    if(paleo_meals_cohort$user_id[k] %in% users) {
                            paleo_meals_cohort$cohort_week[k] <- i}
                    if(is.na(paleo_meals_cohort$user_id[k]))
                    {paleo_meals_cohort$cohort_week[k] <- paleo_meals_cohort$week_number[k]}
            }
    }

    ##add variable with cohort_week number renamed as "cohort_#"
    cohort_week2 <- character()
    for(i in 1:length(paleo_meals_cohort$week_number)) {
            cohort_week2[i] <- paste0("cohort_",paleo_meals_cohort$cohort_week[i])
    }
    paleo_meals_cohort <- cbind(paleo_meals_cohort,cohort_week2)


    ##Organize dataframes and prepare for plotting

    ##Dataframe of Total Cost, Contribution and ROI for paleo meals cohorts

    paleo_meals_cohorts_total_ROI <- paleo_meals_cohort %>%
            group_by(cohort_week) %>%
            summarize(cohort_cost = sum(cost, na.rm = TRUE),
                      cohort_contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
            mutate(cohort_ROI = cohort_contribution - cohort_cost) %>%
            gather(type,value,cohort_cost,cohort_contribution,cohort_ROI)

    ##Set up ROI as a separate variable to allow it to be plotted differently

    paleo_meals_cohorts_total_ROI <- as.data.frame(paleo_meals_cohorts_total_ROI)
    cohort_ROI <- filter(paleo_meals_cohorts_total_ROI,type == "cohort_ROI")

    paleo_meals_cohorts_total_ROI <- paleo_meals_cohorts_total_ROI %>%
                            filter(type != "cohort_ROI") %>%
                            merge(cohort_ROI,by="cohort_week",all.x=TRUE) %>%
                            arrange(cohort_week,desc(type.x))

    ##make cost values negative
    for(i in 1:length(paleo_meals_cohorts_total_ROI$cohort_week)) {
            if(paleo_meals_cohorts_total_ROI[i,"type.x"]=="cohort_cost") {
                    paleo_meals_cohorts_total_ROI[i,"value.x"] <- -(paleo_meals_cohorts_total_ROI[i,"value.x"])
            } else {next}
    }

    ##Dataframe of cumulative cost, contribution and ROI over time.
    paleo_meals_cohorts_over_time <- paleo_meals_cohort %>%
            group_by(cohort_week,week_number) %>%
            summarize(cost = sum(cost, na.rm = TRUE),
                      contribution = sum(money_in_the_bank_paid_to_us,na.rm=TRUE) *.25) %>%
            mutate(cum_contribution = cumsum(contribution), 
                   cum_cost = cumsum(cost),
                   cum_ROI = cum_contribution - cum_cost) %>%
            gather(type,value,cum_cost,cum_contribution,cum_ROI)

    ##Create variables to track the ROI breakeven week for a given cohort (if it exists)

    paleo_meals_cohorts_over_time <- as.data.frame(paleo_meals_cohorts_over_time)

    paleo_meals_cohort_breakeven <- filter(paleo_meals_cohorts_over_time,type=="cum_ROI")

    ##breaken_week is a logical variable tracking if a Cohort broke even (ROI crossed over zero) in a given week

    breakeven_week <- logical()
    for(i in 2:length(paleo_meals_cohort_breakeven$week)) {
            if(paleo_meals_cohort_breakeven$value[i] > 0
               & paleo_meals_cohort_breakeven$value[i-1] < 0
               & paleo_meals_cohort_breakeven$cohort_week[i] == paleo_meals_cohort_breakeven$cohort_week[i-1]
                  |  paleo_meals_cohort_breakeven$value[i]>0
                     & paleo_meals_cohort_breakeven$cohort_week[i] != paleo_meals_cohort_breakeven$cohort_week[i-1])
                     
              {breakeven_week[i] <- 1} else {breakeven_week[i] <- 0}
            }
            
    paleo_meals_cohort_breakeven <- cbind(paleo_meals_cohort_breakeven,breakeven_week)

    ##create new variable (weeks_unti_breakeven) which gives the number of weeks it took that cohort to break even
    cohort_vector <- unique(paleo_meals_cohort_breakeven$cohort_week)
    weeks2breakeven <- numeric()

    for(i in 1:length(cohort_vector)) {
            bkevn <- paleo_meals_cohort_breakeven[which(paleo_meals_cohort_breakeven$breakeven_week == 1 & paleo_meals_cohort_breakeven$cohort_week == cohort_vector[i]),"week_number"]
            frst <- min(paleo_meals_cohort_breakeven[which(paleo_meals_cohort_breakeven$cohort_week==cohort_vector[i]),"week_number"])
            if(length(bkevn-frst)==0) {
                    weeks2breakeven[i] <- NA} else {
                            weeks2breakeven[i] <- ((bkevn - frst)+1)
                    } 
            }
    weeks_until_breakeven <- numeric()
    for(i in 1:length(paleo_meals_cohort_breakeven$cohort_week)) {
            weeks_until_breakeven[i] <- weeks2breakeven[which(paleo_meals_cohort_breakeven$cohort_week[i] == cohort_vector)]
    }

    paleo_meals_cohort_breakeven <- cbind(paleo_meals_cohort_breakeven,weeks_until_breakeven)


    ###################################### PALEO MEALS COHORT PLOTS ###################################


    ##Weekly Cohort Plot 1: Stacked barchart of Contribution/Cost per Cohort Week with Overlaid ROI points
    ##!!Need to figure out how to add "cohort_ROI" line to the legend.

    max_cw <- max(paleo_meals_cohort$cohort_week)
    ymax <- ceiling(signif(max(paleo_meals_cohorts_total_ROI$value.x,paleo_meals_cohorts_total_ROI$value.y),2))
    ymin <- floor(signif(min(paleo_meals_cohorts_total_ROI$value.x,paleo_meals_cohorts_total_ROI$value.y),2))

    plot( 
            ggplot(
                    paleo_meals_cohorts_total_ROI, 
                    aes(x=cohort_week,y=value.x, fill=type.x)) +
                    geom_bar(stat="identity", position="identity") +
                    geom_line(data=paleo_meals_cohorts_total_ROI,aes(x=cohort_week,y=value.y),linetype=1,size=1,color="grey55") +
                    geom_point(data=paleo_meals_cohorts_total_ROI,aes(x=cohort_week,y=value.y,color="cohort_ROI"),shape=21,size=2,fill="chartreuse1",color="turquoise4") +
                    scale_x_continuous(breaks=seq(1,max_cw,1)) +
                    scale_y_continuous(breaks=seq(ymin,ymax,500),labels=scales::dollar) + 
                    scale_fill_manual(values=c("springgreen4","red2")) +
                    guides(fill=guide_legend(title=NULL)) +
                    labs(title="Contribution, Cost, and ROI by Cohort Week",x="Cohort Week",y="") 
    )

    ##Weekly Cohort Plot 2: Number of weeks it took each cohort to break even

    max_be <- max(paleo_meals_cohort_breakeven$weeks_until_breakeven,na.rm=TRUE)
    plot(
            ggplot(
                    paleo_meals_cohort_breakeven, 
                    aes(x=cohort_week,y=weeks_until_breakeven)) +
                    geom_bar(stat="identity", position="dodge") +
                    scale_x_continuous(breaks=seq(1,max_cw,1)) +
                    scale_y_continuous(breaks=seq(0,max_be,1)) +
                    labs(title="Weeks to Break Even per Cohort Week",x="Cohort Week",y="Number of Weeks")
    )
}