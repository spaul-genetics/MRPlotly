#' Plot TwoSampleMR leave-one-out crossvalidation
#'
#' @author Subrata Paul
#' @examples
#' bmi_exp_dat <- extract_instruments(outcomes = c('ieu-a-2', 'ieu-a-9', 'ieu-a-12'))
#' chd_out_dat <- extract_outcome_data(snps = bmi_exp_dat$SNP, outcomes = 'ieu-a-7')
#' dat <- harmonise_data(bmi_exp_dat, chd_out_dat)
#' res_loo <- mr_leaveoneout(dat)
#' res.loo.plotly(res_loo)
#'
#' @param `res_loo`, output from `mr_leaveoneout`
#' @export
#'

res.loo.plotly<-function(res_loo){

  res.loo.single.plotly<-function(res_loo){
    res_loo = res_loo%>%mutate(upci = b + qnorm(0.975)*se,
                               downci = b-qnorm(0.975)*se,
                               tot = ifelse(SNP == 'All', 'All', 'SNP'))%>%
      group_by(id.exposure, id.outcome, tot)%>%
      arrange(b, .by_group = T)

    fig = res_loo%>%
      plot_ly(x = ~b,
              y = ~SNP,
              type = 'scatter',
              error_x = ~list(array = se*1.96),
              color = ~tot,
              mode = 'markers',
              hovertemplate = paste(
                "<b>%{y}</b><br>",
                "MR Effect = %{x:.4f} <span>&#177;</span> %{error_x.array:.4f} </br>",
                "<extra></extra>"
              ),
              hoveron = 'points+fills'
      )
    fig = fig%>%layout(xaxis = list(title = 'MR effect size'),
                       yaxis = list(title = '', type = 'category', categoryarray = ~SNP, categoryorder = 'trace', autorange = T))
    return(fig)
  }

  plot_names = expand.grid(exposures = unique(res_loo$id.exposure), outcomes = unique(res_loo$id.outcome))
  plot_names$names = paste(plot_names$exposures, plot_names$outcomes, sep = '.')

  if(nrow(plot_names)==1){
    return(
      res.loo.single.plotly(
        res_loo%>%dplyr::filter(id.exposure == plot_names$exposures[1] & id.outcome == plot_names$outcomes[1]))
    )
  }else{
    all_loo_plotly<-list()
    for(i in 1:nrow(plot_names)){
      all_loo_plotly[[plot_names$names[i]]] <- res.loo.single.plotly(
        res_loo%>%dplyr::filter(id.exposure == plot_names$exposures[i] & id.outcome == plot_names$outcomes[i])
      )
    }
    return(all_loo_plotly)
  }

}
