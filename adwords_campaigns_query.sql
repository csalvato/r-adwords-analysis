"select
  *
from 
  stage.gs_adwords_campaigns awc
where 
  awc.date between '@{start_date}' and '@{end_date}'
order by awc.date desc"