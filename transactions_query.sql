"select
  t.created_at as transaction_date
  , du.name as user_name
  , t.discount_amount
  , ti.credit_used
  , t.sales_tax_amount
  , (CASE t.refunded
  WHEN 't' THEN
  ti.price
  ELSE
  0
  END) as refund_amount
  , (CASE 
  WHEN d.type = 'GiftCard'
  THEN d.amount
  ELSE
  ti.price - t.sales_tax_amount + t.discount_amount
  END) as retail_price
  , (CASE t.refunded
  WHEN 't' THEN
  0
  ELSE
  ti.price - ti.credit_used
  END) as money_in_the_bank_paid_to_us
  , (CASE
  WHEN d.type = 'GiftCard'
  THEN 'Gift Card'
  WHEN ti.recurring = 't'
  THEN 'Recurring Food Purchase'
  ELSE 
  'One-Time Meal Purchase'
  END
  ) as purchase_type
  --, ti.name as meal_plan
  --, ti.recurring
  --  , l.name
  --  , l.city
  --  , l.state
  --  , l.store_front_id
  --  , l.home_delivery
  , mp.*
  from
  transactions t
  inner join
  transaction_items ti on t.id = ti.transaction_id
  inner join
  dim_users du on t.user_id = du.user_id
  inner join
  (
  select distinct on (gsm.user_id)
  gsm.*
  from
  stage.gs_mixpanel gsm
  order by gsm.user_id, mixpanel_event_timestamp
  ) mp on du.user_id = mp.user_id
  --left outer join
  --  locations l on ti.location_id = l.id
  left outer join
  discounts d on ti.discount_id = d.id
where
  t.created_at between '@{start_date}' and '@{end_date}'
--  AND
--  t.refunded IS NOT TRUE
order by t.created_at desc"