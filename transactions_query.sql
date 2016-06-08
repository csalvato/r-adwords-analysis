"select
  t.created_at as transaction_date
  , u.name as user_name
  , u.id as app_user_id
  , t.discount_amount
  , t.credit
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
  , (CASE
  WHEN t.refunded = 't' THEN
     0
    WHEN t.credit > 0 THEN
     ti.price - t.credit
    ELSE
     ti.price
    END
  ) as money_in_the_bank_paid_to_us
  , (CASE
  WHEN d.type = 'GiftCard'
  THEN 'Gift Card'
  WHEN ti.recurring = 't'
  THEN 'Recurring Food Purchase'
  ELSE 
  'One-Time Meal Purchase'
  END
  ) as purchase_type
  from
  transactions t
  inner join
  transaction_items ti on t.id = ti.transaction_id
  left outer join
  discounts d on ti.discount_id = d.id
  inner join
  users u on t.user_id = u.id
where
  t.created_at between date'@{start_date}' and (date '@{end_date}' + integer '1') 
  and t.complete = TRUE
  -- Adding 1 makes sure that no values are cut off by time zones.
order by t.created_at desc"