require 'mixpanel_client'
require 'json'
require 'csv'

puts 'Starting Script...'

MIXPANEL_API_KEY = "ce370ab09a166e168d448080b55715f6"
MIXPANEL_API_SECRET = "af3b32cc21c7b6e91b71f7c0417735d2"

client = Mixpanel::Client.new(
api_key:    MIXPANEL_API_KEY,
api_secret: MIXPANEL_API_SECRET
)

# mixpanel_data = client.request('engage', where: "(properties[\"latest_ad_search\"] != null)" )

# json = mixpanel_data["results"][0].to_json
# puts mixpanel_data["results"].length
# puts JSON.pretty_generate( JSON.parse( json ) )

mixpanel_data = client.request('export', 
                                from_date: "2015-12-17",
                                to_date: Time.now.strftime("%Y-%m-%d"),
                                event: ["Completed Order"],
                                where: "(properties[\"latest_ad_search\"])")

                      
output_filename = "completed_orders.csv"
CSV.open(output_filename, "wb", {:encoding => "utf-8", force_quotes: false }) do |csv|
  csv << ["mixpanel_event_timestamp", 
          "mixpanel_event_readable_timestamp",
          "user_id",
          "operating_system",
          "order_id",
          "latest_ad_utm_campaign",
          "latest_ad_utm_source",
          "latest_ad_utm_medium",
          "latest_ad_awcreativeid",
          "latest_ad_awkeyword",
          "latest_ad_awmatchtype",
          "latest_ad_awnetwork",
          "latest_ad_awposition",
          "latest_ad_device",
          "latest_ad_psgeo",
          "latest_ad_search"
        ]

  mixpanel_data.each do |completed_order|
    completed_order_properties = completed_order['properties']
    csv << [completed_order_properties['time'],
            readable_time = Time.at( completed_order_properties['time'] ).strftime("%Y-%m-%dT%H:%M:%S %z"), # "2015-12-27T11:16:17"
            completed_order_properties['id'],
            completed_order_properties['$os'],
            completed_order_properties['orderId'],
            completed_order_properties['latest_ad_utm_campaign'],
            completed_order_properties['latest_ad_utm_source'],
            completed_order_properties['latest_ad_utm_medium'],
            completed_order_properties['latest_ad_awcreativeid'],
            completed_order_properties['latest_ad_awkeyword'],
            completed_order_properties['latest_ad_awmatchtype'],
            completed_order_properties['latest_ad_awnetwork'],
            completed_order_properties['latest_ad_awposition'],
            completed_order_properties['latest_ad_device'],
            completed_order_properties['latest_ad_psgeo'],
            completed_order_properties['latest_ad_search']
           ]
    #puts JSON.pretty_generate( JSON.parse(completed_order_properties.to_json))
  end

end

puts 'File accessible through: '
puts
puts "#{output_filename}"
puts
puts 'All Done!'