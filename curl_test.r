library(httr)

url <- "https://adinsight.api.bingads.microsoft.com/Api/Advertiser/AdInsight/V10/AdInsightService.svc"

body <- '<s:Envelope xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
  <s:Header xmlns="Microsoft.Advertiser.AdInsight.Api.Service">
<Action mustUnderstand="1">GetHistoricalKeywordPerformance</Action>
<ApplicationToken i:nil="false"></ApplicationToken>
<AuthenticationToken i:nil="false"></AuthenticationToken>
<CustomerAccountId i:nil="false"></CustomerAccountId>
<CustomerId i:nil="false"></CustomerId>
<DeveloperToken i:nil="false"></DeveloperToken>
<Password i:nil="false"></Password>
<UserName i:nil="false"></UserName>
</s:Header>
<s:Body>
<GetHistoricalKeywordPerformanceRequest xmlns="Microsoft.Advertiser.AdInsight.Api.Service">
<Keywords i:nil="false" xmlns:a1="http://schemas.microsoft.com/2003/10/Serialization/Arrays">
<a1:string></a1:string>
</Keywords>
<TimeInterval i:nil="false"></TimeInterval>
<TargetAdPosition i:nil="false"></TargetAdPosition>
<MatchTypes i:nil="false">
<MatchType></MatchType>
</MatchTypes>
<Language i:nil="false"></Language>
<PublisherCountries i:nil="false" xmlns:a1="http://schemas.microsoft.com/2003/10/Serialization/Arrays">
<a1:string></a1:string>
</PublisherCountries>
<Devices i:nil="false" xmlns:a1="http://schemas.microsoft.com/2003/10/Serialization/Arrays">
<a1:string></a1:string>
</Devices>
</GetHistoricalKeywordPerformanceRequest>
</s:Body>
</s:Envelope>'

# post the api request
response <- POST(url,
     c(verbose(),add_headers( "Content-Type" = "text/xml; charset=utf-8",
                              "CustomerAccountId"  =  "54058098",
                              "CustomerId"  =  "B0243UCW",
                              "DeveloperToken"  =  "024AH54102585194",
                              "Password"  =  "piKablu6420",
                              "UserName"  =  "chris@mypowersupply.com")),
     body = body)