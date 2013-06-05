importUsers = function(rawData, conversionFunc=jsonToUsers) {
  return(importObj(rawData, conversionFunc))
}

importStatuses = function(rawData, conversionFunc=jsonToStatuses) {
  return(importObj(rawData, conversionFunc))
}

importTrends = function(rawData, conversionFunc=jsonToTrends) {
  return(importObj(rawData, conversionFunc))
}

importObj = function(rawData, conversionFunc) {
  return(conversionFunc(rawData))
}

jsonToUsers = function(rawData) {
  return(sapply(rawData, buildUser))
}

jsonToStatuses = function(rawData) {
  return(sapply(rawData, buildStatus))
}

jsonToTrends = buildTrendLocationDf
