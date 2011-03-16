buildDM <- function(json) {
  sender <- buildUser(json[['sender']])
  recip <- buildUser(json[['recipient']])
  if (is.null(json$text))
    json$text <- character()
  if (is.null(json$"recipient_id"))
    json[['recipient_id']] <- numeric()
  if (is.null(json[['sender_id']]))
    json[['sender_id']] <- numeric()
  if (is.null(json[['sender_screen_name']]))
    json[['sender_screen_name']] <- character()
  if (is.null(json[['created_at']]))
    json[['created_at']] <- character()
  if (is.null(json[['recipient_screen_name']]))
    json[['recipient_screen_name']] <- character()
  if (is.null(json[['id']]))
    json[['id']] <- numeric()
  new("directMessage",
      text=json[['text']],
      recipientSN=json[['recipient_screen_name']],
      created=json[['created_at']],
      recipientID=json[['recipient_id']],
      sender=sender,
      recipient=recip,
      senderID=json[['sender_id']],
      id=json[['id']],
      senderSN=json[['sender_screen_name']]
      )
}

setClass("directMessage",
         representation(
                        text="character",
                        recipientSN="character",
                        created="character",
                        recipientID="numeric",
                        sender="user",
                        recipient="user",
                        senderID="numeric",
                        id="numeric",
                        senderSN="character"
                        )
         )

setMethod("show", signature="directMessage", function(object) {
    print(paste(screenName(object@sender), "->",
                screenName(object@recipient),  ":",
                object@text, sep=""))
})


setMethod("recipientSN", signature="directMessage", function(object) {
    object@recipientSN
})

setMethod("created", signature="directMessage", function(object) {
    object@created
})

setMethod("recipientID", signature="directMessage", function(object) {
    object@recipientID
})

setMethod("sender", signature="directMessage", function(object) {
    object@sender
})

setMethod("recipient", signature="directMessage", function(object) {
    object@recipient
})

setMethod("senderID", signature="directMessage", function(object) {
    object@senderID
})

setMethod("id", signature="directMessage", function(object) {
    object@id
})

setMethod("senderSN", signature="directMessage", function(object) {
    object@senderSN
})
