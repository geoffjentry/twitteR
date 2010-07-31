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

setMethod("text", signature="directMessage", function(x) {
    x@text
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
