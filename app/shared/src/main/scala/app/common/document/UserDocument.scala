package app.common.document

import hydro.common.OrderToken

case class UserDocument(
    documentId: Long,
    name: String,
    orderToken: OrderToken,
)
