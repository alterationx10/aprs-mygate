package models

import dev.alteration.branch.friday.Json.JsonString
import dev.alteration.branch.friday.{Json, JsonCodec, JsonDecoder, JsonEncoder}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

given JsonEncoder[LocalDateTime] = (a: LocalDateTime) =>
  JsonString(
    a.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  )

given JsonDecoder[LocalDateTime] = (json: Json) =>
  Try(json.strVal).map(LocalDateTime.parse)

case class AprsPacket(
    timestamp: LocalDateTime,
    sender: String,
    destPath: String,
    payload: String,
    raw: String
) derives JsonCodec
