package org.http4s
package twirl

import org.scalacheck.{Arbitrary, Gen}

class TwirlSpec extends Http4sSpec {
  implicit val arbCharset: Arbitrary[Charset] = Arbitrary {
    Gen.oneOf(
      Charset.`UTF-8`,
      Charset.`ISO-8859-1`,
      Charset.fromString("Windows-1251").yolo,
      Charset.fromString("GB2312").yolo,
      Charset.fromString("Shift-JIS").yolo,
      Charset.fromString("Windows-1252").yolo
    )
  }

}
