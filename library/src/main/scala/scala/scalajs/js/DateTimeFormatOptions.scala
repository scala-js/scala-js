package scala.scalajs.js

import scala.scalajs.js

/**
  * An object adjusting the output format. Corresponds to the options parameter of the
  * Intl.DateTimeFormat() constructor. The timeStyle option must be undefined, or a TypeError would be thrown.
  * If weekday, year, month, and day are all undefined, then year, month, and day will be set to "numeric".
  *
  * MDN
  */
trait DateTimeFormatOptions extends js.Object {

  /**
   * The date formatting style to use when calling `format()`.
   * Possible values include: `"full"`, `"long"`, `"medium"`, `"short"`.
   *
   * MDN
   */
  var dateStyle: js.UndefOr[String] = js.undefined

  /**
   * The time formatting style to use when calling `format()`.
   * Possible values include: `"full"`, `"long"`, `"medium"`, `"short"`.
   *
   * MDN
   */
  var timeStyle: js.UndefOr[String] = js.undefined

  /**
   * Calendar. Possible values include: `"buddhist"`, `"chinese"`, `"coptic"`, `"dangi"`, `"ethioaa"`,
   * `"ethiopic"`, `"gregory"`, `"hebrew"`, `"indian"`, `"islamic"`, `"islamic-umalqura"`, `"islamic-tbla"`,
   * `"islamic-civil"`, `"islamic-rgsa"`, `"iso8601"`, `"japanese"`, `"persian"`, `"roc"`, `"islamicc"` (deprecated).
   *
   * MDN
   */
  @deprecated
  var calendar: js.UndefOr[String] = js.undefined


  /**
   * The formatting style used for day periods like "in the morning", "am", "noon", "n" etc.
   * Possible values include: `"narrow"`, `"short"`, `"long"` (only has an effect if a 12-hour clock is used).
   *
   * MDN
   */
  var dayPeriod: js.UndefOr[String] = js.undefined

  /**
   * Numbering System. Possible values include: `"arab"`, `"arabext"`, `"bali"`, `"beng"` ,` "deva"` ,`
   * "fullwide"` ,` "gujr"` ,` "guru"` ,` "hanidec"` ,` "khmr"` ,` "knda"` ,` "laoo"` ,` "latn"` ,` "limb"`
   * ,` "mlym"` ,` "mong"` ,` "mymr"` ,` "orya"` ,` "tamldec"` ,` "telu"` ,` "thai"` ,` "tibt"`
   *
   * MDN
   */
  var numberingSystem: js.UndefOr[String] = js.undefined

  /**
   * The locale matching algorithm to use.
   * Possible values are: "`lookup`" and "`best fit`"; the default is "`best fit`".
   *
   * MDN
   */
  var localeMatcher: js.UndefOr[String] = js.undefined

  /**
   * The time zone to use.
   * The only value implementations must recognize is "`UTC`"; the default is the runtime's default time zone.
   * Implementations may also recognize the time zone names of the IANA time zone database, such as "`Asia/Shanghai`",
   * "`Asia/Kolkata`", "`America/New_York`".
   *
   * MDN
   */
  var timeZone: js.UndefOr[String] = js.undefined

  /**
   * Whether to use 12-hour time (as opposed to 24-hour time).
   * Possible values are `true` and `false`; the default is locale dependent.
   *
   * MDN
   */
  var hour12: js.UndefOr[Boolean] = js.undefined

  /**
   * The hour cycle to use. Possible values are "`h11`", "`h12`", "`h23`", or "`h24`".
   * This option overrides the `hour12` option.
   * If no `hourCycle` is provided, it will be set to `h12` if `hour12` is true, or `h23` if `hour12` is false.
   *
   * MDN
   */
  var hourCycle: js.UndefOr[String] = js.undefined

  /**
   * The format matching algorithm to use. Possible values are: "`basic`" and "`best fit`"; the default is "`best fit`".
   *
   * MDN
   */
  var formatMatcher: js.UndefOr[String] = js.undefined

  /**
   * The representation of the weekday. Possible values are: "`narrow`", "`short`", "`long`".
   *
   * MDN
   */
  var weekday: js.UndefOr[String] = js.undefined

  /**
   * The representation of the era. Possible values are: "`narrow`", "`short`", "`long`".
   *
   * MDN
   */
  var era: js.UndefOr[String] = js.undefined

  /**
   * The representation of the year. Possible values are: "`numeric`", "`2-digit`".
   *
   * MDN
   */
  var year: js.UndefOr[String] = js.undefined

  /**
   * The representation of the month. Possible values are: "`numeric`", "`2-digit`", "`narrow`", "`short`", "`long`.
   *
   * MDN
   */
  var month: js.UndefOr[String] = js.undefined

  /**
   * The representation of the day. Possible values are: "`numeric`", "`2-digit`.
   *
   * MDN
   */
  var day: js.UndefOr[String] = js.undefined

  /**
   * The representation of the hour. Possible values are: "`numeric`", "`2-digit`.
   *
   * MDN
   */
  var hour: js.UndefOr[String] = js.undefined

  /**
   * The representation of the minute. Possible values are: "`numeric`", "`2-digit`.
   *
   * MDN
   */
  var minute: js.UndefOr[String] = js.undefined

  /**
   * The representation of the second. Possible values are: "`numeric`", "`2-digit`.
   *
   * MDN
   */
  var second: js.UndefOr[String] = js.undefined

  /**
   * The number of fractional second digits to show in the output.
   * Possible values are 0, 1, 2, or 3; the default is 0.
   *
   * MDN
   */
  var fractionalSecondDigits: js.UndefOr[Int] = js.undefined
}
