package java.time.temporal

trait TemporalField {
  // Not implemented
  // def getDisplayName(locale: java.util.Locale): String

  def getBaseUnit(): TemporalUnit

  def getRangeUnit(): TemporalUnit

  def range(): ValueRange

  def isDateBased(): Boolean

  def isTimeBased(): Boolean

  def isSupportedBy(temporal: TemporalAccessor): Boolean

  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange

  def getFrom(temporal: TemporalAccessor): Long

  def adjustInto[R <: Temporal](temporal: R, value: Long): R

  // Not implemented
  // def resolve(fieldValues: java.util.Map[TemporalField, Long],
  //     partialTemporal: TemporalAccessor,
  //     resolverStyle: ResolverStyle): TemporalAccessor
}
