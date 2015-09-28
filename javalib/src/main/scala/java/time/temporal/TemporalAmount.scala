package java.time.temporal

trait TemporalAmount {
  def get(unit: TemporalUnit): Long

  def getUnits(): java.util.List[TemporalUnit]

  def addTo(temporal: Temporal): Temporal

  def subtractFrom(temporal: Temporal): Temporal
}
