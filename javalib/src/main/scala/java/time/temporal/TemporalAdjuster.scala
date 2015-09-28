package java.time.temporal

trait TemporalAdjuster {
  def adjustInto(temporal: Temporal): Temporal
}
