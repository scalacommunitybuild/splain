package splain

trait ImplicitStats
{ self: Analyzer =>
  import global._
  import statistics._

  def withImplicitStats[A](run: () => A) = {
    val findMemberStart =
      if (settings.areStatisticsEnabled) statistics.startCounter(findMemberImpl) else null
    val subtypeStart = if (settings.areStatisticsEnabled) statistics.startCounter(subtypeImpl) else null
    val start = if (settings.areStatisticsEnabled) statistics.startTimer(implicitNanos) else null
    val result = run()
    if (settings.areStatisticsEnabled) statistics.stopTimer(implicitNanos, start)
    if (settings.areStatisticsEnabled) statistics.stopCounter(findMemberImpl, findMemberStart)
    if (settings.areStatisticsEnabled) statistics.stopCounter(subtypeImpl, subtypeStart)
    result
  }
}
