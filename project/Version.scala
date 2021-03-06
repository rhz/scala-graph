object Version {
  val compiler = "2.10.3"
  val compiler_2 = "2.11.0"
  val compilerIsRC = false

  private val isSnapshot = false
  private def snapshot = if (isSnapshot) "-SNAPSHOT" else ""
  private val major = "1.8"
  val all         = major + ".1" + snapshot
  val core        = major + ".1" + snapshot
  val constrained = major + ".1" + snapshot
  val dot         = major + ".1" + snapshot
  val json        = major + ".1" + snapshot
  val test        = major + ".1" + snapshot
  val misc        = major + ".1" + snapshot
}