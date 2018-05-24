package standardscala

case class TempData(day: Int,  doy: Int, month: Int, year: Int,
                    precip: Double, snow: Double, tave:Double, tmax: Double, tmin: Double)

object TempData {

  def main(args: Array[String]): Unit = {

    val source = scala.io.Source.fromFile("MN212142_9392.csv")
    val lines = source.getLines().drop(1)
    val data = lines.filter(!_.contains(",.,")).map { line =>
      val p = line.split(",")
      TempData(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble,
        p(6).toDouble,p(7).toDouble,p(8).toDouble, p(9).toDouble)
    }.toArray

    val maxTemp = data.map(_.tmax).max
    val hotDays = data.filter(_.tmax == maxTemp)
    println(s"Hot days are ${hotDays.mkString(", ")}")

    val hotDay = data.maxBy(_.tmax)

    println(s"Hot day 1 is $hotDay")

    val hotDay2 = data.reduceLeft((d1, d2) => if (d1.tmax >= d2.tmax ) d1 else d2)
    println(s"Hot day 2 is $hotDay2")

    val rainyCount = data.count(_.precip >= 1.0)
    println(s"There are $rainyCount rainy day. there is ${rainyCount*100.0/data.length} percent")
    val monthG = data.groupBy(_.month)
    val monthlyTemp = monthG.map { case (m, days) =>

        m -> days.foldLeft(0.0)((sum, td) => sum+td.tmax)/days.length

    }
    monthlyTemp.toSeq.sortBy(_._1) foreach println
    source.close()
  }

}
