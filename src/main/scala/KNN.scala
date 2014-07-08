/**
 * Created by Mark on 7/6/2014.
 */


class KNN(val trainSet: List[dataRow], val k: Int) {

  def classify(observable: Map[String, Double]): String = {
    val distances: List[(Double, String)] = trainSet.map(train => train.getDistance(observable) -> train.label)
    var kMinDistances = getKDistancesthatMinimun(distances)
    getLabelTimes(kMinDistances).maxBy(dis=>dis._2)._1
  }

  //取得前k個最小距離
  private def getKDistancesthatMinimun(distances: List[(Double, String)]): List[(Double, String)] = {
    distances.sortBy(dis => dis._1).take(k)
  }

  //取得各label出現的次數
  private def getLabelTimes(distances: List[(Double, String)]): List[(String, Int)] = {

    val labels=distances.map(dis=>dis._2).distinct
    labels.map(label=>label->distances.count(dis=>dis._2==label))


  }
}

trait dataRow {
  val label: String
  val properties: Map[String, Double]

  def getDistance(other: Map[String, Double]) = {
    val sequares = properties.map(p => math.pow(p._2 - other(p._1), 2))
    val sumOfSeqQuare = sequares.sum

    math.sqrt(sumOfSeqQuare)
  }
}

class trainSet(dataSet: List[dataRow]) {
  if (dataSet.exists(s => s.label == null))
    throw new Exception("label is null.")


}
