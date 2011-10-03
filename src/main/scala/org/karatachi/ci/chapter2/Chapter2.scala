package org.karatachi.ci.chapter2

import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import org.apache.mahout.cf.taste.impl.common.FastByIDMap
import org.apache.mahout.cf.taste.impl.model.GenericDataModel
import org.apache.mahout.cf.taste.impl.model.GenericPreference
import org.apache.mahout.cf.taste.impl.model.GenericUserPreferenceArray
import org.apache.mahout.cf.taste.impl.neighborhood.NearestNUserNeighborhood
import org.apache.mahout.cf.taste.impl.recommender.GenericItemBasedRecommender
import org.apache.mahout.cf.taste.impl.recommender.GenericUserBasedRecommender
import org.apache.mahout.cf.taste.impl.similarity.AbstractSimilarity
import org.apache.mahout.cf.taste.impl.similarity.GenericItemSimilarity
import org.apache.mahout.cf.taste.impl.similarity.PearsonCorrelationSimilarity
import org.apache.mahout.cf.taste.impl.similarity.CIBookEuclideanDistanceSimilarity
import org.apache.mahout.cf.taste.model.DataModel
import org.apache.mahout.cf.taste.model.PreferenceArray
import org.karatachi.util._
import org.slf4j.LoggerFactory

object Chapter2 extends App {
  val critics = Map(
    "Lisa Rose" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0, "Superman Returns" -> 3.5, "You, Me and Dupree" -> 2.5, "The Night Listener" -> 3.0),
    "Gene Seymour" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 3.5, "Just My Luck" -> 1.5, "Superman Returns" -> 5.0, "The Night Listener" -> 3.0, "You, Me and Dupree" -> 3.5),
    "Michael Phillips" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.0, "Superman Returns" -> 3.5, "The Night Listener" -> 4.0),
    "Claudia Puig" -> Map("Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0, "The Night Listener" -> 4.5, "Superman Returns" -> 4.0, "You, Me and Dupree" -> 2.5),
    "Mick LaSalle" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0, "Just My Luck" -> 2.0, "Superman Returns" -> 3.0, "The Night Listener" -> 3.0, "You, Me and Dupree" -> 2.0),
    "Jack Matthews" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0, "The Night Listener" -> 3.0, "Superman Returns" -> 5.0, "You, Me and Dupree" -> 3.5),
    "Toby" -> Map("Snakes on a Plane" -> 4.5, "You, Me and Dupree" -> 1.0, "Superman Returns" -> 4.0))

  // 評価対象データ
  type Prefs = Map[String, Map[String, Double]]

  val users = critics.keys.toSet.zipWithIndex.toMap
  val usersRev = users.map(_.swap)
  val items = critics.values.flatMap(_.keys).toSet.zipWithIndex.toMap
  val itemsRev = items.map(_.swap)

  def toUser(id: Long) = usersRev(id.toInt)
  def toItem(id: Long) = itemsRev(id.toInt)

  def toUserbaseModel(critics: Prefs): DataModel = {
    val userData = new FastByIDMap[PreferenceArray]()
    critics.foreach {
      case (user, prefs) =>
        val userId = users(user)
        val data = prefs.map { case (item, pref) => new GenericPreference(userId, items(item), pref.toFloat) }
        userData.put(userId, new GenericUserPreferenceArray(data.toList))
    }
    new GenericDataModel(userData)
  }

  def toItembaseModel(critics: Prefs): DataModel = {
    val itemData = new FastByIDMap[PreferenceArray]()
    transformPrefs(critics).foreach {
      case (item, prefs) =>
        val itemId = items(item)
        val data = prefs.map { case (user, pref) => new GenericPreference(itemId, users(user), pref.toFloat) }
        itemData.put(itemId, new GenericUserPreferenceArray(data.toList))
    }
    new GenericDataModel(itemData)
  }

  def transformPrefs(prefs: Prefs): Prefs = {
    val result = Map[String, Map[String, Double]]()
    for (person <- prefs.keys; item <- prefs(person).keys) {
      result.getOrElseUpdate(item, Map[String, Double]())
      // itemとpersonを入れ替える
      result(item)(person) = prefs(person)(item)
    }
    result
  }

  val model = toUserbaseModel(critics)

  section("2.3.1 ユークリッド距離によるスコア") {
    subsection("Lisa RoseとGene Seymourのユークリッド距離") {
      val sim = new CIBookEuclideanDistanceSimilarity(model)
      output(sim.userSimilarity(users("Lisa Rose"), users("Gene Seymour")))
    }
  }

  section("2.3.2 ピアソン相関によるスコア") {
    subsection("Lisa RoseとGene Seymourのピアソン相関") {
      val sim = new PearsonCorrelationSimilarity(model)
      output(sim.userSimilarity(users("Lisa Rose"), users("Gene Seymour")))
    }
  }

  section("2.3.4 評者をランキングする") {
    subsection("Tobyに似た評者を探す") {
      val sim = new PearsonCorrelationSimilarity(model)
      val neighborhood = new NearestNUserNeighborhood(3, sim, model)
      neighborhood.getUserNeighborhood(users("Toby")).map(toUser).foreach(output)
    }
  }

  section("2.4 アイテムを推薦する") {
    subsection("ピアソン相関でToby用の商品を推薦") {
      val sim = new PearsonCorrelationSimilarity(model)
      val neighborhood = new NearestNUserNeighborhood(10, sim, model)
      val recommender = new GenericUserBasedRecommender(model, neighborhood, sim)
      recommender.recommend(users("Toby"), 10).map { r => (toItem(r.getItemID), r.getValue) }.foreach(output)
    }

    subsection("ユークリッド距離でToby用の商品を推薦") {
      val sim = new CIBookEuclideanDistanceSimilarity(model)
      val neighborhood = new NearestNUserNeighborhood(10, sim, model)
      val recommender = new GenericUserBasedRecommender(model, neighborhood, sim)
      recommender.recommend(users("Toby"), 10).map { r => (toItem(r.getItemID), r.getValue) }.foreach(output)
    }
  }

  section("2.5 似ている商品") {
    val model = toItembaseModel(critics)
    subsection("Superman Returnsに似ている商品を探す") {
      val sim = new PearsonCorrelationSimilarity(model)
      val neighborhood = new NearestNUserNeighborhood(5, sim, model)
      neighborhood.getUserNeighborhood(items("Superman Returns")).map(toItem).foreach(output)
    }
    subsection("Just My Luckを見ていない評者の中で高い評価をつけそうな人を予測する") {
      val sim = new PearsonCorrelationSimilarity(model)
      val neighborhood = new NearestNUserNeighborhood(10, sim, model)
      val recommender = new GenericUserBasedRecommender(model, neighborhood, sim)
      recommender.recommend(items("Just My Luck"), 10).map { r => (toUser(r.getItemID), r.getValue) }.foreach(output)
    }
  }

  section("2.7.2 推薦を行う") {
    subsection("アイテムベースの表からToby向け推薦を行う") {
      val sim = new GenericItemSimilarity(new CIBookEuclideanDistanceSimilarity(model), model)
      val recommender = new GenericItemBasedRecommender(model, sim)
      recommender.recommend(users("Toby"), 10).map { r => (toItem(r.getItemID), r.getValue) }.foreach(output)
    }
  }
}
