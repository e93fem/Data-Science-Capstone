import java.io.PrintWriter

import play.api.libs.json.{JsValue, JsObject, JsNumber, Json}
import scala.io.Source

/**
 * Created by fredr_000 on 2015-10-13.
 */
object ConvertBusinessJson {
  def main(args: Array[String]) {
    val input = "F:\\Documents\\Data Science Capstone v2\\data\\yelp_dataset_challenge_academic_dataset\\yelp_academic_dataset_business.json"
//    val input = "F:\\Documents\\Data Science Capstone v2\\json-files\\business.json"

    var res: String = ""
    Source.fromFile(input).
      getLines().foreach
    { line =>
      {
        val json = Json.parse(line)
        val categories = (json \ "categories").as[List[String]]
        var newCategories = Json.obj()
        categories.foreach(category => {
          newCategories = newCategories + (category.toString() -> JsNumber(1))
        })

        val neighborhoods = (json \ "neighborhoods").as[List[String]]
        var newNeighborhoods = Json.obj()
        neighborhoods.foreach(neighborhood => {
          newNeighborhoods = newNeighborhoods + (neighborhood.toString() -> JsNumber(1))
        })

        val attributes = (json \\ "attributes")
        var newAttributes = Json.obj()
        attributes.foreach(f => f.as[Map[String, JsValue]].foreach(f2 => {
          f2._2 match {
            case f3: JsObject => f2._2.as[Map[String, JsValue]].foreach(f4 =>
              newAttributes = newAttributes + ((f2._1 + "__" + f4._1 + "__" + f4._2).replaceAll("\"","") -> JsNumber(1)))
            case _ => newAttributes = newAttributes + ((f2._1 + "__" + f2._2).replaceAll("\"","") -> JsNumber(1))
          }
        }))

        var json2 = json.as[JsObject] - "neighborhoods" - "categories" - "attributes" +
          ("categories" -> newCategories) + ("neighborhoods" -> newNeighborhoods) + ("attributes" -> newAttributes)

        res = res + json2 + "\n"
      }
    }

    new PrintWriter(input.replaceFirst("\\.json","\\.fix.json")) { write(res); close }
  }
}
