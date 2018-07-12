package pl.pecet.wordsanalyzer

import java.nio.charset.StandardCharsets

import scala.io.Source

object WordsAnalyzer extends App {
  val episodes = Map(
    "s01" -> 24, "s02" -> 24, "s03" -> 24,
    "s04" -> 24, "s05" -> 23, "s06" -> 25,
    "s07" -> 24, "s08" -> 24, "s09" -> 24,
    "s10" -> 17
  )

  print("Download subtitles from the Internet. Please wait...")
  val episodeDialogs = for ((s, n) <- episodes; e <- 1 to n) yield findEpisodeDialogs(s, e)
  println(" OK!")

  println("Start analysis. Please wait...")
  mostPopularWord(episodeDialogs, args(0).toInt) foreach println

  def findEpisodeDialogs(season: String, episode: Int) = {
    val url = f"https://www.springfieldspringfield.co.uk/view_episode_scripts.php?tv-show=friends&episode=${season}e$episode%02d"
    val html = Source.fromURL(url, StandardCharsets.UTF_8.name)
    val divPattern = """(?s)<div class="scrolling-script-container">(.*?)</div>""".r
    divPattern.findFirstMatchIn(html.mkString) match {
      case Some(m) => m.group(1).trim
      case None => "Not found."
    }
  }

  def removePunctuationAndDigitCharacters(s: String) = {
    val toRemovePattern = """<br>|[0-9]|["\,\.!\?\-]""".r
    toRemovePattern.replaceAllIn(s.toLowerCase, " ")
  }

  def mostPopularWord(lines: Iterable[String], limit: Int) = {
    val words = lines.flatMap(removePunctuationAndDigitCharacters(_).split("\\s+")).filterNot(_.isEmpty)
    val wordCounts = words.groupBy(identity).mapValues(_.size)
    wordCounts.toList.sortBy(_._2)(Ordering[Int].reverse).take(limit).map(_._1)
  }
}
