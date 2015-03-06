package org.nlpr.cip.kb.util

import java.io.PrintWriter

import scala.io.Source


object CorpusFilter {

  val base_path = "G:\\temp\\TransX\\fb15k_filter\\"

  def main(args: Array[String]) {

    filterRelation()


  }

  def filterRelation() {
    //    val relations = Set("gender")//for fb13
    val relations = Set(
      "/location/hud_foreclosure_area/total_90_day_vacant_residential_addresses./measurement_unit/dated_integer/source",
      "/soccer/football_team/current_roster./sports/sports_team_roster/position",
      "/people/marriage_union_type/unions_of_this_type./people/marriage/spouse",
      "/location/hud_foreclosure_area/estimated_number_of_mortgages./measurement_unit/dated_integer/source",
      "/film/film/release_date_s./film/film_regional_release_date/film_release_distribution_medium",
      "/tv/tv_producer_type/tv_producers_of_this_type./tv/tv_producer_term/producer",
      "/location/hud_foreclosure_area/hhuniv./measurement_unit/dated_integer/source",
      "/location/statistical_region/rent50_0./measurement_unit/dated_money_value/currency",
      "/common/topic/webpage./common/webpage/category",
      "/soccer/football_team/current_roster./soccer/football_roster_position/position",
      "/film/film/estimated_budget./measurement_unit/dated_money_value/currency",
      "/sports/sports_team/roster./soccer/football_roster_position/position",
      "/location/hud_foreclosure_area/estimated_number_foreclosures./measurement_unit/dated_integer/source",
      "/sports/sports_position/players./soccer/football_roster_position/team",
      "/location/hud_foreclosure_area/total_residential_addresses./measurement_unit/dated_integer/source",
      "/people/person/gender",
      "/tv/tv_producer/programs_produced./tv/tv_producer_term/producer_type",
      "/location/statistical_region/rent50_1./measurement_unit/dated_money_value/currency",
      "/user/tsegaran/random/subject_taxonomy/entry./user/tsegaran/random/taxonomy_entry/subject",
      "/location/statistical_region/rent50_2./measurement_unit/dated_money_value/currency",
      "/location/statistical_region/rent50_3./measurement_unit/dated_money_value/currency",
      "/user/tsegaran/random/taxonomy_subject/entry./user/tsegaran/random/taxonomy_entry/taxonomy",
      "/people/person/spouse_s./people/marriage/type_of_union",
      "/location/statistical_region/rent50_4./measurement_unit/dated_money_value/currency",
      "/common/annotation_category/annotations./common/webpage/topic")

    for (name <- List("train", "valid", "test")) {
      val inputPath = base_path + "data/" + name + ".txt"
      val outputPath = base_path + "data/" + name + "_filter.txt"
      val out = new PrintWriter(outputPath)
      for (line <- Source.fromFile(inputPath).getLines)
        if (!relations.contains(line.split("\t")(1)))
          out.println(line)
      out.close()
    }
  }
}
