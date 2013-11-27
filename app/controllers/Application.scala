package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    val (results, patientsByType) = models.AnalysisResult.performBIAnalysis
    Ok(views.html.prospection(results, patientsByType))
  }
  
  def data = Action {
    Ok(views.html.data(models.AnalysisResult.dumpData, models.AnalysisResult.testClassify))
  }

}