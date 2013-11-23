package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.prospection(models.AnalysisResult.performBIAnalysis))
  }
  
  def data = Action {
    Ok(views.html.data(models.AnalysisResult.dumpData))
  }

}