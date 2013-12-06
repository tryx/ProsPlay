package controllers

import play.api._
import play.api.mvc._
import models.AnalysisResult
import models.PatientType

object Ajax extends Controller {
  def cusType(patientType: Int) = Action {
    Ok(views.html.patientTypeList(
        (AnalysisResult.performBIAnalysis _2)(
            PatientType.values.toList(patientType))))
  }
  
  def transactionList(PatientID: Int) = Action {
    Ok(views.html.patientTransactionList(
        (AnalysisResult.dumpData).apply(PatientID), 
        PatientID
    ))
  }
}