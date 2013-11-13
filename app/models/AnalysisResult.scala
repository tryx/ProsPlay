package models
import PatientType._

object AnalysisResult {

	def performBIAnalysis(): Map[PatientType, Int] =
	{
	  Map(
	      VIOLATED -> 200,
	      VALID_NO_COMED -> 500,
	      VALID_BI_SWITCH -> 100,
	      VALID_IB_SWITCH -> 100,
	      VALID_I_TRIAL -> 50,
	      VALID_B_TRIAL -> 50
	  )
	}
}

